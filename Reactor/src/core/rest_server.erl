%%  Copyright (C) 2008 Alan Wood
%%  This file is part of Reactored

%%     Reactored is free software: you can redistribute it and/or modify
%%     it under the terms of the GNU General Public License as published by
%%     the Free Software Foundation, either version 2 of the License, or
%%     (at your option) any later version.

%%     Reactored is distributed in the hope that it will be useful,
%%     but WITHOUT ANY WARRANTY; without even the implied warranty of
%%     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%     GNU General Public License for more details.

%%     You should have received a copy of the GNU General Public License
%%     along with Reactored.  If not, see <http://www.gnu.org/licenses/>.

%%     Further information about Reactored and it's ideas can be found at
%%     http://www.Reactored.org/

%%%-------------------------------------------------------------------
%%% File    : rest_server.erl
%%% Author  : Alan Wood <awood@alan-woods-macbook.local>
%%% Description : 
%%%
%%% Created : 19 Jun 2008 by Alan Wood <awood@alan-woods-macbook.local>
%%%-------------------------------------------------------------------
-module(rest_server).
-author('awood@alan-woods-macbook.local').
-include("schema.hrl").
-include("system.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([start/1, stop/0, react_to/2]).
-export([indexes/0]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:react_to(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

%% Reactor main http dispatch
react_to(Request, DocRoot) ->
    react_to(Request:get(method),Request:get(path),Request, DocRoot).

%% Homes
%% react_to(Method,"/",Request,DocRoot) ->
%%     react_to(Method,"/static/index.html",Request, DocRoot);
%% react_to(Method,"/index.html",Request,DocRoot) ->
%%     react_to(Method,"/static/index.html",Request, DocRoot);
%% react_to(Method,"/index.htm",Request,DocRoot) ->
%%     react_to(Method,"/static/index.html",Request, DocRoot);
%% react_to(Method,"/home.html",Request,DocRoot) ->
%%     react_to(Method,"/static/index.html",Request, DocRoot);
%% react_to(Method,"/home.htm",Request,DocRoot) ->
%%     react_to(Method,"/static/index.html",Request, DocRoot);
%% react_to(Method,"/_/error",Request,DocRoot) ->
%%     redirect(retrieve,"/static/error.html",Request,[]);

%% Special cases for actor based interactions
% Tag resource (or external url) using 'GET'

% Special functions - short cut to actor's identity page
react_to(Method,"/home" ++ Resource,Request,_DocRoot) ->
    react_to(Method,"/~" ++ Resource,Request,_DocRoot);
react_to(Method,"/~",Request,_DocRoot) ->
    io:format("Home ~s~n",["Where the heart is"]),
    case credentials(Request) of
	{uri,Actor} ->
	    Url = item_to_url(Actor),
	    io:format("Redirecting to ~s~n",[Url]),
	    redirect(retrieve,"http://" ++ Url,Request,[]);
	_ -> 
	    redirect(retrieve,domain(Request) ++ "_login",Request,[])
    end;

react_to(Method,"/~" ++ Resource,Request,_DocRoot) ->
    react(actor,adaptor(lists:reverse(Resource),accepts(Request)),Method,"~"++ Resource,Request);

%% Reactor REST dispatcher
react_to(Method,?CONTEXT ++ Resource,Request,_DocRoot) ->
    react_to(Method,Resource,Request);

%% Reactor Resource dsipatcher
react_to(Method,"/" ++ Path,Req, DocRoot) ->
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
                _ ->
                    Req:serve_file(Path, DocRoot)
            end;
        'POST' ->
            case Path of
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

%% Basic REST Operations
react_to('HEAD',Url,Request) -> %% Todo optimise HEAD calls
    case split(Url) of
	{Resource,Ext} ->
	    react(adaptor(Ext,accepts(Request)),retrieve,Resource,Request);
	_ ->
	    error(html_adaptor,'HEAD',Url,Request,"Illegal resource request")
    end;
react_to('GET',Url,Request) -> 
    case split(Url) of
	{Resource,Ext} ->
	    react(adaptor(Ext,accepts(Request)),retrieve,Resource,Request);
	_ ->
	    error(html_adaptor,'HEAD',Url,Request,"Illegal resource request")
    end;
react_to('POST',Url,Request) ->
    case split(Url) of
	{Resource,Ext} ->
	    react(adaptor(Ext,accepts(Request)),create,Resource,Request);
	_ ->
	    error(html_adaptor,'HEAD',Url,Request,"Illegal resource request")
    end;
react_to('PUT',Url,Request) ->
    case split(Url) of
	{Resource,Ext} ->
	    react(adaptor(Ext,accepts(Request)),update,Resource,Request);
	_ ->
	    error(html_adaptor,'HEAD',Url,Request,"Illegal resource request")
    end;
react_to('DELETE',Url,Request) ->
    case split(Url) of
	{Resource,Ext} ->
	    react(adaptor(Ext,accepts(Request)),delete,Resource,Request);
	_ ->
	    error(html_adaptor,'HEAD',Url,Request,"Illegal resource request")
    end;

%% Catch all, error default
react_to(Method,Resource,Request) ->
    Request:respond({501, [], atom_to_list(Method) ++ " not supported for " ++ Resource}).

%% Special actor based API
    
% List actor's tags only
react(actor,Adaptor,retrieve,"~/tag/",Request) ->
    Attributes = attributes('GET',Request), 
    case credentials(Request) of
	{uri,Actor} ->
	    tag(Adaptor,Actor,Request,Attributes);    
	_ -> 
	    error(Adaptor,retrieve,"~/tag/",Request,"Current access privelages do not allow tagging")
    end;

% Tag resource (or external url) using 'GET', copy to CC
react(actor,Adaptor,retrieve,"~/tag/" ++ CC,Request) ->
    Attributes = attributes('GET',Request), 
    case credentials(Request) of
	{uri,Actor} ->
	    case actor_server:update({uri,Actor},?MODULE,CC,Attributes) of
		{ok,_Xref} -> 
		    tag(Adaptor,Actor,Request,Attributes);
		{error,Error} ->
		    error(Adaptor,retrieve,"~/tag/" ++ CC,Request,"Issues copying tag request to destination resource");
		{autherror,Why} ->
		    error(Adaptor,retrieve,"~/tag/" ++ CC,Request,"Current access privelages do not allow tagging and copying")
	    end;
	_ -> 
	    error(Adaptor,retrieve,"~/tag/" ++ CC,Request,"Current access privelages do not allow tagging")
    end;

% Just items participant tagged with tag/tags
react(actor,Adaptor,retrieve,"~/tags/" ++ Tags,Request) ->
    case credentials(Request) of
	{uri,Actor} ->
	    Domain = domain(Request),
	    Url = Actor ++ "/tags/",
	    case actor_server:tagged(credentials(Request),?MODULE,Domain,{string:tokens(Tags,"/"),Actor}) of
		{error, Error} -> 
		    error(Adaptor,retrieve,Url,Request,Error);
		{autherror,Why} ->
		    forbidden(Url,Request,Why);
		Items -> 
		    respond(Request,Adaptor:render("Your Tags",Url,Items))
	    end;
	_ -> 
	    error(Adaptor,retrieve,"~/tags/" ++ Tags,Request,"Could not retrieve tags, credentials may not be correct, are you logged in?")
    end.

%% Internal API REST interfaces

%% Domain REST interface
react(Adaptor,retrieve,"_/",Request) ->
    Res = qres("_/",Request),
    case actor_server:domain(credentials(Request),?MODULE,actor_server:lookup(Res)) of 
	{ok,Domains} ->  
	    Props = [{"title","Domains"},{"uri",Res},{"description","Domain Listing"},{"author",domain(Request) ++ ?IDENTITIES ++ "/founder"}],
	    respond(Request,Adaptor:render("Domains",Res,Props ++ Domains));
	{error,Error} ->
	    error(Adaptor,retrieve,"_/",Request,"Could not retrieve domain list " ++ Error);
	{autherror,Why} ->
	    forbidden("_/",Request,Why)
    end;

react(Adaptor,create,"_/",Request) ->
    Attribs = attributes('POST',Request),
    Matcher = case proplists:get_value("matcher", Attribs) of
		undefined -> 
		    ?ERRORMATCHER;
		M -> M
	    end,
    case proplists:get_value("domain", Attribs) of
	undefined -> 
	    error(Adaptor,retrieve,"_/",Request,"Domain not provided, could not create domain ");
	Domain ->   
	    case actor_server:domain(credentials(Request),?MODULE,?CONTEXT ++ Domain,qres("_/",Request),Matcher) of
		{ok,_Domains} -> 
		    redirect(create,"_/",Request,[]);
		{error,Error} ->
		    error(Adaptor,retrieve,"_/",Request,"Could not create " ++ Error);
		{autherror,Why} ->
		    forbidden("_/",Request,Why)
	    end
    end;



%% Echo tests
react(_Adaptor,Operation,"_/echo/" ++ Resource,Request) ->
    %% these echo responses could be extended if we moved the service detector up higher and call react with the service as a param rather than using Module nside react calls, we would have to use a header or something instead of the _/echo/ url, In fact Service could also indicate API vs None API calls
    Response = case Operation of
		   retrieve -> 
		       actor_server:retrieve(credentials(Request),echo,qres(Resource,Request));
		   create -> 
		       case lists:last(Resource) of
			   $/ -> 
			       Domain = domain(Request),
			       Attributes = attributes('POST',Request),
			       Item = Resource ++ attribute:today() ++ "_" ++ title(Attributes),
			       actor_server:create(credentials(Request),echo,Domain,Item,Attributes);
			   _ -> 
			       actor_server:update(credentials(Request),echo,qres(Resource,Request), attributes('POST',Request))
		       end;
		   update -> 
		       actor_server:update(credentials(Request),echo,qres(Resource,Request), attributes('POST',Request));
		   delete -> 
		       case attributes("DELETE",Request) of
			   [] -> actor_server:delete(credentials(Request),echo,qres(Resource,Request));
			   Attributes -> actor_server:delete(credentials(Request),echo,qres(Resource,Request),Attributes)
		       end
	       end,   
    Request:respond({200, [], Response});

%% Logging into REST interface
%% Todo implement plain http login as an alternative to form based login
react(_Adaptor,retrieve,"_/login",Request) ->
    io:format("Cookie ~p~n",[Request:get_cookie_value(?COOKIE)]),
    show_login_form(Request); % TODO we might just forward to /static/login.html instead, same below on login errors
react(Adaptor,create,"_/login",Request) ->
    io:format("~n Authenticating ~n"),
    %% Todo need to handle cases where login params are no provided
    Attribs = attributes('POST',Request),
    Id = proplists:get_value("identity", Attribs),
    Pswd = proplists:get_value("password", Attribs),
    case identity_server:authenticate(Id,Pswd) of
	{ok,Actor} -> 
	    io:format("Authenticated ~s~n",[Actor]), 
	    Header = save_session(Request,Actor),
	    io:format("Headers ~p~n",[Header]),
	    %redirect(create,Actor,Request,[Header]);
	    Request:respond({200, [{"Content-Type", "text/html"} | [Header]], html("<h3>Logged in</h3>")});
	{error,Error} ->
	    io:format("Not authenticated ~s~n",[Error]),
	    show_login_form(Request,Error)
    end;

react(_Adaptor,retrieve,"_/logout" ++ _,Request) ->
    remove_session(Request),
    {uri,Actor} = credentials(Request),
    redirect(create,Actor,"_/login",[]);

%% Tagging REST interface

% Tag resource (or external url) using 'Post' under identities ID tags
% This is like tag posting, tagging to a resource's tag fork or to a identity tag fork
react(Adaptor,create,"_/tag/" ++ Dest,Request) ->
    Attributes = attributes('POST',Request), 
    % Todo fixes dstination as a provided actor (dest) this could be opened to be less specific as in bookmarking sending a tagged hyperlink to a given resource.
    tag(Adaptor,qres(?IDENTITIES ++ ?DOMAINSEPERATOR ++ "/" ++ Dest ++ "/tags",Request),Request,Attributes);

% All Items tagged with tag/tags that participant has retrieve acl for
react(Adaptor,retrieve,"_/tags/" ++ Tagz,Request) ->
    Domain = domain(Request),
    Url = Domain ++ "_/tags/"  ++ Tagz,
    Tags = string:tokens(Tagz,"/"),
    Tagged = lists:flatten(["Tagged : "|Tags]),
    case actor_server:tagged(credentials(Request),?MODULE,Domain,Tags) of
	{error, Error} -> 
	    error(Adaptor,retrieve,"_/tags/" ++ Tagz,Request,Error);
	{autherror,Why} ->
	    forbidden("_/tags/" ++ Tagz,Request,Why);
	Items -> 
	    respond(Request,Adaptor:render(Tagged,Url,Items))
    end;

react(Adaptor,create,"_/search/" ++ S,Request) ->
    Attribs = attributes("POST",Request),
    Text = case proplists:get_value("search", Attribs) of
	       undefined -> "";
	       Val -> Val
	   end,
    Domain = domain(Request),
    Url = Domain ++ "_/search/"  ++ S,
    Title = "Search Results for " ++ Text,
    case actor_server:search(credentials(Request),?MODULE,Domain,Text) of
	{error, Error} -> 
	    error(Adaptor,retrieve,"_/search/" ++ S,Request,Error);
	{autherror,Why} ->
	    forbidden("_/search/" ++ S,Request,Why);
	Items -> 
	    respond(Request,Adaptor:render(Title,Url,lists:map(fun({T,I}) -> I end,Items)))
    end;

react(Adaptor,retrieve,"_/search/" ++ Tokens,Request) ->
    Search = string:tokens(Tokens,"/"),
    Text = string:join(Search," "),
    Domain = domain(Request),
    Url = Domain ++ "/_/search/"  ++ Tokens,
    Title = lists:flatten(["Search Results for "|Text]),
    case actor_server:search(credentials(Request),?MODULE,Domain,Text) of
	{error, Error} -> 
	    error(Adaptor,retrieve,"_/search/" ++ Tokens,Request,Error);
	{autherror,Why} ->
	    forbidden("_/search/" ++ Tokens,Request,Why);
	Items -> 
	    respond(Request,Adaptor:render(Title,Url,lists:map(fun({T,I}) -> I end,Items)))
    end;

%% Identity REST reactors    

%% I think this is now redundant and I'm not sure if the profile call is relevant
%% react(Adaptor,retrieve,"_/id/" ++ Id,Request) ->
%%     Domain = domain(Request),
%%     Url = Domain ++ "/_/id/"  ++ Id,
%%     Title = "Profile of " ++ Id,
%%     case actor_server:profile(credentials(Request),?MODULE,Domain,Id) of
%% 	{error, Error} -> error(Adaptor,retrieve,"_/id/" ++ Id,Request,Error);
%% 	Items -> respond(Request,Adaptor:render(Title,Url,Items))
%%     end;

react(Adaptor,create,"_/id/",Request) ->
    Domain = domain(Request),
    Attributes = attributes('POST',Request),
    Item = "_/id/" ++ attribute:today() ++ "_" ++ title(Attributes),
    case actor_server:new(credentials(Request),?MODULE,Domain,Item,Attributes) of
	{ok,_Xref} -> 
	    redirect(create,qres(Item,Request),Request,[]);
	{error,Error} -> 
	    error(Adaptor,retrieve,"_/id/",Request,Error);
	{autherror,Why} ->
	    forbidden("_/id/",Request,Why)
    end;

react(Adaptor,delete,"_/id/" ++ Id,Request) ->
    case actor_server:remove(credentials(Request),?MODULE,qres("_/id/" ++ Id,Request)) of
	{ok,_Xref} -> 
	    redirect(create,qres("_/id/",Request),Request,[]);
	{error,Error} -> 
	    error(Adaptor,delete,"_/id/" ++ Id,Request,Error);
	{autherror,Why} ->
	    forbidden("_/id/" ++ Id,Request,Why)
    end;

%% ACL REST reactors
% Todo much of this could be redundant and instead be placed under /identity/id/acl/ namespace rather than its own, particularly is an acl is created for every user by default. The trick is the pattern matching to recognise the URL!
react(Adaptor,create,"_/acl/",Request) ->
    error(Adaptor,create,"_/acl/",Request,"Identity ACL can mot be created remotely, it is an internal function");
react(Adaptor,retrieve,"_/acl/",Request) ->
    %forbidden(Resource,Request,Why)
    error(Adaptor,retrieve,"_/acl/",Request,"Identity ACLs can not be listed remotely, it is an internal function only");

react(Adaptor,retrieve,"_/acl/" ++ Id,Request) ->
    react(Adaptor,retrieve,"_/id/" ++ Id ++ "/acl",Request);

react(Adaptor,create,"_/acl/" ++ Id,Request) ->
    react(Adaptor,update,"_/acl/" ++ Id,Request);

react(Adaptor,update,"_/acl/" ++ Id,Request) ->
    % contains resource (Qitem, or use referer) being ACL'ed + controls
    Attributes = attributes('POST',Request), 
    {Resource,Attrib} = case get_option("resource",Attributes) of
		   {undefined,Attr} -> 
		       {referer(Request),Attr};
		   {Url,Attr} -> 
		       {Url,Attr}
	       end,
    Acl = proplists:get_value("acl",Attrib),
    case {Resource,Acl} of 
	{undefined,_} -> 
	    error(Adaptor,update,"_/acl/" ++ Id,Request,"Resource not provided for ACL Update");
	{_,undefined} -> 
	    error(Adaptor,update,"_/acl/" ++ Id,Request,"ACL not provided for ACL Update");
	{_,_} ->
	    Attribs = [{Resource,Acl}|Attrib],
	    case get_option("control",Attribs) of
		{"grant",Atts} -> 
		    case actor_server:grant(credentials(Request),?MODULE,qres("_/id/" ++ Id,Request),Resource,Atts) of
			{ok,Iacl} ->
			    actor_server:update(credentials(Request),?MODULE,qres("_/id/" ++ Id ++ "/acl",Request),repack_acl(Resource,Iacl,Atts)),
			    redirect(update,qres("_/acl/" ++ Id,Request),Request,[]);
			{error,Error} -> 
			    error(Adaptor,update,"_/acl/" ++ Id,Request,Error);
			{autherror,Why} ->
			    forbidden(Resource,Request,Why)
		    end;
		{"revoke",Atts} ->
		    case actor_server:revoke(credentials(Request),?MODULE,qres("_/id/" ++ Id,Request),Resource,Atts) of
			{ok,Iacl} -> 
			    actor_server:update(credentials(Request),?MODULE,qres("_/id/" ++ Id ++ "/acl",Request),repack_acl(Resource,Iacl,Atts)),
			    redirect(update,qres("_/acl/" ++ Id,Request),Request,[]);
			{error,Error} -> 
			    error(Adaptor,update,"_/acl/" ++ Id,Request,Error);
			{autherror,Why} ->
			    forbidden(Resource,Request,Why)
		    end;
		{undefined,Atts} -> 
		    error(Adaptor,update,"_/acl/" ++ Id,Request,"ACL control not supplied ");
		{Control,Atts} -> 
		    error(Adaptor,update,"_/acl/" ++ Id,Request,"ACL " ++ Control ++ " not supported")
	    end
    end;


%% General Namespace REST Reactors
%% Todo - graph(Credentials,Service,Domain, Uri, Attributes)

react(Adaptor,retrieve,Resource,Request) ->
    %% Todo this is an untidy hack! the credentials call should remove token attribs, and return a tuple like {Credentials,Attributes}, this should be called at a higher level e.g allowing react(adaptor(Ext,accepts(Request)),retrieve,Resource,{Credentials,Attributes},Request);
    Credentials = credentials(Request),
    {_Token,Attributes} = get_option("token",attributes('GET',Request)),
    %io:format("Retrieving resource ~s~n",[Resource]),
    case lists:last(Resource) of
	$/ -> react(Adaptor,list,Resource,Request);
	_ ->
	    Domain = domain(Request),
	    Url = Domain ++ Resource,
	    Title = "Resource " ++ Resource,
	    case actor_server:lookup(qres(Resource,Request)) of
		[] ->
		    error(Adaptor,retrieve,Resource,Request,"Could not identify resource " ++ Resource);
		Qitem ->
		    case Attributes of
			[] -> 
						% Retrieve entire item, all attributes
			    case actor_server:retrieve(Credentials,?MODULE,Qitem) of
				{error,Error} -> 
				    error(Adaptor,retrieve,Resource,Request,Error);
				{autherror,Why} ->
				    forbidden(Resource,Request,Why);
				{ok,Attrib} ->
				    Response = Adaptor:render(Title,Url,Attrib),
				    respond(Request,Response)
			    end;
			Attrib -> 
						% partial retrieve specified attributes only
			    case actor_server:retrieve(Credentials,?MODULE,Domain,Resource,Attrib) of
				{error,Error} -> 
				    error(Adaptor,retrieve,Resource,Request,Error);
				{autherror,Why} ->
				    forbidden(Resource,Request,Why);
				Attrib -> 
				    respond(Request,Adaptor:render(Title,Url,Attrib))
			    end
		    end
	    end
    end;

react(Adaptor,list,Resource,Request) ->
    %% Todo Can we add in graph queries here as well a domain queries, and distinguish between them? Basic requirement is ability to differentiate between domain and non domian queries via a is_domain(Resource).
    Credentials = credentials(Request),
    {_Token,Attributes} = get_option("token",attributes('GET',Request)),
    [_|Niamod] = lists:reverse(qres(Resource,Request)),
    Domain = lists:reverse(Niamod),
    case actor_server:q(Credentials,?MODULE,Domain, attributes('GET',Request)) of
	{error,Error} -> 
	    error(Adaptor,list,Resource,Request,Error);
	{autherror,Why} ->
	    forbidden(Resource,Request,Why);
	Items ->
	    respond(Request,Adaptor:render("Domain Query " ++ Domain,Resource,Items))
    end;


react(Adaptor,create,Resource,Request) ->
    case lists:last(Resource) of
	$/ ->
	    Credentials = case credentials(Request) of
			      {annonymous} ->
				  {annonymous};
			      {token,Token} ->
				  {uri,identity_server:actor_from_token(Token)};
			      {uri,Actor} ->
				  {uri,Actor}
			  end,
	    case actor_server:lookup(qres(Resource,Request)) of
		[] ->
		    error(Adaptor,create,Resource,Request,"Cannot create resource as child of unknown resource/domain " ++ Resource);
		Qitem ->
		    [Domain,Res] = string:tokens(Qitem,?DOMAINSEPERATOR),
		    {Tags,A1} = get_option("tags",attributes('POST',Request)),
		    {_Token,A2} = get_option("token",A1),
		    {Redirect,A3} = get_option("redirect",A2),
		    {Item,Attributes} = case get_option("itemid",A3) of
					    {undefined,A4} ->
						{Res ++ attribute:today() ++ "_" ++ title(A4),A4};
					    {Id,A4} ->
						{Res ++ Id,A4}
					end,
		    case actor_server:create(Credentials,?MODULE,Domain,Item,Attributes) of
			{ok,_Xref} -> 
			    case Tags of
				undefined ->
				    case Redirect of
					undefined ->
					    react(Adaptor,retrieve,Resource,Request);
					_ -> 
					    redirect(create,Redirect,Request,[])
				    end;
				Tags ->
				    {uri,A} = Credentials,
				    Dest = A ++ "/tags",
				    Tagz = string:tokens(Tags," "),
				    case identity_server:tag(A,Domain ++ ?DOMAINSEPERATOR ++ Item,Tagz) of
					{ok,Iacl} ->
					    actor_server:update(Credentials,?MODULE,Dest,[{Item,Tags}|Attributes]),
					    case Redirect of
						undefined ->
						    react(Adaptor,retrieve,Resource,Request);
						_ ->
						    redirect(create,Redirect,Request,[])
					    end;
					{error,Error} -> 
					    error(Adaptor,update,"_/tag/" ++ Dest,Request,Error);
					{autherror,Why} ->
					    forbidden("_/tag/" ++ Dest,Request,Why)
				    end
			    end;
			{error,Error} -> 
			    error(Adaptor,create,Resource,Request,Error);
			{autherror,Why} ->
			    forbidden(Resource,Request,Why)
		    end
	    end;
	_ -> 
	    %% cannot create unless ends in "/" must be an update (POST faking PUT)
	    react(Adaptor,update,Resource,Request)
    end;

react(Adaptor,update,Resource,Request) ->
    case actor_server:update(credentials(Request),?MODULE,qres(Resource,Request), attributes('POST',Request)) of
	{ok,_Xref} -> 
	    react(Adaptor,retrieve,Resource,Request);
	{error,Error} -> 
	    error(Adaptor,retrieve,Resource,Request,Error);
	{autherror,Why} ->
	    forbidden(Resource,Request,Why)
    end;

react(Adaptor,delete,Resource,Request) ->
    case actor_server:lookup(qres(Resource,Request)) of
	[] ->
	    error(Adaptor,retrieve,Resource,Request,"Could not identify resource to be deleted " ++ Resource);
	Qitem ->
	    Attributes = attributes("DELETE",Request),
	    case Attributes of
		[] -> 
		    case actor_server:delete(credentials(Request),?MODULE,Qitem) of
			%% Todo Should we be forwarding to parent on a delte item? what does an http DELET expect in reply, Also going up to parent may cock up resousource linking like styles/images as browsers url will now be incorrect?
			{ok,_Xref} -> 
			    redirect(delete,attribute:parent(domain(Request),Resource),Request,[]);
			{error,Error} -> 
			    error(Adaptor,retrieve,Resource,Request,Error);
			{autherror,Why} ->
			    forbidden(Resource,Request,Why)
		    end;
		_ -> 
		    case actor_server:delete(credentials(Request),?MODULE,qres(Resource,Request),Attributes) of
			{ok,_Xref} -> 
			    react(Adaptor,retrieve,Resource,Request);
			{error,Error} -> 
			    error(Adaptor,retrieve,Resource,Request,Error);
			{autherror,Why} ->
			    forbidden(Resource,Request,Why)
		    end
	    end
    end;

react(_Adaptor,Operation,Resource,Request) ->
    io:format("Resource not found ~s ~s~n",[atom_to_list(Operation),Resource]),
    Request:not_found().
    %Request:respond({501, [{"Content-Type", "plain/text"}], "Unrecognised request for " ++ ?CONTEXT ++ Resource}).

respond(Request,{ContentType,Body}) ->
    Headers = [],
    Request:respond({200, [{"Content-Type", ContentType} | Headers], Body}).

error(_Adaptor,_Operation,Resource,Request,Error) ->
    Request:respond({500, [], ?CONTEXT ++  Resource ++ "\n" ++ Error ++ "\n"}).

%% Redirect for any type (in this case only retrieves/'GET'
redirect(retrieve,"http://" ++ Resource,Request,Headers) ->
    Headers2 = [{"Location","http://" ++ Resource}|Headers],
    Request:respond({303, [{"Content-Type", "text/html"} | Headers2], a(?CONTEXT ++ Resource)});
%% Redirect for any type (in this case only retrieves/'GET'
redirect(retrieve,Resource,Request,Headers) ->
    Headers2 = [{"Location",?CONTEXT ++ Resource}|Headers],
    Request:respond({303, [{"Content-Type", "text/html"} | Headers2], a(?CONTEXT ++ Resource)});
%% None get based redirect, ask browser to 'GET' on Resource
redirect(Operation,"http://" ++ Resource,Request,Headers) when Operation /= retrieve ->
    Headers2 = [{"Location","http://" ++ Resource}|Headers],
    Request:respond({303, [{"Content-Type", "text/html"} | Headers2], a(?CONTEXT ++ Resource)});
redirect(Operation,Resource,Request,Headers) when Operation /= retrieve ->
    Headers2 = [{"Location",?CONTEXT ++ Resource}|Headers],
    Request:respond({303, [{"Content-Type", "text/html"} | Headers2], a(?CONTEXT ++ Resource)}).

%% Todo we need to use this on auth failures, This isnt being fully employed yet as plain http auth hasn't been catered for outside of form based login.
unauthorised(Resource,Request) ->
    % Todo Need to add the WWW-Authentication header field
    Headers = [],
    Request:respond({401, [{"Content-Type", "plain/text"} | Headers], ?CONTEXT ++ Resource}).

forbidden(Resource,Request,Why) ->
    Headers = [],
    Reason = lists:flatten("Resource :" ++ ?CONTEXT ++ Resource ++ ", Forbidden, Reason : " ++ Why ++ "\n"),
    Request:respond({403, [{"Content-Type", "html/text"} | Headers], Reason}).

adaptor("atom" ++ _, _Accept) ->
    atom_adaptor;
adaptor("js" ++ _, _Accept) ->
    json_adaptor;
adaptor("html" ++ _, _Accept) ->
    html_adaptor;
adaptor("xhtml" ++ _, _Accept) ->
    html_adaptor;
adaptor("csv" ++ _, _Accept) ->
    csv_adaptor;
adaptor(_, "application/atom+xml" ++ _) ->
    atom_adaptor;
adaptor(_, "application/xml" ++ _) ->
    atom_adaptor;
adaptor(_, "application/json" ++ _) ->
    json_adaptor;
adaptor(_, "application/xhtml+xml" ++ _) ->
    html_adaptor;
adaptor(_, "text/html" ++ _) ->
    html_adaptor;
adaptor(_, "text/plain" ++ _) ->
    csv_adaptor;
adaptor(_, _Accept) ->
    html_adaptor.


get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

title(Attributes) ->
    case proplists:get_value("title", Attributes) of
	undefined ->
	    [];
	Title -> safeUri(Title)
    end.

safeUri(Title) -> 
	{_,S,_} = regexp:gsub(Title,"[^a-zA-Z0-9\-_]+","_"),
	S.

accepts(Req) ->
    Req:get_header_value("Accept").

referer(Req) ->
    case Req:get_header_value("Referer") of
	undefined ->
	    [];
	"http://" ++ Ref ->
	    Ref;
	Ref -> 
	    Ref
    end.

credentials(Request) ->
    case Request:get_cookie_value(?COOKIE) of
	undefined -> 
	    case Request:get_header_value("security-token") of
		undefined ->
		    Attributes = attributes(Request:get(method),Request),
		    case proplists:get_value("token",Attributes) of
			undefined ->
			    {annonymous};
			UrlToken ->
			    io:format("Token ~s~n",[UrlToken]),
			    {token,UrlToken}
		    end;
		Token ->
		    {token,Token}
	    end;
	UserId -> 
	    get_credentials(UserId)
    end.

cookie(Cookie,UserId,Options) ->
    mochiweb_cookies:cookie(Cookie,UserId,Options).

cookie_options(Request,Uri) ->
    Domainr = string:tokens(domain(Request),"/"),
    case string:tokens(hd(Domainr),":") of
	[Domain,Port] ->
	    [{max_age, ?MAXAGE},
	     {local_time, {date(), time()}},
	     {domain, Domain},
	     {path, Uri},
	     {port,8000},
	     {secure, false}];
	[Domain] ->
	    [{max_age, ?MAXAGE},
	     {local_time, {date(), time()}},
	     {domain, Domain},
	     {path, Uri},
	     {secure, false}]
   end.
    

save_session(Request,Actor) ->
    case add_credentials(Actor) of
	{ok,{Actor,UserId}} -> 
	    cookie(?COOKIE,UserId,cookie_options(Request,"/"));
	{error,_Error} -> 
	    []
    end.

remove_session(Request) ->
    case Request:get_cookie_value(?COOKIE) of
	undefined -> 
	    void;
	UserId -> 
	    remove_credentials(UserId)
    end.

get_credentials(UserId) ->
    F = fun() ->
		mnesia:read({usession,UserId})
	end,
    case q(F) of
	{ok,Val} ->
	    Session = hd(Val),
	    {uri,Session#usession.actor};
	{error,_Error} ->
	    {annonymous}
    end.

add_credentials(Actor) ->
    F = fun() ->
		case qlc:e(qlc:q([X || X <- mnesia:table(usession),
				       X#usession.actor =:= Actor])) of
		    [] ->
			UserId = attribute:today(),
			mnesia:write({usession,UserId,Actor,attribute:ts()}),
			{Actor,UserId};
		    Sssns -> 
			S = hd(Sssns),
			mnesia:write(S#usession{ts=attribute:ts()}),
			{S#usession.actor,S#usession.userid}
		end
	end,
    case q(F) of
	{ok,ActorId} ->
	    {ok,ActorId};
	{error,Error} ->
	    {error,Error}
    end.

remove_credentials(UserId) ->
    F = fun() ->
		mnesia:delete({usession,UserId})
	end,
    q(F).

indexes() ->
    F = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(usession)]))
	end,
    mnesia:transaction(F).

qres(Resource,Req) ->
    ?DOMAIN ++ ?CONTEXT ++ Resource.
%% should be able to build this from req:get(raw_path)

domain(Request) ->
    ?DOMAIN ++ ?CONTEXT.

attributes('POST',Request) ->
    Request:parse_post();
attributes(_,Request) ->
    Request:parse_qs().

%% not effective need to consider alternatives
safe_char_set(Attribs) ->
    case proplists:get_value("type",Attribs) of
	undefined ->
	    Attribs;
	"application/xml" ->
	    case get_option("description",Attribs) of
		{undefined,Attrbs} ->
		    Attrbs;
		{Text,Attrbs} ->
		    [{"description",xmerl_ucs:to_utf8(Text)}|Attrbs]
	    end
    end.

	


show_login_form(Request) ->
    Headers = [],
    Request:respond({200, [{"Content-Type", "text/html"} | Headers], html(?LOGIN)}).

show_login_form(Request,Error) ->
    Message = "<div color='red'>Login falied " ++ Error ++ "</div>",
    Headers = [],
    Request:respond({200, [{"Content-Type", "text/html"} | Headers], html(?LOGIN ++ Message)}).

a(Resource) ->
    "<a href=\"" ++ Resource ++ "\">" ++ Resource ++ "</a>\n".

q(F) ->
    case mnesia:transaction(F) of
	{atomic,Val} -> {ok,Val};
	Error -> {error,error(Error)}
    end.

%% Todo need to check how mochiweb query/post passing is a multiple field converted to a comma seperated or a lsit of lists? That effects the choice below.
repack_acl(Resource,{_Id,Acl},Attribs) ->
    Cacl = string:join(lists:map(fun erlang:atom_to_list/1,Acl),","),
    [{Resource,Cacl}|proplists:delete(Resource,Attribs)].


tag(Adaptor,Dest,Request,Attributes) ->
    Actor = case credentials(Request) of
		{uri,A} -> 
		    A;
		{token,Token} ->
		    identity_server:actor_from_token(Token)
	    end,
    {Resource,Attrbs} = case get_option("resource",Attributes) of
		   {undefined,Attr} -> 
		       actor_server:lookup(referer(Request));
		   {"http://" ++ Url,Attr} ->
		       {Url,Attr};
		   {Url,Attr} -> 
		       {actor_server:lookup(Url),Attr};
		   Err -> io:format("Err ~p~n",[Err])
		   
	       end,
    Tags = proplists:get_value("tags",Attrbs),
    case {Resource,Tags} of 
	{[],_} -> 
	    error(Adaptor,update,"_/tag/" ++ Dest,Request,"Resource not recognised to tag");
	{undefined,_} -> 
	    error(Adaptor,update,"_/tag/" ++ Dest,Request,"Resource not provided to tag");
	{_,undefined} -> 
	    error(Adaptor,update,"_/tag/" ++ Dest,Request,"Tags not provided for tagging");
	{_,_} ->
	    Attribs = [{Resource,Tags}|proplists:delete("tags",Attrbs)],
	    case actor_server:tag({uri,Actor},?MODULE,Dest,Resource,string:tokens(Tags," ")) of
		{ok,Iacl} ->
		    actor_server:update({uri,Actor},?MODULE,Dest,Attribs),
		    redirect(create,qres(Resource,Request),Request,[]);
		{error,Error} -> 
		    error(Adaptor,update,"_/tag/" ++ Dest,Request,Error);
		{autherror,Why} ->
		    forbidden("_/tag/" ++ Dest,Request,Why)
	    end
    end.

html(Markup) ->
    "<html><body>" ++ Markup ++ "</body></html>".

error(Error) ->
    error_logger:error_msg("Rest server - Says Whoops ~p~n",[Error]),
    Error.

split(Url) ->
    case string:tokens(Url,".") of
	[Resource,Ext] ->
	     {Resource,Ext};
	[Resource] -> 
	    {Resource,".html"};
	_ ->
	    error
    end.

item_to_url(Item) ->
    lists:flatten(string:tokens(Item,?DOMAINSEPERATOR)).

