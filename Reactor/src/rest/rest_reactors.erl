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
%%% File    : rest_reactors.erl
%%% Author  : Alan Wood <awood@awmb.local>
%%% Description : 
%%%
%%% Created : 11 Dec 2008 by Alan Wood <awood@awmb.local>
%%%-------------------------------------------------------------------
-module(rest_reactors).
-author('awood@awmb.local').
-include("schema.hrl").
-include("system.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([respond_to/6,respond_to/7]).


%% Special actor based Rest API
    
% List actor's tags only
respond_to(actor,Adaptor,retrieve,"~/tag/",{annonymous},Attributes,Request) ->
    rest_helper:forbidden("~/tag/",Request,"You have to be logged in to manage your tags");
respond_to(actor,Adaptor,retrieve,"~/tag/",{uri,Actor},Attributes,Request) -> 
    % TODO this doesn't look right is this for adding tags, reading participant atgs or both or what, not sure the tags call achieves whats required here.
    rest_helper:tag(Adaptor,Actor,Request,Attributes,{uri,Actor});   

% Tag resource (or external url) using 'GET', copy to CC
respond_to(actor,Adaptor,retrieve,"~/tag/" ++ CC,{annonymous},Attributes,Request) ->
    rest_helper:forbidden("~/tag/",Request,"You have to be logged in to tag items");
respond_to(actor,Adaptor,retrieve,"~/tag/" ++ CC,Credentials,Attributes,Request) -> 
    case actor_server:update(Credentials,?MODULE,CC,Attributes) of
	{ok,_Xref} -> 
	    {uri,Actor} = Credentials,
	    rest_helper:tag(Adaptor,Actor,Request,Attributes,Credentials);
	{error,Error} ->
	    rest_helper:error(Adaptor,retrieve,"~/tag/" ++ CC,Request,"Issues copying tag request to destination resource");
	{autherror,Why} ->
	    rest_helper:error(Adaptor,retrieve,"~/tag/" ++ CC,Request,"Current access privelages do not allow tagging and copying")
    end;

% Just items participant tagged with tag/tags
respond_to(actor,Adaptor,retrieve,"~/tags/" ++ Tags,{annonymous},Attributes,Request) ->
    rest_helper:forbidden("~/tags/" ++ Tags,Request,"You have to be logged in in order to view your tags");
respond_to(actor,Adaptor,retrieve,"~/tags/" ++ Tags,{uri,Actor},Attributes,Request) ->
	    Domain = rest_helper:domain(Request),
	    Url = Actor ++ "/tags/",
	    case actor_server:tagged({uri,Actor},?MODULE,Domain,{string:tokens(Tags,"/"),Actor}) of
		{error, Error} -> 
		    rest_helper:error(Adaptor,retrieve,Url,Request,Error);
		{autherror,Why} ->
		    rest_helper:forbidden(Url,Request,Why);
		Items -> 
		    rest_helper:respond(Request,Adaptor:render("Your Tags",Url,Items))
	    end.

%% Internal API REST interfaces

%% Domain REST interface
respond_to(Adaptor,retrieve,"_/",Credentials,Attributes,Request) ->
    Res = rest_helper:qres("_/",Request),
    case actor_server:domain(Credentials,?MODULE,actor_server:lookup(Res)) of 
	{ok,Domains} ->  
	    Props = [{"title","Domains"},{"uri",Res},{"description","Domain Listing"},{"author",rest_helper:domain(Request) ++ ?IDENTITIES ++ "/founder"}],
	    rest_helper:respond(Request,Adaptor:render("Domains",Res,Props ++ Domains));
	{error,Error} ->
	    rest_helper:error(Adaptor,retrieve,"_/",Request,"Could not retrieve domain list " ++ Error);
	{autherror,Why} ->
	    rest_helper:forbidden("_/",Request,Why)
    end;

respond_to(Adaptor,create,"_/",Credentials,Attributes,Request) ->
    Matcher = case proplists:get_value("matcher", Attributes) of
		undefined -> 
		    ?ERRORMATCHER;
		M -> M
	    end,
    case proplists:get_value("domain", Attributes) of
	undefined -> 
	    rest_helper:error(Adaptor,retrieve,"_/",Request,"Domain not provided, could not create domain ");
	Domain ->   
	    case actor_server:domain(Credentials,?MODULE,?CONTEXT ++ Domain,rest_helper:qres("_/",Request),Matcher) of
		{ok,_Domains} -> 
		    rest_helper:redirect(create,"_/",Request,[]);
		{error,Error} ->
		    rest_helper:error(Adaptor,retrieve,"_/",Request,"Could not create " ++ Error);
		{autherror,Why} ->
		    rest_helper:forbidden("_/",Request,Why)
	    end
    end;


respond_to(_Adaptor,Operation,"_/echo/" ++ Resource,Credentials,Attributes,Request) ->
    %% these echo responses could be extended if we moved the service detector up higher and call react with the service as a param rather than using Module nside react calls, we would have to use a header or something instead of the _/echo/ url, In fact Service could also indicate API vs None API calls
    Response = case Operation of
		   retrieve -> 
		       actor_server:retrieve(Credentials,echo,rest_helper:qres(Resource,Request));
		   create -> 
		       case lists:last(Resource) of
			   $/ -> 
			       Domain = rest_helper:domain(Request),
			       Item = Resource ++ attribute:today() ++ "_" ++ rest_helper:title(Attributes),
			       actor_server:create(Credentials,echo,Domain,Item,Attributes);
			   _ -> 
			       actor_server:update(Credentials,echo,rest_helper:qres(Resource,Request), rest_helper:attributes('POST',Request))
		       end;
		   update -> 
		       actor_server:update(Credentials,echo,rest_helper:qres(Resource,Request), rest_helper:attributes('POST',Request));
		   delete -> 
		       case rest_helper:attributes("DELETE",Request) of
			   [] -> actor_server:delete(Credentials,echo,rest_helper:qres(Resource,Request));
			   Attributes -> actor_server:delete(Credentials,echo,rest_helper:qres(Resource,Request),Attributes)
		       end
	       end,   
    Request:respond({200, [], Response});

respond_to(_Adaptor,retrieve,"_/logout" ++ _,{uri,Actor},Attributes,Request) ->
    rest_helper:remove_session(Request),
    rest_helper:redirect(create,Actor,"_/login",[]);

%% Tagging REST interface

% Tag resource (or external url) using 'Post' under identities ID tags
% This is like tag posting, tagging to a resource's tag fork or to a identity tag fork
respond_to(Adaptor,create,"_/tag/" ++ Dest,Credentials,Attributes,Request) -> 
    % Todo fixes dstination as a provided actor (dest) this could be opened to be less specific as in bookmarking sending a tagged hyperlink to a given resource.
    rest_helper:tag(Adaptor,rest_helper:qres(?IDENTITIES ++ ?DOMAINSEPERATOR ++ "/" ++ Dest ++ "/tags",Request),Request,Attributes,Credentials);

% TODO No tags provided in url, try parameters NOT YET IMPLEMENTED
respond_to(Adaptor,retrieve,"_/tags/",Credentials,Attributes,Request) ->
    rest_helper:error(Adaptor,retrieve,"_/tags/",Request,"Parameter based tag querying is not currently implemented");

% All Items tagged with tag/tags that participant has retrieve acl for
respond_to(Adaptor,retrieve,"_/tags/" ++ Tagz,Credentials,Attributes,Request) ->
    Domain = rest_helper:domain(Request),
    Url = Domain ++ "_/tags/"  ++ Tagz,
    Tags = string:tokens(Tagz,"/"),
    Tagged = lists:flatten(["Tagged : "|Tags]),
    case actor_server:tagged(Credentials,?MODULE,Domain,Tags) of
	{error, Error} -> 
	    rest_helper:error(Adaptor,retrieve,"_/tags/" ++ Tagz,Request,Error);
	{autherror,Why} ->
	    rest_helper:forbidden("_/tags/" ++ Tagz,Request,Why);
	Items -> 
	    rest_helper:respond(Request,Adaptor:render(Tagged,Url,item:chrono(Items)))
    end;

respond_to(Adaptor,create,"_/search/" ++ S,Credentials,Attributes,Request) ->
    Text = case proplists:get_value("search", Attributes) of
	       undefined -> "";
	       Val -> Val
	   end,
    Domain = rest_helper:domain(Request),
    Url = Domain ++ "_/search/"  ++ S,
    Title = "Search Results for " ++ Text,
    case actor_server:search(Credentials,?MODULE,Domain,Text) of
	{error, Error} -> 
	    rest_helper:error(Adaptor,retrieve,"_/search/" ++ S,Request,Error);
	{autherror,Why} ->
	    rest_helper:forbidden("_/search/" ++ S,Request,Why);
	Items -> 
	    rest_helper:respond(Request,Adaptor:render(Title,Url,lists:map(fun({T,I}) -> I end,Items)))
    end;

respond_to(Adaptor,retrieve,"_/search/" ++ Tokens,Credentials,Attributes,Request) ->
    Search = string:tokens(Tokens,"/"),
    Text = string:join(Search," "),
    Domain = rest_helper:domain(Request),
    Url = Domain ++ "/_/search/"  ++ Tokens,
    Title = lists:flatten(["Search Results for "|Text]),
    case actor_server:search(Credentials,?MODULE,Domain,Text) of
	{error, Error} -> 
	    rest_helper:error(Adaptor,retrieve,"_/search/" ++ Tokens,Request,Error);
	{autherror,Why} ->
	    rest_helper:forbidden("_/search/" ++ Tokens,Request,Why);
	Items -> 
	    rest_helper:respond(Request,Adaptor:render(Title,Url,lists:map(fun({T,I}) -> I end,Items)))
    end;

%% Identity REST reactors    

%% I think this is now redundant and I'm not sure if the profile call is relevant
%% respond_to(Adaptor,retrieve,"_/id/" ++ Id,Request) ->
%%     Domain = domain(Request),
%%     Url = Domain ++ "/_/id/"  ++ Id,
%%     Title = "Profile of " ++ Id,
%%     case actor_server:profile(credentials(Request),?MODULE,Domain,Id) of
%% 	{error, Error} -> error(Adaptor,retrieve,"_/id/" ++ Id,Request,Error);
%% 	Items -> respond(Request,Adaptor:render(Title,Url,Items))
%%     end;

respond_to(Adaptor,create,"_/id/",Credentials,Attributes,Request) ->
    Domain = rest_helper:domain(Request) ++ ?IDENTITIES,
    %Item = "_/id/" ++ attribute:today() ++ "_" ++ rest_helper:title(Attributes),
    Item = case rest_helper:title(Attributes) of
		[] ->
		  "/" ++ attribute:today();
		Title ->
		    case actor_server:lookup(Domain ++ "/" ++ Title) of
			[] ->
			    "/" ++ Title;
			Qitem ->
			    "/" ++ attribute:today() ++ "_" ++ Title
		    end
	    end,
    %io:format("Domain,Item ~p~n",[{Domain,Item}]),
    case actor_server:new(Credentials,?MODULE,Domain,Item,Attributes) of
	{ok,_Xref} -> 
	    rest_helper:redirect(create,?IDENTITIES ++ Item,Request,[]);
	{error,Error} -> 
	    rest_helper:error(Adaptor,retrieve,"_/id/",Request,Error);
	{autherror,Why} ->
	    rest_helper:forbidden("_/id/",Request,Why)
    end;
%Todo this isn't tested at all yet.
respond_to(Adaptor,delete,"_/id/" ++ Id,Credentials,Attributes,Request) ->
    case actor_server:remove(Credentials,?MODULE,rest_helper:qres("_/id/" ++ Id,Request)) of
	{ok,_Xref} -> 
	    rest_helper:redirect(create,?IDENTITIES ++ "/",Request,[]);
	{error,Error} -> 
	    rest_helper:error(Adaptor,delete,"_/id/" ++ Id,Request,Error);
	{autherror,Why} ->
	    rest_helper:forbidden("_/id/" ++ Id,Request,Why)
    end;

%% ACL REST reactors
% Todo much of this could be redundant and instead be placed under /identity/id/acl/ namespace rather than its own, particularly is an acl is created for every user by default. The trick is the pattern matching to recognise the URL!
respond_to(Adaptor,create,"_/acl/",Credentials,Attributes,Request) ->
    rest_helper:error(Adaptor,create,"_/acl/",Request,"Identity ACL can not be created remotely, it already exists");
respond_to(Adaptor,retrieve,"_/acl/",Credentials,Attributes,Request) ->
    %rest_helper:forbidden(Resource,Request,Why)
    rest_helper:error(Adaptor,retrieve,"_/acl/",Request,"Identity ACLs can not be listed remotely, it is an internal function only");

respond_to(Adaptor,retrieve,"_/acl/" ++ Id,Credentials,Attributes,Request) ->
    respond_to(Adaptor,retrieve,?IDENTITIES ++ "/" ++ Id ++ "/acl",Credentials,Attributes,Request);

respond_to(Adaptor,create,"_/acl/" ++ Id,Credentials,Attributes,Request) ->
    respond_to(Adaptor,update,"_/acl/" ++ Id,Credentials,Attributes,Request);

respond_to(Adaptor,update,"_/acl/" ++ Id,Credentials,Attributes,Request) ->
    %io:format("About to update ACL fork ~p~n",[{Id,Attributes}]),
    % contains resource (Qitem, or use referer) being ACL'ed + controls
    {Resource,Attrib} = case rest_helper:get_option("resource",Attributes) of
		   {undefined,Attr} -> 
				case actor_server:lookup(rest_helper:referer(Request)) of
				    [] ->
					{undefined,Attr};
				    Qitem ->
					{Qitem,Attr}
				end;
		   {Uri,Attr} -> % expects item uri ref not url!!
		       {Uri,Attr}
	       end,
    Acl = proplists:get_value("acl",Attrib),
    Idurl = rest_helper:domain(Request) ++ ?IDENTITIES,
    case {Resource,Acl} of 
	{undefined,_} -> 
	    rest_helper:error(Adaptor,update,"_/acl/" ++ Id,Request,"Resource not provided for ACL Update");
	{_,undefined} -> 
	    rest_helper:error(Adaptor,update,"_/acl/" ++ Id,Request,"ACL not provided for ACL Update");
	{_,_} ->
	    %io:format("Updating ACL fork ~p~n",[{Resource,Acl,Attrib}]),
	    Attribs = [{Resource,Acl}|Attrib],
	    case rest_helper:get_option("control",Attribs) of
		{"grant",Atts} -> 
		    case actor_server:grant(Credentials,?MODULE,rest_helper:qualified(Idurl,"/" ++ Id),Resource,Atts) of
			{ok,Iacl} ->
			    actor_server:update(Credentials,?MODULE,rest_helper:qualified(Idurl,"/" ++ Id ++ "/acl"),rest_helper:repack_acl(Resource,Iacl,Atts)),
			    rest_helper:redirect(update,?CONTEXT ++ ?IDENTITIES ++ "/" ++ Id ++ "/acl",Request,[]);
			{error,Error} -> 
			    rest_helper:error(Adaptor,update,"_/acl/" ++ Id,Request,Error);
			{autherror,Why} ->
			    rest_helper:forbidden(Resource,Request,Why)
		    end;
		{"revoke",Atts} ->
		    case actor_server:revoke(Credentials,?MODULE,rest_helper:qualified(Idurl,"/" ++ Id),Resource,Atts) of
			{ok,Iacl} -> 
			    actor_server:update(Credentials,?MODULE,rest_helper:qualified(Idurl,"/" ++ Id ++ "/acl"),rest_helper:repack_acl(Resource,Iacl,Atts)),
			    rest_helper:redirect(update,?CONTEXT ++ ?IDENTITIES ++ "/" ++ Id ++ "/acl",Request,[]);
			{error,Error} -> 
			    rest_helper:error(Adaptor,update,"_/acl/" ++ Id,Request,Error);
			{autherror,Why} ->
			    rest_helper:forbidden(Resource,Request,Why)
		    end;
		{undefined,Atts} -> 
		    rest_helper:error(Adaptor,update,"_/acl/" ++ Id,Request,"ACL control not supplied ");
		{Control,Atts} -> 
		    rest_helper:error(Adaptor,update,"_/acl/" ++ Id,Request,"ACL " ++ Control ++ " not supported")
	    end
    end;


%% General Namespace REST Reactors

respond_to(Adaptor,retrieve,Resource,Credentials,Attributes,Request) ->
    %io:format("Retrieving resource ~s~n",[Resource]),
    case lists:last(Resource) of
	$/ -> respond_to(Adaptor,list,Resource,Credentials,Attributes,Request);
	_ ->
	    Domain = rest_helper:domain(Request),
	    Url = Domain ++ Resource,
	    Title = "Resource " ++ Resource,
	    case actor_server:lookup(rest_helper:qres(Resource,Request)) of
		[] ->
		    rest_helper:error(Adaptor,retrieve,Resource,Request,"Could not identify resource " ++ Resource);
		Qitem ->
		    case Attributes of
			[] -> 
						% Retrieve entire item, all attributes
			    case actor_server:retrieve(Credentials,?MODULE,Qitem) of
				{error,Error} -> 
				    rest_helper:error(Adaptor,retrieve,Resource,Request,Error);
				{autherror,Why} ->
				    rest_helper:forbidden(Resource,Request,Why);
				{ok,Attrib} ->
				    Response = Adaptor:render(Title,Url,Attrib),
				    rest_helper:respond(Request,Response)
			    end;
			Attrib -> 
						% partial retrieve specified attributes only
			    case actor_server:retrieve(Credentials,?MODULE,Domain,Resource,Attrib) of
				{error,Error} -> 
				    rest_helper:error(Adaptor,retrieve,Resource,Request,Error);
				{autherror,Why} ->
				    rest_helper:forbidden(Resource,Request,Why);
				Attrib -> 
				    rest_helper:respond(Request,Adaptor:render(Title,Url,Attrib))
			    end
		    end
	    end
    end;

% handles domain queries and graph queries (1 or other)
respond_to(Adaptor,list,Resource,Credentials,Attributes,Request) ->
    [_|Niamod] = lists:reverse(rest_helper:qres(Resource,Request)),
    Domain = lists:reverse(Niamod),
    case domain:retrieve(Domain) of
	{atomic,[]} -> % graph query
	    Res = lists:reverse(tl(lists:reverse(Resource))),
	    case actor_server:lookup(rest_helper:qres(Res,Request)) of
		[] ->
		    rest_helper:error(Adaptor,list,Resource,Request,"Cannot graph unknown resourse" ++ Res);
		Qitem ->
		    [Dom|Query] = string:tokens(lists:flatten(Qitem),?DOMAINSEPERATOR),
		    case actor_server:graph(Credentials,?MODULE,Dom,lists:flatten(Query),Attributes) of
			{error,Error} -> 
			    rest_helper:error(Adaptor,list,Resource,Request,Error);
			{autherror,Why} ->
			    rest_helper:forbidden(Resource,Request,Why);
			[] -> % TODO could probably loose this once graph is debugged
			    io:format("Empty Graph query call args: ~n~p",[{Credentials,?MODULE,Dom,Query,Attributes}]),
			    rest_helper:respond(Request,Adaptor:render("Resource Graph Query (Empty) " ++ Dom ++ "," ++ Query,Resource,[]));
			Items ->
			    rest_helper:respond(Request,Adaptor:render("Resource Graph Query " ++ Domain,Resource,Items))
		    end
	    end;
	_ -> % domain query
	    case actor_server:q(Credentials,?MODULE,Domain, Attributes) of
		{error,Error} -> 
		    rest_helper:error(Adaptor,list,Resource,Request,Error);
		{autherror,Why} ->
		    rest_helper:forbidden(Resource,Request,Why);
		Items ->
		    rest_helper:respond(Request,Adaptor:render("Domain Query " ++ Domain,Resource,Items))
	    end
    end;


respond_to(Adaptor,create,Resource,Credentials,Attributes,Request) ->
    case lists:last(Resource) of
	$/ ->
	    R = case string:tokens(Resource, "/") of
			   [_] ->
			       Resource;
			   R1 -> 
			       string:join(R1,"/")
		       end,
	    case actor_server:lookup(rest_helper:qres(R,Request)) of
		[] ->
		    rest_helper:error(Adaptor,create,Resource,Request,"Cannot create resource as child of unknown resource/domain " ++ R);
		Qitem ->
		    [Domain,Res] = string:tokens(Qitem,?DOMAINSEPERATOR),
		    {Tags,A1} = rest_helper:get_option("tags",Attributes),
		    {Redirect,A2} = rest_helper:get_option("redirect",A1),
		    R2 = case Res of 
			     "/" ->
				 Res;
			     _ ->
				 Res ++ "/"
			 end,
		    {Item,Attributes1} = case rest_helper:get_option("itemid",A2) of
					    {undefined,A4} ->
						{R2 ++ attribute:today() ++ "_" ++ rest_helper:title(A4),A4};
					    {Id,A4} ->
						{R2 ++ Id,A4}
					end,
		    case actor_server:create(Credentials,?MODULE,Domain,Item,Attributes) of
			{ok,_Xref} -> 
			    case Tags of
				undefined ->
				    case Redirect of
					undefined ->
					    respond_to(Adaptor,retrieve,Resource,Credentials,Attributes,Request);
					_ -> 
					    rest_helper:redirect(create,Redirect,Request,[])
				    end;
				Tags ->
				    {uri,A} = Credentials,
				    Dest = A ++ "/tags",
				    Tagz = string:tokens(Tags," "),
				    case identity_server:tag(A,Domain ++ ?DOMAINSEPERATOR ++ Item,Tagz) of
					{ok,Iacl} ->
					    actor_server:update(Credentials,?MODULE,Dest,[{Item,Tags}|Attributes1]),
					    case Redirect of
						undefined ->
						    respond_to(Adaptor,retrieve,Resource,Credentials,Attributes,Request);
						_ ->
						    rest_helper:redirect(create,Redirect,Request,[])
					    end;
					{error,Error} -> 
					    rest_helper:error(Adaptor,update,"_/tag/" ++ Dest,Request,Error);
					{autherror,Why} ->
					    rest_helper:forbidden("_/tag/" ++ Dest,Request,Why)
				    end
			    end;
			{error,Error} -> 
			    rest_helper:error(Adaptor,create,Resource,Request,Error);
			{autherror,Why} ->
			    rest_helper:forbidden(Resource,Request,Why)
		    end
	    end;
	_ -> 
	    %% cannot create unless ends in "/" must be an update (POST faking PUT)
	    respond_to(Adaptor,update,Resource,Credentials,Attributes,Request)
    end;

respond_to(Adaptor,update,Resource,Credentials,Attributes,Request) ->
    case actor_server:update(Credentials,?MODULE,rest_helper:qres(Resource,Request), Attributes) of
	{ok,_Xref} -> 
	    respond_to(Adaptor,retrieve,Resource,Credentials,Attributes,Request);
	{error,Error} -> 
	    rest_helper:error(Adaptor,retrieve,Resource,Request,Error);
	{autherror,Why} ->
	    rest_helper:forbidden(Resource,Request,Why)
    end;

respond_to(Adaptor,delete,Resource,Credentials,Attributes,Request) ->
    case actor_server:lookup(rest_helper:qres(Resource,Request)) of
	[] ->
	    rest_helper:error(Adaptor,retrieve,Resource,Request,"Could not identify resource to be deleted " ++ Resource);
	Qitem ->
	    Attributes = rest_helper:attributes("DELETE",Request),
	    case Attributes of
		[] -> 
		    case actor_server:delete(Credentials,?MODULE,Qitem) of
			%% Todo Should we be forwarding to parent on a delete item? what does an http DELETE expect in reply, Also going up to parent may cock up resource linking like styles/images as browsers url will now be incorrect?
			{ok,_Xref} -> 
			    rest_helper:redirect(delete,attribute:parent(rest_helper:domain(Request),Resource),Request,[]);
			{error,Error} -> 
			    rest_helper:error(Adaptor,retrieve,Resource,Request,Error);
			{autherror,Why} ->
			    rest_helper:forbidden(Resource,Request,Why)
		    end;
		_ -> 
		    case actor_server:delete(Credentials,?MODULE,rest_helper:qres(Resource,Request),Attributes) of
			{ok,_Xref} -> 
			    respond_to(Adaptor,retrieve,Resource,Credentials,Attributes,Request);
			{error,Error} -> 
			    rest_helper:error(Adaptor,retrieve,Resource,Request,Error);
			{autherror,Why} ->
			    rest_helper:forbidden(Resource,Request,Why)
		    end
	    end
    end;

respond_to(_Adaptor,Operation,Resource,Credentials,Attributes,Request) ->
    io:format("Resource not found ~s ~s~n",[atom_to_list(Operation),Resource]),
    Request:not_found().
    %Request:respond({501, [{"Content-Type", "plain/text"}], "Unrecognised request for " ++ ?CONTEXT ++ Resource}).

