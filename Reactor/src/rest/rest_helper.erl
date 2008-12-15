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
-module(rest_helper).
-author('awood@awmb.local').
-include("schema.hrl").
-include("system.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([respond/2,error/5,redirect/4,unauthorised/2,forbidden/3]).
-export([qres/2,domain/1,attributes/2,safe_char_set/1,show_login_form/1,show_login_form/2,q/1,repack_acl/3,tag/5,html/1,split/1,item_to_url/1,title/1,safeUri/1,accepts/1,attributes_and_actor/2,credentials/1,cookie/3,cookie_options/2,save_session/2,remove_session/1,get_credentials/1,add_credentials/1,get_option/2]).

%% Response support functions
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
    Request:respond({403, [{"Content-Type", "text/html"} | Headers], Reason}).

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

attributes_and_actor(Request,Method) ->
    case credentials(Request) of
	{annonymous} ->
	    {attributes(Method,Request),{annonymous}};
	{token,Token} ->
	    {_Token,Attributes} = get_option("token",attributes(Method,Request)),
	    {Attributes,{uri,identity_server:actor_from_token(Token)}};
	{uri,Actor} ->
	    {attributes(Method,Request),{uri,Actor}}
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
	     {port,Port}, %TODO this was 8000 , I replaced it with Port might need converting to integer
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

qres(Resource,_Req) ->
    config_server:domain() ++ ?CONTEXT ++ Resource.
%% should be able to build this from req:get(raw_path)

domain(_Request) ->
    config_server:domain() ++ ?CONTEXT.

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
    Request:respond({200, [{"Content-Type", "text/html"} | Headers], html(?LOGINFORM)}).

show_login_form(Request,Error) ->
    Message = "<div color='red'>Login falied " ++ Error ++ "</div>",
    Headers = [],
    Request:respond({200, [{"Content-Type", "text/html"} | Headers], html(?LOGINFORM ++ Message)}).

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

tag(_Adaptor,Dest,Request,_Attributes,{annonymous}) ->
    forbidden("_/tag/" ++ Dest,Request,"You have to be logged for tagging");
tag(Adaptor,Dest,Request,Attributes,{uri,Actor}) ->
    {Resource,Attrbs} = case get_option("resource",Attributes) of
		   {undefined,_Attr} -> 
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
		{ok,_Iacl} ->
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

