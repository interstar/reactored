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
-export([indexes/0]).
-export([start/1, stop/0, react_to/2]).


indexes() ->
    F = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(usession)]))
	end,
    mnesia:transaction(F).

%% External API

start(Options) ->
    {DocRoot, Options1} = rest_helper:get_option(docroot, Options),
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
react_to(_Method,"/~",Request,_DocRoot) ->
    io:format("Home ~s~n",["Where the heart is"]),
    case rest_helper:credentials(Request) of
	{uri,Actor} ->
	    Url = rest_helper:item_to_url(Actor),
	    io:format("Redirecting to ~s~n",[Url]),
	    rest_helper:redirect(retrieve,"http://" ++ Url,Request,[]);
	_ -> 
	    rest_helper:redirect(retrieve,rest_helper:domain(Request) ++ "_login",Request,[])
    end;

react_to(Method,"/~" ++ Resource,Request,_DocRoot) ->
    case rest_helper:attributes_and_actor(Request,Method) of
	{_,{annonymous}} ->
	    rest_helper:forbidden(Resource,Request,"You have to be logged in to access your resources");
	{Attributes,Credentials} ->
	    rest_reactors:respond_to(actor,adaptor(lists:reverse(Resource),rest_helper:accepts(Request)),Method,"~"++ Resource,Credentials,Attributes,Request)
    end;

%% Reactor REST dispatcher
react_to(Method,?CONTEXT ++ Resource,Request,_DocRoot) ->
    react_to(Method,Resource,Request);

%% Reactor public Resource dispatcher
react_to(Method,?PUBLIC ++ Path,Req, DocRoot) ->
    case Req:get(method) of
	Method when Method =:= 'GET'; Method =:= 'HEAD' ->
	    case Path of
		_ ->
		    Req:serve_file(Path, DocRoot)
	    end;
	_ ->
	    Req:respond({501, [], []})
    end;

%% Logging into REST interface
%% Todo implement plain http login as an alternative to form based login
react_to('GET',?LOGIN,Request,DocRoot) ->
    io:format("Cookie ~p~n",[Request:get_cookie_value(?COOKIE)]),
    rest_helper:show_login_form(Request); % TODO we might just forward to /static/login.html instead, same below on login errors
react_to('POST',?LOGIN,Request,DocRoot) ->
    io:format("~n Authenticating ~n"),
    Attributes = rest_helper:attributes('POST',Request),
    %% Todo need to handle cases where login params are no provided
    Id = proplists:get_value("identity", Attributes),
    Pswd = proplists:get_value("password", Attributes),
    case identity_server:authenticate(Id,Pswd) of
	{ok,Actor} -> 
	    io:format("Authenticated ~s~n",[Actor]), 
	    Header = rest_helper:save_session(Request,Actor),
	    io:format("Headers ~p~n",[Header]),
	    %redirect(create,Actor,Request,[Header]);
	    Request:respond({200, [{"Content-Type", "text/html"} | [Header]], rest_helper:html("<h3>Logged in</h3>")});
	{error,Error} ->
	    io:format("Not authenticated ~s~n",[Error]),
	    rest_helper:show_login_form(Request,Error)
    end; 


%% Reactor Resource dispatcher
react_to(Method,?RESOURCES ++ Path,Req, DocRoot) ->
    case Req:get(method) of
	Method when Method =:= 'GET'; Method =:= 'HEAD' ->
	    case Path of
		_ ->
		    Req:serve_file(Path, DocRoot)
	    end;
	'POST' ->
	    case Path of
		_ ->
		    Res = upload:store(Req),
		    io:format("Upload response ~p~n",[Res]),
		    Req:respond({200,[{"Content-Type","text/html"}],rest_helper:html("<h1>uploaded</h1>")})
	    end;
	_ ->
	    Req:respond({501, [], []})
    end;

% Domain Interceptors (proxies etc..)
react_to(_Method,"/",Request, _DocRoot) ->
    Request:respond({404, [], []});
react_to(Method,"/" ++ Path,Request,_DocRoot) ->
    [Dom|Resource] = string:tokens(Path,"/"),
    Domain = rest_helper:qres(Dom,Request),
    {_Attributes,Credentials} = rest_helper:attributes_and_actor(Request,Method),
    case Credentials of
	{annonymous} ->
	    rest_helper:forbidden(Resource,Request,"You have to be logged in to access domains");
	{uri,Actor} -> 
	    case domain:retrieve(Domain) of
		{atomic,[]} ->
		    rest_helper:error(html_adaptor,Method,"/_" ++ Path,Request,"No domain for interceptor");
		_ ->
		    io:format("about to call intercept Domain ~p~n",[Domain]),
		    case domain_server:intercept(Domain,Method,Resource,Request,Actor) of
			{error,forbidden} ->
			    rest_helper:forbidden(Resource,Request,"You have to be logged in to create or update resources");
			{error,Reason} ->
			    error(Reason),
			    rest_helper:error(html_adaptor,Method,"/_" ++ Path,Request,"Sorry an error occured with your request");
			{ok,{Code, Result}} ->	    
			    Request:respond({Code, [], Result});
			{ok,{Status,Headers,Result}} ->
			    {_Vers, Code, _Reason} = Status,	    
			    Request:respond({Code, Headers, Result});
			{ok,Result} ->
			    Request:respond({200,[{"Content-Type","text/html"}],Result});
			_ ->
			    rest_helper:error(html_adaptor,Method,"/_" ++ Path,Request,"Could not handle request")
		    end
	    end
    end.

%% Basic REST Operations
react_to('HEAD',Url,Request) -> 
    %% Todo optimise HEAD calls
    case rest_helper:split(Url) of
	{Resource,Ext} ->
	    {Attributes,Credentials} = rest_helper:attributes_and_actor(Request,'HEAD'),
	    rest_reactors:respond_to(adaptor(Ext,rest_helper:accepts(Request)),retrieve,Resource,Credentials,Attributes,Request);
	_ ->
	    rest_helper:error(html_adaptor,'HEAD',Url,Request,"Illegal resource request")
    end;
react_to('GET',Url,Request) -> 
    case rest_helper:split(Url) of
	{Resource,Ext} ->
	    {Attributes,Credentials} = rest_helper:attributes_and_actor(Request,'POST'),
	    rest_reactors:respond_to(adaptor(Ext,rest_helper:accepts(Request)),retrieve,Resource,Credentials,Attributes,Request);
	_ ->
	    rest_helper:error(html_adaptor,'HEAD',Url,Request,"Illegal resource request")
    end;
react_to('POST',Url,Request) ->
    case rest_helper:split(Url) of
	{Resource,Ext} ->
	    case rest_helper:attributes_and_actor(Request,'POST') of
		{_,{annonymous}} ->
		    rest_helper:forbidden(Resource,Request,"You have to be logged in to create or update resources");
		{Attributes,Credentials} ->
		    rest_reactors:respond_to(adaptor(Ext,rest_helper:accepts(Request)),create,Resource,Credentials,Attributes,Request)
	    end;
	_ ->
	    rest_helper:error(html_adaptor,'HEAD',Url,Request,"Illegal resource request")
    end;
react_to('PUT',Url,Request) ->
    case rest_helper:split(Url) of
	{Resource,Ext} ->
	    case rest_helper:attributes_and_actor(Request,'POST') of
		{_,{annonymous}} ->
		    rest_helper:forbidden(Resource,Request,"You have to be logged in to create or update resources");
		{Attributes,Credentials} ->
		    rest_reactors:respond_to(adaptor(Ext,rest_helper:accepts(Request)),update,Resource,Credentials,Attributes,Request)
	    end;
	_ ->
	    rest_helper:error(html_adaptor,'HEAD',Url,Request,"Illegal resource request")
    end;
react_to('DELETE',Url,Request) ->
    case rest_helper:split(Url) of
	{Resource,Ext} ->
	    case rest_helper:attributes_and_actor(Request,'POST') of
		{_,{annonymous}} ->
		    rest_helper:forbidden(Resource,Request,"You have to be logged in to delete resources");
		{Attributes,Credentials} ->
		    rest_reactors:respond_to(adaptor(Ext,rest_helper:accepts(Request)),delete,Resource,Credentials,Attributes,Request)
	    end;
	_ ->
	    rest_helper:error(html_adaptor,'HEAD',Url,Request,"Illegal resource request")
    end;

%% Catch all, error default
react_to(Method,Resource,Request) ->
    Request:respond({501, [], atom_to_list(Method) ++ " not supported for " ++ Resource}).

%% Content & Type Adaptors
adaptor("atom" ++ _, _Accept) ->
    atom_adaptor;
adaptor("js" ++ _, _Accept) ->
    json_adaptor;
adaptor("html" ++ _, _Accept) ->
    html_adaptor;
adaptor("xhtml" ++ _, _Accept) ->
    xhtml_adaptor;
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

error(Error) ->
    error_logger:error_msg("Actor server - Says Whoops ~p~n",[Error]),
    Error.
