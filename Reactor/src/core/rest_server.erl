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
                    Res = upload:store(Req),
		    io:format("Upload response ~p~n",[Res]),
		    Headers = [],
		    Req:respond({200, [{"Content-Type", "text/html"} | Headers], rest_helper:html("<h1>uploaded</h1>")})
            end;
        _ ->
            Req:respond({501, [], []})
    end.

%% Basic REST Operations
react_to('HEAD',Url,Request) -> %% Todo optimise HEAD calls
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
