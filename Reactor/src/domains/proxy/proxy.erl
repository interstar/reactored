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

-module(proxy).
-include("schema.hrl").
-include("system.hrl").
-export([all/6,read/6,write/6,intercept/6]).

%% this matcher is called for all operations
all(_Actor,_Service,_Command,_Domain,_Resource,_Params) ->
	   {nomatch}.

%% This matcher is only called on read operations
read(_Actor,_Service,_Command,_Domain,_Resource,_Params) ->
	   {nomatch}.

%% This matcher is only called on write operations
write(_Actor,_Service,_Command,_Domain,_Resource,_Params) ->
	   {nomatch}.

intercept(Domain,Method,Path,Request,Actor,Proxy) ->
    %{ok,"Default Interceptor, nothing to see move along now."}
    case identity_server:authorise({uri,Actor},proxy,retrieve,{Domain ++ ?DOMAINSEPERATOR ++ "/",[]}) of 
	{ok,_} -> 
	    pattern_server:process(Actor,proxy,proxy,[],Domain,[]),
	    Dom = case string:tokens(Domain,"/") of
		      [] -> 
			  "";
		      D when is_list(D) ->
			  lists:last(D);
		      _ ->
			  ""
		  end,
	    Headers = headers(Request),
	    Url = Proxy ++ Dom ++ "/" ++ string:join(Path,"/"),
	    io:format("headers ~p~n",[Headers]),
	    Req = case Method of
		      'POST' ->
			  {Url,Headers,"application/x-www-form-urlencoded",post_encode(Request)};
		      'GET' ->
			  {Url,Headers}
		  end,
	    case http:request(method(Method),Req,[],[]) of
		{ok,Result} ->
		    {ok,Result};
		{error,Reason} ->
		    error(Reason),
		    {error,"Proxy error"}
	    end;
	{error,Actor,Why} ->
	    error(Why),
	    {error,forbidden}
    end.

method(Method) ->
    case Method of
	'HEAD' ->
	    head;
	'GET' ->
	    get;
	'PUT' ->
	    put;
	'POST' ->
	    post;
	'TRACE' ->
	    trace;
	'OPTIONS' ->
	    options;
	'DELETE' ->
	    delete
    end.

post_encode(Request) ->
    list_to_binary(string:join([K ++ "=" ++ V ||{K,V} <- rest_server:attributes(Request)],"&")).

headers(Request) ->
    Headers = mochiweb_headers:to_list(Request:get(headers)),
    [{atom_to_list(K),V} || {K,V} <- Headers].

error(Error) ->
    error_logger:error_msg("Proxy - Says Whoops ~p~n",[Error]),
    Error.
