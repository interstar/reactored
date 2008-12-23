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
	    %io:format("headers ~p~n",[Headers]),
	    {M,Req,HO,O} = case Method of
		      'POST' ->
			  {post,{Url,Headers,"application/x-www-form-urlencoded",post_encode(Request)},[],[{body_format, binary}]};
		      'GET' ->
			  {get,{Url ++ query_string(Request),Headers},[],[]}
		  end,
	    %io:format("proxy requesting ~p~n",[Req]),
	    case http:request(M,Req,HO,O) of
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


post_encode(Request) ->
    Body = Request:recv_body(),
    io:format("Proxied post body ~p~n",[Body]),
    Body.
    %list_to_binary(mochiweb_util:urlencode(rest_server:attributes(Request)).
%%     Attributes = case Request:recv_body() of
%%                          undefined ->
%%                              [];
%%                          Binary ->
%%                              case get_primary_header_value("content-type") of
%%                                  "application/x-www-form-urlencoded" ++ _ ->
%%                                      mochiweb_util:parse_qs(Binary);
%%                                  _ ->
%%                                      []
%%                              end
%%                      end,
%%     list_to_binary(string:join([K ++ "=" ++ V ||{K,V} <- Attributes],"&")).

query_string(Request) ->
    {_, QueryString, _} = mochiweb_util:urlsplit_path(Request:get(raw_path)),
    %Attributes = mochiweb_util:parse_qs(QueryString),
    %io:format("Proxied Attributes ~p~n",[Attributes]),
    case QueryString of %mochiweb_util:urlencode(Attributes)
	[] ->
	    "";
	Qs -> 
	    "?" ++ Qs
    end.

headers(Request) ->
    Headers = mochiweb_headers:to_list(Request:get(headers)),
    [{key(K),V} || {K,V} <- Headers].

key(K) when is_list(hd(K)) ->
    hd(K);
key(K) when is_list(K) ->
    K;
key(K) when is_atom(K) ->
    atom_to_list(K).

error(Error) ->
    error_logger:error_msg("Proxy - Says Whoops ~p~n",[Error]),
    Error.
