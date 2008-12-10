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
%%% File    : action_server.erl
%%% Author  : Alan Wood <awood@alan-woods-macbook.local>
%%% Description : 
%%%
%%% Created : 29 May 2008 by Alan Wood <awood@alan-woods-macbook.local>
%%%-------------------------------------------------------------------
-module(action_server).
-include("system.hrl").
-include("schema.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([start/0,stop/0]).
-export([action/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%%====================================================================
%% API
%%====================================================================
action(Action)->
    gen_server:call(?MODULE,{action,Action}).
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start() -> start_link().
stop() -> gen_server:call(?MODULE,stop).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    io:format("~p starting~n",[?MODULE]),
    {ok, ets:new(?MODULE,[])}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({action,Action}, _From, Sinks) ->
    {_Queue,Sink,Mid} = decode(Action),
    Reply = case apply(get_sinker(Sink,Sinks),sink,[Action]) of
	ok -> 
	    {true,Mid};
	{error,Why} -> 
	    error({"Sink failed",Sink,Why}),
{false,Mid,Why};
	Bad -> 
	    {false,Mid,error({"Sink failed",Sink,Bad})} % sinker module error/ not loaded?
    end,
    {reply, Reply, Sinks};
handle_call(stop, _From, State) ->
    {stop,normal,stopped, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, Sinkers) ->
    %% stop all sinkers and unload them
    lists:foreach(fun plugin:unload/1,ets:tab2list(Sinkers)),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
get_sinker(Sink,Sinks) ->
    %% Lookup sink in sinkers dict
    case ets:lookup(Sinks,Sink) of
	[] -> % Not yet loaded so load it first, then add it to live matchers
	    Sinker = case plugin:load( list_to_atom(Sink)) of
		       {ok,Plugin} -> 
			   Plugin;
		       {error} -> 
			   case plugin:load(?ERRORSINKER) of
			       {ok,Plugin} ->
				   Plugin;
			       _ -> 
				   ?ERRORSINKER
			   end
		   end,
	    ets:insert(Sinks,{Sink,Sinker}),
	    plugin:get_server(Sinker);
	[{_Snk,Snkr}] -> 
	    plugin:get_server(Snkr)
    end.

decode(Action) ->
    Queue = Action#item.domain,
    Sink = Action#item.type,
    Mid = Action#item.item,
    {Queue,Sink,Mid}.

error(Error) ->
    error_logger:error_msg("Action server - Says Whoops ~p~n",[Error]),
    Error.
