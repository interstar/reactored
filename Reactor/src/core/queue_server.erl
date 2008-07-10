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
%%% File    : queue_server.erl
%%% Author  : Alan Wood <awood@alan-woods-macbook.local>
%%% Description : 
%%%
%%% Created : 16 May 2008 by Alan Wood <awood@alan-woods-macbook.local>
%%%-------------------------------------------------------------------
-module(queue_server).
-include("system.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0,start/0,stop/0]).
-export([add/3,fetch/1,update/2,delete/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
start() -> start_link().
stop() -> gen_server:call(?MODULE,stop).

add(Source,Sink,Data) ->
    gen_server:call(?MODULE,{put,Source,Sink,Data}).
fetch(Queue) ->
    gen_server:call(?MODULE,{fetch,Queue}).
fetch(Queue,From) ->
    gen_server:call(?MODULE,{fetch,Queue,From}).
update(Mid,Reason) ->
    gen_server:call(?MODULE,{update,Mid,Reason}).
delete(Item) ->
    gen_server:call(?MODULE,{delete,Item}).
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
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
    io:format("~p starting~n",[?MODULE]),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({put,?DOMAIN ++ ?CONTEXT ++ ?SYSTEM,Sink,Data}, _From, State) ->
    Id = queue_util:id(),
    Queue = ?DOMAIN ++ ?CONTEXT ++ ?SYSTEM,
    Reply = queue_util:add(Queue,Sink,Id,Data),
    sink_server:nudge(Queue),
    {reply, Reply, State};
handle_call({put,Source,Sink,Data}, _From, State) ->
    Id = queue_util:id(),
    Queue = ?DOMAIN ++ ?CONTEXT ++ ?QUEUE ++ Source,
    Reply = queue_util:add(Queue,Sink,Id,Data),
    sink_server:nudge(Queue),
    {reply, Reply, State};
handle_call({fetch,Queue}, _From, State) ->
    Reply = queue_util:fetch(Queue, live),
    {reply, Reply, State};
handle_call({update,Mid,Reason}, _From, State) ->
    Reply = queue_util:update(Mid,Reason),
    {reply, Reply, State};
handle_call({delete,Item}, _From, State) ->
    Reply = queue_util:delete(Item),
    {reply, Reply, State};
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
terminate(_Reason, _State) ->
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
error(Error) ->
    error_logger:error_msg("Queue server - Says Whoops ~p~n",[Error]),
    Error.

