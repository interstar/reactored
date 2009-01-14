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
%%% File    : sink_server.erl
%%% Author  : Alan Wood <awood@alan-woods-macbook.local>
%%% Description : 
%%%
%%% Created : 19 May 2008 by Alan Wood <awood@alan-woods-macbook.local>
%%%-------------------------------------------------------------------
-module(sink_server).
-include("system.hrl").
-include("schema.hrl").
-behaviour(gen_server).

%% API
-export([start_link/1,start/1,stop/0]).
-export([flush/0,nudge/1,fetch/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
start(Domain) -> start_link(Domain).
stop() -> gen_server:call(?MODULE,stop).

flush() ->
    gen_server:cast(?MODULE,flush).
nudge(Queue) ->
    gen_server:cast(?MODULE,{nudge,Queue}).
fetch() ->
    gen_server:cast(?MODULE,fetch).
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Domain) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Domain], []).

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
init([Domain]) ->
    monostable:start(?MODULE,fetch,5000),
    Queues = ets:new(?MODULE,[]),
    ets:insert(Queues,{Domain ++ ?CONTEXT ++ ?SYSTEM,0}),
    io:format("~p starting~n",[?MODULE]),
    {ok, Queues}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
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
handle_cast(flush, Queues) ->
    monostable:trigger([]), % Trigger monostable for fetch
    {noreply, Queues};
handle_cast({nudge,Queue}, Queues) ->
    add_queue(Queue,Queues),
    monostable:trigger([]), % Trigger monostable for fetch
    {noreply, Queues};
handle_cast(fetch, Queues) ->
    % ToDo this could be enhaced by spawning each queue actioning?
    %io:format("About to run sink fetch on ~p",[ets:tab2list(Queues)]),
    lists:foreach(fun action_queue/1,ets:tab2list(Queues)),
    {noreply, Queues};
handle_cast(_Msg, Queues) ->
    {noreply, Queues}.

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
    monostable:stop(),
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
add_queue(Queue,Queues) ->
     %% Lookup queue in queues dict
    case ets:lookup(Queues,Queue) of
	[] -> % Not yet known add to queues
	    ets:insert(Queues,{Queue,1});
	[{_,_}] -> 
		   void
    end.

action_queue({Queue,_Priority}) ->
    Actions = case queue_server:fetch(Queue) of
	{ok,Actns} -> Actns;
		  _ -> []
	      end,
    sink(Actions).

sink([Action|Actions]) ->
    case action_server:action(Action) of
	{true,Mid} -> % Action sunk delete from queue
	    %io:fwrite("Deleting : ~s ~s~n",[Queue,Mid]),
	    queue_server:delete(Mid),
	    sink(Actions);
	{false,Mid,Reason} -> % Action not sunk, update queue history
	    queue_server:update(Mid,Reason),
	    sink(Actions)
    end;
sink([]) ->
	  void.  

    
error(Error) ->
    error_logger:error_msg("Sink server - Says Whoops ~p~n",[Error]),
    Error.

% redundant
process(Action) -> % Action -> {Queue,Sink,Aid,Data}
    io:fwrite("Actioning : ~p~n",[Action]),
     {_Queue,_Sink,Mid} = decode(Action),
    {true,Mid}.


decode(Action) ->
    Queue = Action#item.domain,
    Sink = Action#item.type,
    Mid = Action#item.item,
    {Queue,Sink,Mid}.


