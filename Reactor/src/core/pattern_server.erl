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
%%% File    : pattern_server.erl
%%% Author  : Alan Wood <awood@alan-woods-macbook.local>
%%% Description : 
%%%
%%% Created : 12 May 2008 by Alan Wood <awood@alan-woods-macbook.local>
%%%-------------------------------------------------------------------
-module(pattern_server).
-include("system.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0,start/0,stop/0]).
-export([process/6]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
start() -> start_link().
stop() -> gen_server:call(?MODULE,stop).

process(Actor,Service,Command,Domain,Resource,Params) ->
    gen_server:cast(?MODULE,{Actor,Service,Command,Domain,Resource,Params}).
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
handle_cast({Actor,Service,Command,Domain,Resource,Params}, State) ->
    %% handle the system pattern matching first
    case match(Actor,Service,Command,Domain,Resource,lists:sort(Params)) of
	Messages when is_list(Messages) -> 
	    lists:foreach(fun queue/1,Messages);
	Bad -> 
	    error({"Pattern system match return error",Bad})
    end,
    %% Next handle the domain specific pattern matching, TODO make this remote
    domain_server:match(Actor,Service,Command,Domain,Resource,lists:sort(Params)),
    {noreply, State};

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
queue({nomatch})->
    void;
queue({Sink,Data})->
    case queue_server:add(?SYSTEM,Sink,Data) of
	{ok,Xref} -> Xref;
	{error,Why} -> error({"Error putting on system queue",Why,{Sink,Data}})
    end;
queue(Unrecognised) ->
    error({"Bad system pattern match return, not recognised",Unrecognised}).

match(Actor,Service,Command,Domain,Resource,Params) ->
    lists:flatten([pattern_matcher:all(Actor,Service,Command,Domain,Resource,Params),
    matching(Actor,Service,Command,Domain,Resource,Params)]).

matching(Actor,Service,create,Domain,Resource,Params) ->
    policy:create(Actor,Service,create,Domain,Resource,Params),
    pattern_matcher:write(Actor,Service,create,Domain,Resource,Params);
matching(Actor,Service,Command,Domain,Resource,Params) when Command =:= update;Command =:= delete ->
    pattern_matcher:write(Actor,Service,Command,Domain,Resource,Params);
matching(Actor,Service,Command,Domain,Resource,Params) ->
    pattern_matcher:read(Actor,Service,Command,Domain,Resource,Params).

error(Error) ->
    error_logger:error_msg("Pattern server - Says Whoops ~p~n",[Error]),
    Error.

% Redundant
arrange(Params) ->
    arrange(lists:sort(Params),[]).
arrange([{_Key,Val}|Params],Values) ->
    arrange(Params,[Val|Values]);
arrange([],Values) ->
    Values.
