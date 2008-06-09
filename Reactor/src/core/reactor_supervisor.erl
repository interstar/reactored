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
%%% File    : reactor_supervisor.erl
%%% Author  : Alan Wood <awood@alan-woods-macbook.local>
%%% Description : 
%%%
%%% Created : 22 May 2008 by Alan Wood <awood@alan-woods-macbook.local>
%%%-------------------------------------------------------------------
-module(reactor_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/0,start_link/1,start/0,start_in_shell/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start() ->
    spawn(fun() ->
		  supervisor:start_link({local, ?SERVER}, ?MODULE, _Arg = [])
	  end).

start_in_shell() ->
    {ok,Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, _Arg = []),
    unlink(Pid).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).
start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    Storage = {storage,{attribute_server,start_link,[]},
	      permanent,2000,worker,[attribute_server]},
    Sinks = {sinks,{sink_server,start_link,[]},
	      permanent,2000,worker,[sink_server]},
    Actions = {actions,{action_server,start_link,[]},
	      permanent,2000,worker,[action_server]},
    Queues = {queues,{queue_server,start_link,[]},
	      permanent,2000,worker,[queue_server]},
    Domains = {domains,{domain_server,start_link,[]},
	      permanent,2000,worker,[domain_server]},
    Patterns = {patterns,{pattern_server,start_link,[]},
	      permanent,2000,worker,[pattern_server]},
    Identity = {identity,{identity_server,start_link,[]},
	      permanent,2000,worker,[identity_server]},
    Actor = {actors,{actor_server,start_link,[]},
	      permanent,2000,worker,[actor_server]},
    {ok,{{one_for_one,3,10}, [Storage,Sinks,Actions,Queues,Domains,Patterns,Identity,Actor]}}.

%%====================================================================
%% Internal functions
%%====================================================================
