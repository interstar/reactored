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
%%% File    : config_server.erl
%%% Author  : Alan Wood <awood@awmb.local>
%%% Description : 
%%%
%%% Created : 10 Dec 2008 by Alan Wood <awood@awmb.local>
%%%-------------------------------------------------------------------
-module(config_server).
-include("schema.hrl").
-include("system.hrl").
-define(SERVER,?MODULE).

-behaviour(gen_server).

%% API
-export([domain/0,home/0,path/1]).
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%%====================================================================
%% API
%%====================================================================
domain() ->
    gen_server:call(?MODULE,domain).
home() ->
    gen_server:call(?MODULE,home).
path(Res) ->
    gen_server:call(?MODULE,{path,Res}).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Conf) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Conf], []).

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
init([Conf]) ->
    io:format("~p starting~n",[?MODULE]),
    %io:format("with config ~n~p~n",[Conf]),
    config:load_domain(Conf),
    {ok, Conf}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(domain, _From, Conf) ->
    Reply = Conf#config.domain,
    {reply, Reply, Conf};
handle_call(home, _From, Conf) ->
    Reply = Conf#config.home,
    {reply, Reply, Conf};
handle_call({path,Res}, _From, Conf) ->
    Home = Conf#config.home,
    Reply = case Res of
		docroot ->
		    Home ++ ?DOCROOT ++ "/";
		audit ->
		    Home ++ ?AUDITFILE;
		_ ->
		    Home
		end,
    {reply, Reply, Conf};
handle_call(_Request, _From, Conf) ->
    Reply = ok,
    {reply, Reply, Conf}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, Conf) ->
    {noreply, Conf}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, Conf) ->
    {noreply, Conf}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _Conf) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, Conf, _Extra) ->
    {ok, Conf}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

