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
%%% File    : attribute_server.erl
%%% Author  : Alan Wood <awood@alan-woods-macbook.local>
%%% Description : 
%%%
%%% Created : 26 Feb 2008 by Alan Wood <awood@alan-woods-macbook.local>
%%%-------------------------------------------------------------------
-module(attribute_server).
-define(SERVER,attribs).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

%%====================================================================
%% API
-export([start/0,stop/0,update/2,create/3,delete/1,delete/2,delete/3,retrieve/1,retrieve/2,retrieve/3,q/1,q/2,graph/3,log_state/0]).
start() -> start_link().
stop() -> gen_server:call(?MODULE,stop).
    
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callback

update(Qitem, Attributes) -> %% Update only
     gen_server:call(?MODULE,{put,Qitem, Attributes}).
create(Domain, Item, Attributes) ->
     gen_server:call(?MODULE,{put,Domain, Item, Attributes}).
delete(Qitem) ->
    gen_server:call(?MODULE,{delete,Qitem}).
delete(Domain, Item) ->
    gen_server:call(?MODULE,{delete,Domain, Item}).
delete(Domain, Item, Attributes) ->
    gen_server:call(?MODULE,{delete,Domain, Item, Attributes}).
retrieve(Qitem) ->
    gen_server:call(?MODULE,{get,Qitem}).
retrieve(Domain, Item) ->
    gen_server:call(?MODULE,{get,Domain, Item}).
retrieve(Domain, Item, Attribute) ->
    gen_server:call(?MODULE,{get,Domain, Item, Attribute}).
q(Qx) ->
   gen_server:call(?MODULE,{q,Qx}). 
q(Domain, Attributes) ->
    gen_server:call(?MODULE,{q,Domain,Attributes}).
graph(Domain, Uri, Attributes) ->
    gen_server:call(?MODULE,{graph,Domain,Uri,Attributes}).
log_state() ->
    gen_server:call(?MODULE,{logstate}). 
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

handle_call({put,Qitem, Attributes},_From,State) ->
    Reply = case attribute:update(Qitem, Attributes) of
		{atomic,Xref} -> {ok,Xref};
		Error -> {error,error(Error)}
	    end,
    {reply, Reply, State};

handle_call({put,Domain, Item, Attributes},_From,State) ->
    Reply = case attribute:create(Domain, Item, Attributes) of
		{atomic,Xref} -> {ok,Xref};
		Error -> {error,error(Error)}
	    end,
    {reply, Reply, State};

handle_call({delete,Qitem},_From,State) ->
    Reply = case attribute:delete(Qitem) of
		{atomic,Xref} -> {ok,Xref};
		Error -> {error,error(Error)}
	    end,
    {reply, Reply, State};

handle_call({delete,Domain, Item},_From,State) ->
    Reply = case attribute:delete(Domain, Item) of
		{atomic,Xref} -> {ok,Xref};
		Error -> {error,error(Error)}
	    end,
    {reply, Reply, State};

handle_call({delete,Domain, Item,Attributes},_From,State) ->
    Reply = case attribute:delete(Domain, Item,Attributes) of
		{atomic,Xref} -> {ok,Xref};
		Error -> {error,error(Error)}
	    end,
    {reply, Reply, State};

handle_call({get,Qitem},_From,State) ->
    {reply, {ok,attribute:retrieve(Qitem)}, State};

handle_call({get,Domain,Item},_From,State) ->
    {reply, {ok,attribute:retrieve(Domain,Item)}, State};

handle_call({get,Domain,Item,Attribute},_From,State) ->
    {reply, {ok,attribute:retrieve(Domain,Item,Attribute)}, State};

handle_call({q,Qx},_From,State) ->
    {reply, {ok,attribute:do(Qx)}, State};

handle_call({q,Domain,Attributes},_From,State) ->
    {reply, {ok,attribute:q(Domain,Attributes)}, State};

handle_call({graph,Domain,Uri,Attributes},_From,State) ->
    {reply, {ok,attribute:graph(Domain,Uri,Attributes)}, State};

handle_call({logstate}, _From, State) -> 
    {reply,error(State),State};

handle_call(stop, _From, State) ->
    {stop,normal,stopped, State}.

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
    mnesia:stop(),
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
    error_logger:error_msg("Attribute server - Says Whoops ~p~n",[Error]),
    Error.
