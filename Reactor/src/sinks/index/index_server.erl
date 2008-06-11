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
%%% File    : index_server.erl
%%% Author  : Alan Wood <awood@alan-woods-macbook.local>
%%% Description : 
%%%
%%% Created : 29 May 2008 by Alan Wood <awood@alan-woods-macbook.local>
%%%-------------------------------------------------------------------
-module(index_server).
-include("schema.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([start/0,stop/0]).
-export([sink/1,profile/1,search/1,tagged/1,tagged/2,controls/2,controls/3,grant/3,revoke/3,inherit/3,delete_controls/1,get_index_id/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
sink(Action)->
    gen_server:call(?MODULE,{sink,Action}).
profile(Profile) ->
    gen_server:call(?MODULE,{profile,Profile}).
search(Text) ->
    gen_server:call(?MODULE,{search,Text}).
tagged(Tags) ->
    gen_server:call(?MODULE,{tagged,Tags}).
tagged(Tags,Author) ->
    gen_server:call(?MODULE,{tagged,Tags,Author}).
controls(Iuri,Uri) -> 
    gen_server:call(?MODULE,{controls,Iuri,Uri}).
controls(Iuri,Command,Lids) -> 
    gen_server:call(?MODULE,{controls,Iuri,Command,Lids}).
grant(Iuri,Uri,Commands) ->
    gen_server:call(?MODULE,{controls,grant,Iuri,Uri,Commands}).
revoke(Iuri,Uri,Commands) ->
    gen_server:call(?MODULE,{controls,revoke,Iuri,Uri,Commands}).
inherit(Iuri,Uri,Parent) ->
    gen_server:call(?MODULE,{controls,inherit,Iuri,Uri,Parent}).
delete_controls(Iuri) ->
    gen_server:call(?MODULE,{controls,delete,Iuri}).
    
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
handle_call({sink,Action}, _From, State) ->
    Result = case Action#item.title of
		 tag -> tag_util:add(get_index_id(Action#item.author),
				     Action#item.xref, % set by tagger domain, fetched from attribute_server for that item
				     Action#item.description), 
		     {error,"Tags not yet implemented"};
		 reindex -> 
		     % Domain based re-index, need to be careful with this, won't handle deleted indexes that may be left in the search index for that domain, although that shouldn't happen of course!
		     reindex_domain(Action#item.description);
		 delete -> 
		     %% need to make sure we differentiate attribute deletion/update from item deletions.
		     search_util:delete(Action#item.xref),
		     tag_util:delete(Action#item.xref), 
		     control_util:delete(Action#item.xref);
		 create ->
		     search_util:index(Action#item.xref,Action#item.description);
		 update ->
		     search_util:update(Action#item.xref,Action#item.description);
		 Command ->
		     {error,error("Index doesn't understand command " ++ atom_to_list(Command))}
    end,
    Reply = case Result of
	[_Indexed] -> ok; %Indexed
	{ok,_Sid} -> ok;%Indexed single action
	{atomic,ok} -> ok; %removed from index
	{error,Error} -> {error,Error}
    end,
    {reply, Reply, State};

handle_call({search,Text}, _From, State) ->
    Reply = search_util:search_words(Text),
    {reply, Reply, State};
handle_call({profile,Profile}, _From, State) ->
    Reply = control_util:q(identity,get_index_id(Profile)),
    {reply, Reply, State};
handle_call({tagged,Tags}, _From, State) ->
    Reply = tag_util:retrieve(Tags),
    {reply, Reply, State};
handle_call({tagged,Tags,Author}, _From, State) ->
    Reply = tag_util:retrieve(Tags,Author),
    {reply, Reply, State};
handle_call({controls,delete,Iuri}, _From, State) ->
    Reply = control_util:delete(identity,get_index_id(Iuri)),
    {reply, Reply, State};
handle_call({controls,Iuri,Uri}, _From, State) ->
    Reply = control_util:q(get_index_id(Iuri),get_index_id(Uri)),
    {reply, Reply, State};
handle_call({controls,Iuri,Command,Lids}, _From, State) ->
    Reply = control_util:q(get_index_id(Iuri),Command,Lids),
    {reply, Reply, State};
handle_call({controls,grant,Iuri,Uri,Commands}, _From, State) ->
    Reply = control_util:add(get_index_id(Iuri),get_index_id(Uri),Commands),
    {reply, Reply, State};
handle_call({controls,revoke,Iuri,Uri,Commands}, _From, State) ->
    Reply = control_util:remove(get_index_id(Iuri),get_index_id(Uri),Commands),
    {reply, Reply, State};
handle_call({controls,inherit,Iuri,Uri,Parent}, _From, State) ->
    Reply = control_util:inherit(get_index_id(Iuri),get_index_id(Uri),get_index_id(Parent)),
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
%% Todo good opportunity for Map reduce basic indexing
reindex_domain(Domain) ->
    case attribute_server:q(Domain,[{"status","all"}]) of
	{ok,Items} -> 
	    lists:map(fun(It) -> search_util:update(It#item.xref,It#item.item) end,Items);
	_  -> 
	    error("could not retrieve items from domain " ++ Domain),
	    []
    end.

error(Error) ->
    error_logger:error_msg("Index sinker server - Says Whoops ~p~n",[Error]),
    Error.

get_index_id(Uri) ->
    case attribute_server:retrieve({raw,Uri}) of
	{ok,[]} -> 
	    error({"Could not look up item : " ++ Uri}),
	    0;
	{ok,[It]} -> It#item.xref
    end.
