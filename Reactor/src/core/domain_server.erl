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
%%% File    : domain_server.erl
%%% Author  : Alan Wood <awood@alan-woods-macbook.local>
%%% Description : 
%%%
%%% Created : 15 May 2008 by Alan Wood <awood@alan-woods-macbook.local>
%%%-------------------------------------------------------------------
-module(domain_server).
-include("system.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0,start/0,stop/0]).
-export([match/6]).
-export([retrieve/0,create/3,retrieve/1,delete/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
% Configuration API
retrieve() ->
    gen_server:call(?MODULE,{get}).
create(Domain,Matcher,Owner) ->
    gen_server:call(?MODULE,{put,Domain,Matcher,Owner}).
retrieve(Domain) ->
    gen_server:call(?MODULE,{get,Domain}).
delete(Domain) ->
    gen_server:call(?MODULE,{delete,Domain}).

% Operational match API
match(Actor,Service,Command,Domain,Resource,Params) ->
    gen_server:cast(?MODULE,{Actor,Service,Command,Domain,Resource,Params}).
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
% Domain handling
handle_call({get},_From,State) ->
    {reply, {ok,domain:retrieve()}, State};

handle_call({put,Domain,Matcher,Owner},_From,State) ->
    Reply = case domain:create(Domain,Matcher,Owner) of
		{atomic,ok} -> {ok,Domain};
		Error -> {error,error(Error)}
	    end,
    {reply, Reply, State};

handle_call({get,Domain},_From,State) ->
    {reply, {ok,domain:retrieve(Domain)}, State};

handle_call({delete,Domain},_From,State) ->
    Reply = case domain:delete(Domain) of
		{atomic,ok} -> {ok,Domain};
		Error -> {error,error(Error)}
	    end,
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
handle_cast({Actor,Service,Command,Domain,Resource,Params}, Matchers) ->
    %% handle the system pattern matching first
    case apply(get_matcher(Domain,Matchers),match,[Actor,Service,Command,Domain,Resource,Params]) of
	Messages when is_list(Messages) -> 
	    lists:foreach(fun(Message) -> queue(Domain,Message) end,Messages);
	Bad -> 
	    error({"Domain Matcher return error for " ++ Domain ++ ", could, be a missing matcher",Bad})% Matcher module error/ not loaded?
    end,
    {noreply, Matchers};

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
terminate(_Reason, Matchers) ->
    %% stop all matcherss and unload them
    lists:foreach(fun unload_matcher/1,ets:tab2list(Matchers)),
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
queue(Domain,{nomatch})->
    void;
queue(Domain,{Sink,Data})->
    case queue_server:add(?DOMAIN ++ ?CONTEXT ++ ?QUEUE,Sink,Data) of
	{ok,Xref} -> Xref;
	{error,Why} -> error({"Error putting on " ++ Domain ++ " queue",Why,{Sink,Data}})
    end;
queue(Domain,Unrecognised) ->
    error({"Bad " ++ Domain ++  "pattern match return, not recognised",Unrecognised}).

get_matcher(Domain,Matchers) ->
    %% Lookup domain in matchers dict
    case ets:lookup(Matchers,Domain) of
	[] -> % Not yet loaded so load it first, then add it to live matchers
	    Matcher = load_matcher(Domain),
	    ets:insert(Matchers,{Domain,Matcher}),
	    get_server(Matcher);
	[{_Dom,Matcher}] -> get_server(Matcher)
    end.

get_server(Matchername) -> % the matchers are OTP applications we need to call match on the application server
    list_to_atom(atom_to_list(Matchername) ++ "_server").

load_matcher(Domain) ->
    %% Start matcher OTP app, if module fails to load/start use error app instead
    Matcher = case domain:retrieve(Domain) of
		  {atomic,[{domain,_Dom,_Owner,undefined}]} -> ?ERRORMATCHER;
		  {atomic,[{domain,_Dom,_Owner,Match}]} -> Match;
		  _ -> ?ERRORMATCHER
	      end,
    io:format("About to load matcher ~p~n",[Matcher]),
    case application:load(Matcher) of
	ok -> 
	    start(Matcher);
	{error,{already_loaded,Matcher}} -> 
		   start(Matcher),
		   Matcher;
	{error,_Why} -> 
	    io:format("Matcher not loaded ~p~n",[Matcher]),
	    ?ERRORMATCHER %flag an error?
    end.

start(Matcher) ->
    case application:start(Matcher) of
	ok -> Matcher;
	{error,{already_started,matcher}} -> Matcher;
	{error,_Why} -> 
	    io:format("Matcher not started ~p~n",[Matcher]),
	    ?ERRORMATCHER %flag an error?
    end.

unload_matcher({_Doman,Matcher}) ->
    case application:stop(Matcher) of
	ok -> 
	    case application:unload(Matcher) of
		ok -> void;
		_  -> io:fwrite("Could not unload : ~p~n",[Matcher])
	    end;
	_  ->
	     io:fwrite("Could not stop : ~p~n",[Matcher])
    end.

error(Error) ->
    error_logger:error_msg("Domain server - Says Whoops ~p~n",[Error]),
    Error.
