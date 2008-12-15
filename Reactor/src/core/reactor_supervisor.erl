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
-include("schema.hrl").
-include("system.hrl").
-behaviour(supervisor).

%% API
-export([start_link/0,start_link/1,start/0,start_in_shell/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(PORT, 8080).

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
    Configfile = case application:get_env(reactor, config_file) of
		     {ok,File} ->
			 File ++ ?CONFIG;
		     _ -> 
			 ?CONFIG
		 end,
    case file:consult(Configfile) of
	{ok,Configs} ->
	    Conf = hd(Configs),
	    io:format("~nInitialising from config file ~s~n", [Configfile]),
	    prep_store(),
	    prep_reactor(),
	    initialize_reactor(Conf);
	{error,enoent} ->
	    io:format("~nCould not find config file ~s~n", [Configfile]),
	    {error,"Could not find config file"};
	{error,Why} ->
	    io:format("~nCould not parse config file ~s~n", [Configfile]),
	    {error,Why};
	_ ->
	    {error,"Unknown config error"}
    end.



%%====================================================================
%% Internal functions
%%====================================================================
initialize_reactor(Conf) ->
    Ip = case os:getenv("REACTORED_IP") of false -> "0.0.0.0"; Any -> Any end,
    Port = case os:getenv("REACTORED_PORT") of 
	       false -> 
		   ?PORT; 
	       RP -> 
		   case string:to_integer(RP) of
		       {error,_} ->
			   ?PORT;
		       {P,_} ->
			   P
		   end
	   end,
    WebConfig = [{ip, Ip},
                 {port, Port},
                 {docroot, Conf#config.home ++ ?DOCROOT}],

    Storage = {storage,{attribute_server,start_link,[]},
	      permanent,2000,worker,[attribute_server]},
    Sinks = {sinks,{sink_server,start_link,[Conf#config.domain]},
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
    Config = {config,{config_server,start_link,[Conf]},
	      permanent,2000,worker,[config_server]},
    Web = {web,{rest_server,start,[WebConfig]},
	      permanent,2000,worker,[rest_server]},
    {ok,{{one_for_one,3,10}, [Storage,Sinks,Actions,Queues,Domains,Patterns,Identity,Actor,Config,Web]}}.

prep_store() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(domain,[{attributes, record_info(fields,domain)},
				{record_name,domain}]),
    mnesia:create_table(item,[{attributes, record_info(fields,item)},
			      {record_name,item}]),
    mnesia:create_table(attribute,[{attributes,record_info(fields,attribute)},
				   {record_name,attribute}]),
    mnesia:create_table(identity,[{attributes, record_info(fields,identity)},
				  {record_name,identity}]),
    mnesia:create_table(tags,[{attributes, record_info(fields,tags)},
			      {record_name,tags}]),
    mnesia:create_table(words,[{attributes, record_info(fields,words)},
			       {record_name,words}]),
    mnesia:create_table(control,[{attributes, record_info(fields,control)},
				 {record_name,control}]),
    mnesia:create_table(usession,[{attributes, record_info(fields,usession)},
				 {record_name,usession}]).

% We need to start some OTP Apps first
prep_reactor() ->
    application:load(crypto),
    application:start(crypto),
    application:load(index),
    application:start(index),
    application:load(inets),
    application:start(inets).
