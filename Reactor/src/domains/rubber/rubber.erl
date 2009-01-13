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

-module(rubber).
-export([start/1, stop/0, intercept/5, process/6]).

start(Cmd) ->
  spawn(fun() ->
    register(?MODULE, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, Cmd}, [{packet, 4}, use_stdio, exit_status, binary]),
    port_loop(Port)
  end). 


stop() -> ?MODULE ! stop.

intercept(Domain,Method,Path,Request,DocRoot) ->
    {Params,Actor} = case rest_helper:attributes_and_actor(Request,Method) of
			  {Attributes,{uri,Id}} ->
			      {Attributes,Id};
			  {Attributes,_} ->
			      {Attributes,""}
		      end,
    ?MODULE ! {intercept,self(),{list_to_binary(Actor),
				 list_to_binary(Domain), 
				 list_to_binary(Path),
				 list_to_binary(DocRoot)}},
    {ok,"intercepted"}.
%Method,
%params_to_binary(Params)

process(Actor,Service,Command,Domain,Resource,Params) ->
    ?MODULE ! {match,self(),{list_to_binary(Actor),
			     Service,
			     Command,
			     list_to_binary(Domain),
			     list_to_binary(Resource),
			     params_to_binary(Params)}},
    {matched}.

port_loop(Port) ->
    receive
	{match,_Caller,Params} ->
	    Data = term_to_binary(match,Params),
	    Port ! {self(),{command,Data}},
	    port_loop(Port);
	{intercept,Caller,{Actor,Domain,Path,DocRoot}} ->
	    Data = term_to_binary({intercept,Actor,Domain,Path,DocRoot}),
	    Port ! {self(),{command,Data}},
	    port_loop(Port);
	stop ->
	    Port ! {self(),close},
	    receive
		{Port,closed} -> exit(normal)
	    end;
	{'EXIT',Port,Reason} ->
	    exit({port_terminated,Reason})
    end.

params_to_binary(Params) ->
    [{list_to_binary(K),list_to_binary(V)} || {K,V} <- Params].

    

%% this matcher is called for all operations
%all(Actor,Service,Command,Domain,Resource,Params) ->
%	   {nomatch}.

%% This matcher is only called on read operations
%read(Actor,Service,Command,Domain,Resource,Params) ->
%	   {nomatch}.

%% This matcher is only called on write operations
%write(Actor,Service,Command,Domain,Resource,Params) ->
%	   {nomatch}.
