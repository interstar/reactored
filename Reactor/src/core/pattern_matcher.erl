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

-module(pattern_matcher).
-include("system.hrl").
-export([all/6,read/6,write/6]).

all(Actor,Service,autherror,Domain,Resource,Params) ->
    error_logger:error_msg("Pattern Match - Authentication error ~p~n",[{Actor,Service,autherror,Domain,Resource,Params}]),
	{nomatch};
%% this matcher is called for all operations
all(_Actor,_Service,_Command,_Domain,_Resource,_Params) ->
	   {nomatch}.

%% This matcher is only called on read operations
read(_Actor,_Service,_Command,_Domain,_Resource,_Params) ->
	   {nomatch}.

%% This matcher is only called on write operations
write(Actor,Service,Command,Domain,Resource,Params) ->
    [
     audit(Actor,Service,Command,Domain,Resource,Params), % Audit all changes
     index(Actor,Service,Command,Domain,Resource,Params) % Index all changes
    ].
    

audit(Actor,Service,Command,Domain,Resource,Params) ->
    Data = [{"title",Resource},
	    {"description", string:join([Domain,atom_to_list(Service),atom_to_list(Command),Resource,attribute:params_to_string(",",Params)],",") },
	    {"author",Actor},
	    {"type","audit"}],
   {audit,Data}.

index(Actor,_Service,Command,Domain,Resource,Params) ->
    Qitem = case Domain of
		[] -> 
		    Resource;
		_ ->
		    attribute:item_id(Domain,Resource)
	    end,
    {_,Xref} = proplists:lookup("xref",Params),
    Data = [{"title",atom_to_list(Command)},
	    {"description",Qitem},
	    {"author",Actor},
	    {"type","index"},
	    {"xref",Xref}],
   {index,Data}.

