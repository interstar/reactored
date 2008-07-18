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

-module(survey).
-include("system.hrl").
-include("schema.hrl").
-export([all/6,read/6,write/6]).

%% this matcher is called for all operations
all(_Actor,_Service,_Command,_Domain,_Resource,_Params) ->
	   {nomatch}.

%% This matcher is only called on read operations
read(_Actor,_Service,_Command,_Domain,_Resource,_Params) ->
	   {nomatch}.

%% This matcher is only called on write operations
write(Actor,Service,Command,Domain,Resource,Params) ->
	   append(Actor,Service,Command,Domain,Resource,Params).

append(Actor,_Service,create,_Domain,_Resource,Params) ->
    case attribs(Params) of
	[] ->
	   {nomatch};
	Fields ->
	    {_,Xref} = proplists:lookup("xref",Params),
	    Data = [{"title", ?DOCROOT ++ "/survey.csv"},
		    {"description",string:join(Fields,",")},
		    {"author",Actor},
		    {"type","append"},
		    {"xref",Xref}],
	    [{append,Data}]
    end;
append(_Actor,_Service,_Command,_Domain,_Resource,_Params) ->
    {nomatch}.


attribs(Params) ->
    attribs(lists:reverse(lists:sort(Params)),[]).

attribs([{"_" ++ _K,V}|Params],Out) ->
   attribs(Params,["\"" ++ V ++ "\"" |Out]);
attribs([_P|Params],Out) ->
    attribs(Params,Out);
attribs([],Out) ->
    Out. 



