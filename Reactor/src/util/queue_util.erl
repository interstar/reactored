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

-module(queue_util).
-include("system.hrl").
-include("schema.hrl").
-export([add/4,fetch/2,delete/1,update/2,id/0]).


fetch(Queue,Status) ->
    attribute_server:q(Queue, [{"status",atom_to_list(Status)}]).

add(Queue,Sink,Id,Data) ->
    attribute_server:create(Queue, pack_item(Sink,Id), pack_attributes(Sink,Id,Data)).

delete(Item) ->
    case attribute_server:delete(Item) of 
	{ok} -> ok;
	{error,Error} -> {error,Error}
    end.

update(Mid,Reason) ->
    attribute_server:update(Mid,[{"History",[Reason]}]). % Add update to queue item history

pack_item(Sink,Id) ->
    lists:flatten(io_lib:format("~s/~s/~B", [?SINKS,atom_to_list(Sink),Id])).

pack_attributes(Sink,Id,Data) ->
   [ {"uri",pack_item(Sink,Id)}|Data].

id() ->
    {Meg,Sec,Mic} = now(),
    Meg * 1000000000000 + Sec * 1000000 + Mic.



%% Redundant
transform(Items) ->
    transform(Items,[]).
transform([Item|Items],Sinks) ->
    [Sink,Aid] = string:tokens( Item#item.uri -- ?SINKS, "/"),
    transform(Items,[{Item#item.domain,Sink,Aid,Item}|Sinks]);
transform([],Sinks) ->
    Sinks.

%% update(Queue,Items) ->
%%     update(Queue,Items,[]).

%% update(Queue,[{Aid,Reason}|Items],Pending) ->
%%     case attribute_server:put(Queue,Aid,[{"History",[Reason]}]) of % Add update to queue item history
%% 	{ok} -> update(Queue,Items,[{Aid,Reason}|Pending]);
%% 	_ -> update(Queue,Items,Pending)
%%     end;
%% update(Queue,[],Pending) -> Pending.

