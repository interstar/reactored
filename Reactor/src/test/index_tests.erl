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

-module(index_tests).
-include_lib("eunit/include/eunit.hrl"). 

start() ->
	mnesia:start().

stop() ->
    mnesia:stop().

%%
%% Unit tests
%%
index_test_() ->
    {setup,
     fun index_test_setup/0,
     fun index_test_cleanup/1,
     {"Index Tests",
      [tag_test_cases(),control_test_cases()]
     }
    }.

index_test_setup() ->    
    start(),
    tag_util:create_tags(),
    tag_util:add("fred",1,"west wild cowboy horse"),
    tag_util:add("joe",2,"south west east north"),
    tag_util:add("fred",3,"donkey mule horse pony"),
    tag_util:add("joe",4,"ranch,herd,cowboy,horse"),

    control_util:create_control(),
    control_util:add(10,20,retrieve),
    {ok,Cid} = control_util:add(10,21,retrieve),
    control_util:add(Cid,delete),
    control_util:add(Cid,update),
    control_util:remove(Cid,update),
    control_util:add(11,20,retrieve),
    control_util:add(12,20,retrieve),
    {ok,Did} = control_util:add(14,20,retrieve),
    control_util:delete(Did),
    [""].

index_test_cleanup(_Cts) ->
    tag_util:reset_tags(),

    control_util:reset_control(),

    stop().

% TODO add test cases for search
tag_test_cases() ->
    {"Tagging Tests",
     {inorder,
      [	{"Tag index",
	?_assertMatch([{1,"fred","cowboy"},
		       {1,"fred","horse"},
		       {1,"fred","west"},
		       {1,"fred","wild"},
		       {2,"joe","east"},
		       {2,"joe","north"},
		       {2,"joe","south"},
		       {2,"joe","west"},
		       {3,"fred","donkey"},
		       {3,"fred","horse"},
		       {3,"fred","mule"},
		       {3,"fred","pony"},
		       {4,"joe","cowboy"},
		       {4,"joe","herd"},
		       {4,"joe","horse"},
		       {4,"joe","ranch"}],
		      lists:sort(tag_util:indexes()))},
	{"Tag retrieve 1",
	?_assertMatch([{1,"west"},{2,"west"}],
		      lists:sort(tag_util:retrieve("west")))},
       {"Tag retrieve 2",
	?_assertMatch([{1,"horse"},{3,"horse"},{4,"horse"}] ,
		      lists:sort(tag_util:retrieve("horse")))},
	 {"Users Tag retrieve 1",
	?_assertMatch([{1,"horse"},{3,"horse"}],
		      lists:sort(tag_util:retrieve("horse","fred")))},
	{"Users Tag retrieve 2",
	?_assertMatch([{4,"horse"}],
		      lists:sort(tag_util:retrieve("horse","joe")))},
	{"Tag delete",
	?_assertMatch({atomic,ok},
		      tag_util:delete(4))},
       {"Tag retrieve 2 post delete",
	?_assertMatch([{1,"horse"},{3,"horse"}] ,
		      lists:sort(tag_util:retrieve("horse")))},
	{"Users Tag retrieve 2 post delete",
	?_assertMatch([],
		      lists:sort(tag_util:retrieve("horse","joe")))},
	{"Tag delete",
	?_assertMatch({atomic,ok},
		      tag_util:delete(3))},
	{"Tag delete",
	?_assertMatch({atomic,ok},
		      tag_util:delete(2))},
	{"Tag delete",
	?_assertMatch({atomic,ok},
		      tag_util:delete(1))}
	
	
      ]}}.


control_test_cases() ->
    {"Control Tests",
     {inorder,
      [	
	{"Control query 1",
	?_assertMatch([retrieve],
		      control_util:q(10,20) )},
	{"Control query 2",
	?_assertMatch([delete,retrieve],
		      control_util:q(10,21) )},
	{"Control query 3",
	?_assertMatch([{control,_,10,20,[retrieve]},
		       {control,_,11,20,[retrieve]},
		       {control,_,12,20,[retrieve]}],
		      lists:sort(control_util:q(location,20)) )},
	{"Control query 4",
	?_assertMatch([{control,_,10,20,[retrieve]},
		       {control,_,10,21,[delete,retrieve]}],
		      lists:sort(control_util:q(identity,10)) )},
	{"Add update control",
	?_assertMatch({ok,_},
		      control_util:add(10,20,update) )},
	{"Query update control change",
	?_assertMatch([update,retrieve],
		      control_util:q(10,20) )},
	{"Add delete control",
	?_assertMatch({ok,_},
		      control_util:add(10,20,delete) )},
	{"Query delete control change",
	?_assertMatch([delete,update,retrieve],
		      control_util:q(10,20) )},
	{"Add create control",
	?_assertMatch({ok,_},
		      control_util:add(10,20,create) )},
	{"Query create control change",
	?_assertMatch([create,delete,update,retrieve],
		      control_util:q(10,20) )},
	{"Remove create control",
	?_assertMatch({ok,_},
		      control_util:remove(10,20,create) )},
	{"Query remove create control change",
	?_assertMatch([delete,update,retrieve],
		      control_util:q(10,20) )},
	{"Remove delete control",
	?_assertMatch({ok,_},
		      control_util:remove(10,20,delete) )},
	{"Query remove delete control change",
	?_assertMatch([update,retrieve],
		      control_util:q(10,20) )},
	{"Remove update control",
	?_assertMatch({ok,_},
		      control_util:remove(10,20,update) )},
	{"Query remove update control change",
	?_assertMatch([retrieve],
		      control_util:q(10,20) )},

	{"Add mulitiple controls",
	?_assertMatch({ok,_},
		      control_util:add(10,20,[update,create]) )},
	{"Query multiple control change",
	?_assertMatch([create,retrieve,update],
		      control_util:q(10,20) )},
	{"remove mulitiple controls",
	?_assertMatch({ok,_},
		      control_util:remove(10,20,[update,create]) )},
	{"Query multiple control removal",
	?_assertMatch([retrieve],
		      control_util:q(10,20) )},

	{"Query retrieve control for given Identity against location list",
	?_assertMatch([20,21],
		      control_util:q(10,retrieve,[20,21]) )},
	{"Query delete control for given Identity against location list",
	?_assertMatch([21],
		      control_util:q(10,delete,[21,22]) )},
	{"delete control record by identity id",
	?_assertMatch({ok,2},
		      control_util:delete(identity,10) )},
	{"delete control record by location id",
	?_assertMatch({ok,2},
		      control_util:delete(location,20) )}
      ]}}.
