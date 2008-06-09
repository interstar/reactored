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

-module(storage_tests).
-include("schema.hrl").
-include("system.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([setup_database/0,start/0,stop/0,reset/0,reset_tables/0]).

% TODO make these unit test no destructive. at the momemnt the kill the entire store. they should only effect a 'testdomain' and its items clearing up after itself

setup_database() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:delete_table(domain),
    mnesia:delete_table(item),
    mnesia:delete_table(attribute),
    mnesia:create_table(domain,[{attributes, record_info(fields,domain)},{record_name,domain}]),
    mnesia:create_table(item,[{attributes, record_info(fields,item)},{record_name,item}]),
    mnesia:create_table(attribute,[{attributes,record_info(fields,attribute)},{record_name,attribute}]),
    mnesia:stop().

start() ->
	mnesia:start(),
	mnesia:wait_for_tables([domain,item,attribute],20000).

stop() ->
    mnesia:stop().

reset() ->
    stop(),
    setup_database(),
    start().

reset_tables() ->
	mnesia:clear_table(domain),
	mnesia:clear_table(item),
	mnesia:clear_table(attribute).

%%
%% Unit tests
%%
cache_test_() ->
    {setup,
     fun cache_test_setup/0,
     fun cache_test_cleanup/1,
     {"Cache Tests",
      [write_test_cases()]
     }
    }.

cache_test_setup() ->
    setup_database(),
    start(),
    [""].

cache_test_cleanup(_Cts) ->
    reset_tables(),
    stop().

%% TODO add Graph function unit test
% TODO add test cases for new qualified item updates/deletes & gets
write_test_cases() ->
    {"Write Tests",
     {inorder,
      [{"Create Domain", 
	?_assertMatch({atomic,ok} ,
		      domain:create("testdomain",?ERRORMATCHER))},
       {"Read Domain",
	?_assertMatch(["testdomain"] ,
		      domain:retrieve())},
       {"Create Item", 
	?_assertMatch({atomic,_xref} ,
		      attribute:create("testdomain",
				"testitem", 
				[{"created",123456789},
				 {"modified",123456789},
				 {"title",["Test item"]},
				 {"description",["Test Item Description"]},
				 {"author",["identity.rel3.com/folknology"]},
				 {"type",["text"]},
				 {"status",["live"]},
				 {"users",["identity.rel3.com/folknology"]},
				 {"groups",["identity.rel3.com/groups/contributors"]},
				 {"category",["testcat"]},
				 {"tags",["test","unit"]}
				]))},
       {"Create Item Read created",
	?_assertMatch({"created",123456789} ,
		      attribute:retrieve("testdomain", "testitem",["created"]))},
       {"Create Item Read Description",
	?_assertMatch({"description",["Test Item Description"]} ,
		      attribute:retrieve("testdomain", "testitem",["description"]))},
       {"Create Item Read Title",
	?_assertMatch({"title",["Test item"]} ,
		      attribute:retrieve("testdomain", "testitem",["title"]))},
       {"Create Item Read author",
	?_assertMatch({"author",["identity.rel3.com/folknology"]} ,
		      attribute:retrieve("testdomain", "testitem",["author"]))},
       {"Create Item Read Type",
	?_assertMatch({"type",["text"]} ,
		      attribute:retrieve("testdomain", "testitem",["type"]))},
       {"Replace Item Attribute", 
	?_assertMatch({atomic,_xref} ,
		      attribute:create("testdomain",
				"testitem",
				[{"status",["Testing"],true}]))},
       {"Replace Item Attribute Read",
	?_assertMatch({"status",["Testing"]} ,
		      attribute:retrieve("testdomain", "testitem",["status"]))},
       {"Replace Attribute", 
	?_assertMatch({atomic,_xref} ,
		      attribute:create("testdomain",
				"testitem",
				[{"category",["Tested"],true}]))},
       {"Replace Attribute Read",
	?_assertMatch({"category",["Tested"]} ,
		      attribute:retrieve("testdomain", "testitem",["category"]))},
       {"Append Attribute", 
	?_assertMatch({atomic,_xref} ,
		      attribute:update("testdomain|testitem",
				[{"tags",["tested"]}]))},
       {"Append Attribute Read",
	?_assertMatch({"tags",["tested","test","unit"]} ,
		      attribute:retrieve("testdomain", "testitem",["tags"]))},

       {"Append Item Attribute", 
	?_assertMatch({atomic,_xref} ,
		      attribute:create("testdomain",
				"testitem",
				[{"groups",["identity.rel3.com/groups/public"]}]))},
       {"Append Item Attribute Read",
	?_assertMatch({"groups",["identity.rel3.com/groups/public","identity.rel3.com/groups/contributors","identity.rel3.com/groups/administrators"]} ,
		      attribute:retrieve("testdomain", "testitem",["groups"]))},
       {"Append Item Multiple Attribute", 
	?_assertMatch({atomic,_xref} ,
		      attribute:create("testdomain",
				"testitem",
				[{"users",["identity.rel3.com/groups/friend1","identity.rel3.com/groups/friend2"]}]))},
       {"Append Item Multiple Attribute Read",
	?_assertMatch({"users",["identity.rel3.com/groups/friend2","identity.rel3.com/groups/friend1","identity.rel3.com/folknology","identity.rel3.com/admin"]} ,
		      attribute:retrieve("testdomain", "testitem",["users"]))},
       {"Create Attribute",
	?_assertMatch({atomic,_xref} ,
		      attribute:create("testdomain",
				"testitem",
				[{"links",["http://www.rel3.com/bookmarks/1","http://www.rel3.com/bookmarks/2"]}]))},
       {"Create Attribute Read",
	?_assertMatch({"links",["http://www.rel3.com/bookmarks/1","http://www.rel3.com/bookmarks/2"]} ,
		      attribute:retrieve("testdomain", "testitem",["links"]))},
       {"Create Integer Attribute", 
	?_assertMatch({atomic,_xref} ,
		      attribute:create("testdomain",
				"testitem",
				[{"marks",[99]}]))},
       {"Create Integer Attribute Read",
	?_assertMatch({"marks",[99]} ,
		      attribute:retrieve("testdomain", "testitem",["marks"]))},
       {"Append Integer Attribute", 
	?_assertMatch({atomic,_xref} ,
		      attribute:create("testdomain",
				"testitem",
				[{"marks",[88]}]))},
       {"Append Integer Attribute Read",
	?_assertMatch({"marks",[88,99]} ,
		      attribute:retrieve("testdomain", "testitem",["marks"]))},
       {"Replace Integer Attribute", 
	?_assertMatch({atomic,_xref} ,
		      attribute:create("testdomain",
				"testitem",
				[{"marks",[100],true}]))},
       {"Replace Integer Attribute Read",
	?_assertMatch({"marks",[100]} ,
		      attribute:retrieve("testdomain", "testitem",["marks"]))},
       {"Delete Attribute",
	?_assertMatch({atomic,_xref} , 
		      attribute:delete("testdomain",
				   "testitem",["links"]))},
       {"Delete Attribute Read",
	?_assertMatch([] ,
		      attribute:retrieve("testdomain", "testitem",["links"]))},
       {"Delete Item",
	?_assertMatch({atomic,_xref} ,
		      attribute:delete("testdomain", "testitem"))},
       {"Delete Item Read",
	?_assertMatch([] ,
		      attribute:retrieve("testdomain", "testitem"))},
       {"Delete Domain",
	?_assertMatch({atomic,ok} ,
		      domain:delete("testdomain"))},
       {"Delete Domain Read",
	?_assertMatch([] , domain:retrieve())}
      ]}}.


