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

-module(tag_util).
-include("schema.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([add/3,delete/1,retrieve/1,retrieve/2,create_tags/0,reset_tags/0,indexes/0,indexes/1]).

add(Author,Sid,Tags) ->
    save_tags([{tags,uid(),Author,Tag,Sid} || Tag <- tags(Tags)]).

delete(Sid) ->
    F = fun() ->
		Locations = qlc:e(qlc:q([X#tags.id || 
					    X <- mnesia:table(tags), 
					    X#tags.lid =:= Sid])),
		lists:foreach(fun(L) -> mnesia:delete({tags,L}) end, Locations)
	end,
    mnesia:transaction(F).

retrieve(Tags) -> 
    lists:flatten(lists:map(fun find_tagged/1,tags(Tags))).

retrieve(Tags,Author) ->
    lists:flatten(lists:map(fun(Tag)-> find_tagged(Tag,Author) end,tags(Tags))).

save_tags(TagRecs) ->
    F = fun() -> lists:foreach(fun mnesia:write/1,TagRecs) end,
    mnesia:transaction(F),
    TagRecs.

find_tagged(Tag) ->
    do(qlc:q([{X#tags.lid,X#tags.tag} || X <- mnesia:table(tags),
						X#tags.tag =:= Tag])).
find_tagged(Tag,Author) ->
    do(qlc:q([{X#tags.lid,X#tags.tag} || X <- mnesia:table(tags),
					 X#tags.author =:= Author,
					 X#tags.tag =:= Tag])).

tags(Text) ->
    search_util:words(string:to_lower(Text)).
				
create_tags() ->
    mnesia:delete_table(tags),
    mnesia:create_table(tags,[{attributes, record_info(fields,tags)},{record_name,tags}]).

reset_tags() ->
    mnesia:clear_table(tags).

indexes() ->
    F = fun() -> 
		qlc:e(qlc:q([{X#tags.lid,X#tags.author,X#tags.tag} || X <- mnesia:table(tags)]))
	end,
    case mnesia:transaction(F) of
	{atomic,[]} ->
	     [];
	{atomic,Locations} ->
	    Locations
    end.

indexes(Author) ->
    F = fun() -> 
		qlc:e(qlc:q([{X#tags.lid,X#tags.author,X#tags.tag} || X <- mnesia:table(tags),X#tags.author =:= Author]))
	end,
    case mnesia:transaction(F) of
	{atomic,[]} ->
	     [];
	{atomic,Locations} ->
	    Locations
    end.

do(Q) ->
	F = fun() -> qlc:e(Q) end,
	{atomic,Val} = mnesia:transaction(F),
	Val.

uid() ->
    search_util:uid().
