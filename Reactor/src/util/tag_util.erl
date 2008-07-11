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
    T = hd(Tags),
    %lists:foldl(fun filter/2, [],lists:flatten(lists:map(fun find_tagged/1,tags(string:tokens(T,"+")))));
    case lists:member($ ,T) of
	true -> % OR tags (any tag match)
	    %lists:usort(lists:flatten(lists:map(fun find_tagged/1,tags(string:tokens(T,"+")))))
	    lists:merge(lists:map(fun find_tagged/1,tags(string:tokens(T," "))));
	_ -> % AND Tags (all tags match)
	    sets:to_list(sets:intersection(lists:map(fun(Ti) ->
						sets:from_list(find_tagged(Ti)) end,
					tags(string:tokens(T,"+")))))
    end.

retrieve(Tags,Author) ->
    lists:foldl(fun filter/2, [],lists:flatten(lists:map(fun(Tag)-> find_tagged(Tag,Author) end,tags(Tags)))).

save_tags(TagRecs) ->
    F = fun() -> lists:foreach(fun mnesia:write/1,TagRecs) end,
    case mnesia:transaction(F) of
	{atomic,_} -> 
	    {ok,tagrec_to_tags(TagRecs)};
	{error,Error} ->
	    {error,Error}
    end.

find_tagged(Tag) ->
    lists:sort(do(qlc:q([X#tags.lid || X <- mnesia:table(tags),
						X#tags.tag =:= Tag]))).

find_tagged([],Author) ->
    lists:sort(do(qlc:q([X#tags.lid || X <- mnesia:table(tags),
					 X#tags.author =:= Author])));
find_tagged(Tag,Author) ->
    lists:sort(do(qlc:q([X#tags.lid || X <- mnesia:table(tags),
					 X#tags.author =:= Author,
					 X#tags.tag =:= Tag]))).


tags(Tags) ->
    lists:map(fun string:to_lower/1,Tags).
				
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

tagrec_to_tags(TagRecs) ->
    tagrec_to_tags(TagRecs,[]).

tagrec_to_tags([{tags,_Id,_author,Tag,_lid}|TagRecs],Tags) ->
    tagrec_to_tags(TagRecs,[Tag|Tags]);
tagrec_to_tags([],Tags) -> lists:reverse(Tags).

filter({T,L},Results) ->
    case dupe(L,Results) of
	true ->
	    Results;
	_ ->
	    [{T,L}|Results]
    end.

dupe(L,[{_T,L}|_R]) ->
    true;
dupe(X,[{_T,_L}|R]) ->
    dupe(X,R);
dupe(_,[]) ->
    false.
