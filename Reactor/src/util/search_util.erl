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

-module(search_util).
-include("schema.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([create_words/0,reset_words/0,add_words/2,retrieve/1,delete/1,update/2,words/1,all/0,index/1,index/2,prep/1,indexes/0,uid/0]).
-define(DOMAIN,"search.rel3.com").
-define(MINWORDSIZE,4).

excluded_words() ->
    ["nbsp","then"].

create_words() ->
    mnesia:delete_table(words),
    mnesia:create_table(words,[{attributes, record_info(fields,words)},{record_name,words}]).

reset_words() ->
    mnesia:clear_table(words).

add_words([],Id) ->
    error({"No words found in",Id});
add_words(Words,Id) ->
    add_words(lists:sort(Words),"",0,Id,[]).

add_words([Word|Words],Word,Count,Id,IWords) when Count > 0 ->
    add_words(Words,Word,Count + 1,Id,IWords);
add_words([Word|Words],Last,Count,Id,IWords) when Count > 0 ->
    add_words(Words,Word,1,Id,[{words,uid(),Last,Id,Count}|IWords]);
add_words([Word|Words],_Last,0,Id,IWords) ->
    add_words(Words,Word,1,Id,IWords);
add_words([],Last,Count,Id,IWords) ->
    save_words([{words,uid(),Last,Id,Count}|IWords]).

save_words(Words) ->
    F = fun() -> lists:foreach(fun mnesia:write/1,Words) end,
    mnesia:transaction(F),
    Words.

delete(Id) ->
    F = fun() ->
		Locations = qlc:e(qlc:q([X#words.id || 
					    X <- mnesia:table(words), 
					    X#words.lid =:= Id])),
		lists:foreach(fun(L) -> mnesia:delete({words,L}) end, Locations)
	   end,
    mnesia:transaction(F).

update(Sid,Url) ->
    delete(Sid),
    index(Sid,Url).

retrieve(Query) ->
    order_by_score(lists:sort(lists:flatten(lists:map(fun search_word/1,words(string:to_lower(Query)))))).
	
index([]) ->
    {error,"nothing to index"};

index(Sources) ->
    lists:map(fun({Sid,Url}) -> index(Sid,Url) end, Sources).

index(Sid,Url) ->
    add_words(words(prep(Url)),Sid),
    {ok,Sid}.

indexes() ->
    F = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(words)]))
	end,
    mnesia:transaction(F).	       

search_word(Word) ->
    do(qlc:q([{X#words.lid,X#words.score} || X <- mnesia:table(words),
						X#words.word =:= Word]) ).
 
order_by_score([Result|Results]) ->
    order(Results,Result,[]);
order_by_score([]) -> [].

order([{Id,Score}|Results],{Id,Sum},Chart)  ->
    order(Results,{Id,Score + Sum},Chart);
order([{Id,Score}|Results],{Lid,Sum},Chart) ->
    order(Results,{Id,Score},[{Sum,Lid}|Chart]);
order([],{Lid,Sum},Chart) ->
    lists:reverse(lists:sort([{Sum,Lid}|Chart])).

words(Text) ->
    word(Text,[],[]).

word([T|Text],Last,Words) when length(T) < 4 ->
    word(Text,Last,Words);
word([T|Text],[],Words) when T<$A orelse T>$Z,T<$a orelse T>$z ->
    word(Text,[],Words);
word([T|Text],Word,Words) when T<$A orelse T>$Z,T<$a orelse T>$z  ->
    word(Text,[],word_check(Word,Words));
word([T|Text],Word,Words) when T>$@,T<$[ ->
    word(Text,[T+32|Word],Words);
word([T|Text],Word,Words) ->
    word(Text,[T|Word],Words);
word([],Word,Words) ->
    word_check(Word,Words).

word_check(Word,Words) when length(Word) < ?MINWORDSIZE ->
    Words;
word_check(Word,Words) ->
    case lists:member(lists:reverse(Word),excluded_words()) of
	true ->
	     Words;
	false -> 
	    [lists:reverse(Word)|Words]
    end.


    

uid() -> 
    {Meg,Sec,Mic} = now(),
    Meg * 1000000000000 + Sec * 1000000 + Mic.

do(Q) ->
	F = fun() -> qlc:e(Q) end,
	{atomic,Val} = mnesia:transaction(F),
	Val.

all() ->
    do(qlc:q([X || X <- mnesia:table(words)])).

prep(Url) ->
    case fetch(Url) of
	{ok,Text} ->
	    markup_stripper:parse(Text);
	_ -> []
    end.

fetch("http://" ++ Url) -> 
    case http:request("http://" ++ Url) of
	{ok,{_Vs,_Headers,Body}} -> {ok,Body};
	{error,Why} -> {error,Why}
    end;
fetch("file://" ++ Url) -> % Local file 
    open(Url);
fetch(Url)-> % attribute server assumption/default
    %io:fwrite("Indexing : ~n" ++ Url),
    case attribute_server:retrieve(Url,basic) of 
	{ok,[]} -> 
	    {error,"Attribute storage request returned empty"};
	{ok,Items} -> 
	    Item = hd(Items),
	    %io:fwrite("Indexing content : " ++ Item#item.description),
	    {ok,Item#item.description ++ "/n"};
	_ -> {error,"Attribute storage request failed"}
    end.

defrag_url(Url) ->
    defrag_url(Url, []).
defrag_url([$#|Url],Domain) -> 
    {lists:reverse(Domain),Url};
defrag_url([C|Url],Domain) ->
    defrag_url(Url,[C|Domain]);
defrag_url([],Domain) ->
    {lists:reverse(Domain),""}.	    

open(Url) ->
    case fopen(Url) of
	{error, Why} -> [] ;
		T	-> T
    end.

fopen(F) ->
    case file:open(F, read) of
	{ok, S}	->
	    Text = line(S),
	    file:close(S),
	    Text;
	{error, Why} ->
	    {error, Why}
    end.
	
line(S) ->
    case io:get_line(S, '') of
	eof -> [];
	Line -> [Line | line(S)]
    end.


error(Error) ->
    error_logger:error_msg("Search utility module - Says Whoops ~p~n",[Error]),
    Error.
