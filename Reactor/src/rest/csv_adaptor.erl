%  Copyright (C) 2008 Alan Wood
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

-module(csv_adaptor).
-author('awood@alan-woods-macbook.local').
-include("schema.hrl").
-export([render/3]).

render(_Title,_Url,Item) when is_record(Item,item)->
    {"text/plain",lines([Item])};
render(_Title,_Url,Items) when is_list(Items)  andalso is_record(hd(Items),item)  ->
    {"text/plain",lines(Items)};
render(_Title,_Url,Items) ->
    {"text/plain",attributes(Items)}.

lines(It) when not is_record(hd(It),item) ->
    attributes(It);
lines(Items) ->
    lists:flatten(lists:map(fun line/1,Items)).

line(It) when is_record(It,item) ->
   lists:flatten(io_lib:format("\"~s\",\"~s\",\"~s\",\"~s\",\"~s\",\"~s\",\"~s\",\"~s\",\"~s\",\"~s\"~n", [
		val(It#item.item),
		val(It#item.uri),
		item:created(It),
		item:modified(It),
		val(It#item.domain),
		val(It#item.title),
		val(markup_stripper:parse(It#item.description)),
		val(It#item.author),
		val(It#item.type),
		val(It#item.status) 									   ])).


attributes(Attribs) ->
    attributes(Attribs,[]).

attributes([{_Key,Val}|Attribs],Values) ->
    attributes(Attribs,[val(Val)|Values]);
attributes([],Values) -> 
    string:join(Values,",").

val(Val) when is_list(hd(Val))->
    string:join(Val,",");
val(Val) ->
    item:attribute(Val).

%% Todo this should be generalised to include all attributes, use item:asttrib calls to handle types etc..
