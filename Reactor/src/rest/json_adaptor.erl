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

-module(json_adaptor).
-author('awood@alan-woods-macbook.local').
-include("schema.hrl").
-export([render/3]).

render(_Title,_Url,Items) when is_list(Items)  andalso is_record(hd(Items),item) ->
    {"application/json",json(Items)};
render(_Title,_Url,Item) when is_record(Item,item) ->
    {"application/json",json([Item])};
render(_Title,_Url,Attributes) ->
    Item = {struct,[{K,list_to_binary(val(V))} || {K,V} <- Attributes]},
    {"application/json",mochijson2:encode({struct,[{<<"items">>,[Item]}]})}.

json(Items) when not is_record(hd(Items),item) ->
    mochijson2:encode({struct,Items});
json(Items) ->
    json(Items,[]).

json([It|Items],Jsobj) ->
    json(Items,[item_to_json_obj(It)|Jsobj]);
json([],Jsobj) ->
     mochijson2:encode({struct,[{<<"items">>,Jsobj}]}).

item_to_json_obj(It) when is_record(It,item) ->
    {struct,[{item,list_to_binary(It#item.item)},
	     {uri,list_to_binary(It#item.uri)},
	     {created,list_to_binary(item:created(It))},
	     {modified,list_to_binary(item:modified(It))},
	     {domain,list_to_binary(It#item.domain)},
	     {title,list_to_binary(It#item.title)},
	     {description,list_to_binary(It#item.description)},
	     {author,list_to_binary(It#item.author)},
	     {type,list_to_binary(It#item.type)},
	     {status,list_to_binary(It#item.status)}
	    ]}.

val(Val) when is_atom(Val)->
    atom_to_list(Val);
val(Val) when is_list(hd(Val))->
    string:join(Val,",");
val(Val) ->
    item:attribute(Val).
