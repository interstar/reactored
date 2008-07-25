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

-module(item).
-author('awood@alan-woods-macbook.local').
-include("schema.hrl").
-export([created/1,modified/1,attribute/1,format_timestamp/1,is_url/1,iso_8601_fmt/1,chrono/1]).

created(It) when is_record(It,item) ->
    lists:flatten(iso_8601_fmt(int_to_datetime(It#item.created))).

modified(It) when is_record(It,item) ->
    lists:flatten(iso_8601_fmt(int_to_datetime(It#item.modified))).

format_timestamp(Int) ->
    lists:flatten(iso_8601_fmt(int_to_datetime(Int))).

attribute(Attribute) when is_integer(Attribute) ->
    io_lib:format("~B",[Attribute]);

attribute(Attribute) when is_float(Attribute) ->
    io_lib:format("~g",[Attribute]);

attribute(Attributes) when is_list(Attributes), is_list(hd(Attributes)) ->
   lists:flatten(Attributes);

attribute(Attribute) ->
    Attribute.

% returns {{Year,Month,Day},{Hour,Min,Sec}}
int_to_datetime(D) -> 
    Meg = D div 1000000000000,
    Rem = D rem 1000000000000,
    Sec = Rem div 1000000,
    Mic = Rem rem 1000000,
    calendar:now_to_datetime({Meg,Sec,Mic}).
    
iso_8601_fmt(DateTime) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
        [Year, Month, Day, Hour, Min, Sec]).

is_url("http://" ++ _Key) ->
    true;
is_url("ftp://" ++ _Key) ->
    true;
is_url(Key) ->
    case string:str(Key, ":") of
	0 -> 
	    case string:str(Key, "/") of
		0 -> 
		    false;
		_ ->
		    true
	    end;
	_ ->
	    true
    end.


chrono(Item)  when is_record(Item,item) -> 
    lists:sort(fun later/2,[Item]);
chrono(Items) when is_list(Items) andalso is_record(hd(Items),item) ->
    lists:sort(fun later/2,Items);
chrono(Other) -> 
    Other.

sort(Item)  when is_record(Item,item) -> 
    lists:sort(fun before/2,[Item]);
sort(Items) when is_list(Items) andalso is_record(hd(Items),item) ->
    lists:sort(fun before/2,Items);
sort(Other) -> 
    Other.

later(A,B) ->
    B#item.created < A#item.created.

before(A,B) ->
    A#item.created < B#item.created.

