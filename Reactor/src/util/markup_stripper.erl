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

-module(markup_stripper).
-export([parse/1]).

parse(Markup) ->
    parse(" " ++ Markup,[],text) -- " ".

parse("script>" ++ MU,Text,filter) ->
    parse(MU,Text,text);
parse("script" ++ MU,Text,tag) ->
    parse(MU,Text,filter);
parse("</" ++ MU,Text,text) ->
    parse(MU,[$ |Text],tag);
parse([$<|MU],Text,text) ->
    parse(MU,Text,tag);
parse([$>|MU],Text,tag) ->
    parse(MU,Text,text);
parse([M|MU],Text,text) ->
    parse(MU,[M|Text],text);
parse([_|MU],Text,tag) ->
    parse(MU,Text,tag);
parse([_|MU],Text,filter) ->
    parse(MU,Text,filter);
parse([],[$ |Text],_) ->
    lists:reverse(Text);
parse([],Text,_) ->
    lists:reverse(Text).

