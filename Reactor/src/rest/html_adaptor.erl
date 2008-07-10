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

-module(html_adaptor).
-author('awood@alan-woods-macbook.local').
-include("schema.hrl").
-include("system.hrl").
-export([render/3]).
-export([outline/3,xml/1,lists/3,pretty_print/1]).

render(Title,Url,Item) when is_record(Item,item) ->
    {"text/html",xml(lists(Title,Url,[Item]))};
render(Title,Url,Items) when is_list(Items) andalso is_record(hd(Items),item) ->
    {"text/html",xml(lists(Title,Url,Items))};
render(Title,Url,Item) ->
    {"text/html",xml(outline(Title,Url,Item))}.

pretty_print(List) ->
    io:format("~s~n",[lists:flatten(List)]).

xml(List) ->
    xmerl:export_simple(lists:flatten([List]), xmerl_xml).

outline(Title,Url,Attribs) ->
    {html,[{xmlns,"http://www.w3.org/1999/xhtml"}],
     [{head,
       [{title,[Title]},
	{link,[{href,[Url]},{rel,["self"]}],[]}
       ]},
      {body,[], [
		 {h1,[Title]},
		 {'div',[{class,"item"}],attributes(Attribs)}]
      } 
     ]}.

attributes(Attribs) ->
    attributes(Attribs,[]).

attributes([Att|Attribs],Atts) ->
    attributes(Attribs,[attribute(Att)|Atts]);
attributes([],Atts) ->
    lists:reverse(Atts).

attribute({Key,Val}) ->
    case item:is_url(Key) of
	false ->
	    {'div',[{class,"attribute"}],[Key ++ " : " ,{a,[{name,Key}],[val(Val)]}]};
	 true ->
	    {'div',[{class,"attribute"}],["Tagged Link : "  ,{a,[{href,h(Key)}],[val(Val)]}]}
    end.

%% attribute({Key,Val}) ->  
%%     {dt,[Key,{dd,[val(Val)]}]}.
val(Val) when is_atom(Val)->
    atom_to_list(Val);
val(Val) when is_list(hd(Val))->
    string:join(Val,",");
val(Val) ->
    item:attribute(Val).

lists(Title,Url,Items) ->
    {html,[{xmlns,"http://www.w3.org/1999/xhtml"}],
     [{head,
       [{title,[Title]},
	{link,[{href,[Url]},{rel,["self"]}],[]}
       ]},
      {body,[],[
		 {h1,[Title]},
		 {'div',[{class,"items"}],entries(Items)}]
      } 
     ]}.

entries(Items) ->
    entries(Items,[]).

entries([Item|Items],Entries) ->
    entries(Items,[entry(Item)|Entries]);
entries([],Entries) -> 
    lists:reverse(Entries).

entry(It) when is_record(It,item) ->
    {'div',[{id,It#item.item},{class,"item"}],
     [{a,[{href,h(It#item.uri)},{title,val(It#item.title)},{type,It#item.type}],[val(It#item.title)]},
      {span,[", "]},
      {a,[{href,h(It#item.author)},{rel,"author"}],[" created"]},
      {span,[" on " ++ item:created(It)]},
      {'div',[s(markup_stripper:parse(It#item.description))]}      
     ]}.

h(Url) ->
    "http://" ++ Url.

s(S) ->
     lists:sublist(S, ?MAXCHARS) ++ "..".

