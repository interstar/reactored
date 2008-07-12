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

-module(atom_adaptor).
-author('awood@alan-woods-macbook.local').
-include("schema.hrl").
-include("system.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-export([render/3]).
-export([xml/1,feed/3,pretty_print/1]).

render(Title,Url,Item) when is_record(Item,item) ->
    {"application/atom+xml",xml(feed(Title,Url,[Item]))};
render(Title,Url,Items) when is_list(Items) andalso is_record(hd(Items),item) ->
    {"application/atom+xml",xml(feed(Title,Url,Items))};
render(Title,Url,Item) ->
    {"text/html",xml(feed(Title,Url,Item))}.
%% Todo what when is not item, i.e. attribute/s

pretty_print(Feed) ->
    io:format("~s~n",[lists:flatten(Feed)]).

xml(Feed) ->
    xmerl:export_simple(lists:flatten([Feed]), xmerl_xml).

feed(Title,Url,Items) ->
    {feed,[{xmlns,"http://www.w3.org/2005/Atom"}],
	    entries(Items,[{title,[Title]},{link,[h(Url)]},{id,[Url]}]) }.



entries(Items) ->
    entries(Items,[]).

entries(Items,Entries) when not is_record(hd(Items),item) ->
    lists:reverse([entry(Items)|Entries]);

entries([Item|Items],Entries) ->
    entries(Items,[entry(Item) |Entries]);
entries([],Entries) -> lists:reverse(Entries).

entry(It) when is_record(It,item) ->
    Content = case It#item.type of
		  "application/xml" ->
		      {Description,_} = xmerl_scan:string(It#item.description),
		      {content,[{type,It#item.type}],[xmerl_lib:simplify_element(Description)]};
		  Type -> 
		      {content,[{type,"text/plain"}],[It#item.description]}
	      end,
%    io:format("Content ~p~n",[Content]), 
    % or Res = It#item.uri -- (It#item.domain ++ ?DOMAINSEPERATOR)
    [_Domain,Res] = string:tokens(It#item.item,?DOMAINSEPERATOR),
    {entry,
     [
      {title,[It#item.title]},
      {link,[{href,h(It#item.uri)},{rel,"self"}],[]},
      {link,[{href,h(Res)},{rel,"alternate"}],[]},
      {id,[It#item.item]},
      {author,[{uri,[h(It#item.author)]}]},
      {published,[item:created(It)]},
      {updated,[item:modified(It)]},
      {summary,[s(markup_stripper:parse(It#item.description))]},
      Content
     ]};

% #xmlText{value=It#item.description}
% Content#xmlElement.content
% {content,[{type,It#item.type}],[Content]}
% #xmlText{value="<l/>"}

entry(It)  ->
    Updated = case pv("modified",It) of
		  N when is_integer(N) ->
		      N;
		  _ -> attribute:ts()
	      end,
    Author = pv("author",It),
    Item = pv("item",It),
    Uri = pv("uri",It),
    Title = pv("title",It),
    Description = pv("description",It),
    {entry,
     [
      {title,[Title]},
      {link,[{href,h(Uri)}],[]},
      {id,[Item]},
      {author,[{uri,[h(Author)]}]},
      {updated,[item:format_timestamp(Updated)]},
      {summary,[markup_stripper:parse(Description)]},
      {content,[{type,"text/xml"}],xtrib(It)}
      ]}.

xtrib(Attribs) ->
    xtrib(Attribs,[]).

xtrib([{Key,Val}|Attribs],Xml) ->
    xtrib(Attribs,[url(Key,Val)|Xml]);
xtrib([],Xml) -> 
    lists:reverse(Xml).

val(Val) when is_atom(Val)->
    atom_to_list(Val);
val(Val) when is_list(hd(Val))->
    string:join(Val,",");
val(Val) ->
    item:attribute(Val).

h("http://" ++ Url) ->
    "http://" ++ Url;
h(Url) ->
    "http://" ++ Url.

url(Key,Val) ->
    case item:is_url(Key) of
	false ->
	    {a,[{name,Key}],[val(Val)]};
	 true ->
	    {a,[{href,h(Key)}],[val(Val)]}
    end.


pv(X,Xs) ->
    case proplists:get_value(X,Xs) of
	undefined ->
	    [];
	V when is_list(V) -> 
	    V;
	N ->
	    N
    end.

s(S) ->
     lists:sublist(S, ?MAXCHARS) ++ "..".
