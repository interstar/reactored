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

-module(attribute).
-include("schema.hrl").
-include("system.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([create/3,update/2,
	 delete/1,
	 delete/2,
	 delete/3,
	 retrieve/1,
	 retrieve/2,
	 retrieve/3,
	 q/1,
	 q/2,
	 q/3,
	 do/1,do/2,
	 move/3,
	 graph/3,
	 domain_from_qitem/1,
	 item_id/2,
	 parent/1,parent/2,
	 ts/0,today/0,params_to_string/2
	]).

%%
%% Spec: put(Domain::strng(),Item::string(),Attributes::list()) -.
%% This is the domain qialified version of Put (updates only)
create(Domain, Item, Attributes) -> 
    store_attributes(Domain,Item,Attributes).

%%
%% Spec: put(Qitem::string(),Attributes::list()) -.
%%
update(Qitem, Attributes) -> 
    store_attributes(Qitem,Attributes).

%%
delete(Qitem) ->
    remove_item(Qitem).

%%
delete(Domain, Item) ->
    remove_item(Domain,Item).

%%
delete(Domain, Item, Attributes) -> 
    remove_attributes(Domain,Item,Attributes).

%% Moves the intem within the heirarchy
move(Domain,Item,Uri) -> 
    [It] = mnesia:read({item,item_id(Domain,Item)}),
    Nit = It#item{uri=Uri}, 
    mnesia:write(Nit).

%%
retrieve(Qitem) when is_list(Qitem) -> 
    case retrieve(Qitem, basic) of
	[It] -> Attributes = [{"item",It#item.item},
			      {"uri",It#item.uri},
			      {"created",It#item.created},
			      {"modified",It#item.modified},
			      {"title",It#item.title},
			      {"description",It#item.description},
			      {"author",It#item.author},
			      {"type",It#item.type},
			      {"status",It#item.status},
			      {"users",It#item.users},
			      {"groups",It#item.groups},
			      {"ref",It#item.xref}],
		lists:flatten(Attributes,retrieve(Qitem, extended));
	[] -> retrieve(Qitem, extended)
    end;
retrieve({raw,Qitem}) when is_list(Qitem)  -> 
    retrieve(Qitem, raw);

retrieve({uri,Uri}) when is_list(Uri)  -> 
    retrieve(Uri, byuri);
retrieve({xref,Uri}) when is_list(Uri)  -> 
    retrieve(Uri, xref);
retrieve({item,Uri}) when is_list(Uri)  -> 
    retrieve(Uri, item);

retrieve(Ref)  -> 
    retrieve(Ref, byref).

retrieve(Ref, byref) -> 
    do(qlc:q([X || X <- mnesia:table(item),
		   X#item.xref =:= Ref]));
retrieve(Uri, byuri) -> 
    do(qlc:q([X || X <- mnesia:table(item),
		   X#item.uri =:= Uri]));
retrieve(Uri, xref) -> 
    do(qlc:q([X#item.xref || X <- mnesia:table(item),
		   X#item.uri =:= Uri]));
retrieve(Uri, item) -> 
    do(qlc:q([X#item.item || X <- mnesia:table(item),
		   X#item.uri =:= Uri]));


retrieve(Qitem, raw) -> 
    F = fun() ->
		mnesia:read({item,Qitem})
	end,
    case mnesia:transaction(F) of
	{atomic,X} -> X;
	Error -> {error,Error}
    end;
retrieve(Qitem, basic) -> 
    do(qlc:q([X || X <- mnesia:table(item),
		   X#item.item =:= Qitem]));

retrieve(Qitem, extended) -> 
    do(qlc:q([{Z#attribute.name,Z#attribute.value} || 
		 Z <- mnesia:table(attribute),
		 X <- mnesia:table(item),			     
		 X#item.item =:= Qitem,
		 Z#attribute.item =:= X#item.item]));
	
%%
retrieve(Domain, Item) -> 
    case retrieve(Domain, Item, basic) of
	[It] -> Attributes = [{"item",It#item.item},
			      {"uri",It#item.uri},
			      {"created",It#item.created},
			      {"modified",It#item.modified},
			      {"title",It#item.title},
			      {"description",It#item.description},
			      {"author",It#item.author},
			      {"type",It#item.type},
			      {"status",It#item.status},
			      {"users",It#item.users},
			      {"groups",It#item.groups},
			      {"ref",It#item.xref}],
		lists:flatten(Attributes,retrieve(Domain, Item, extended));
	[] -> retrieve(Domain, Item, extended)
    end.


retrieve(Domain, Item, basic) -> 
    do(qlc:q([X || X <- mnesia:table(item),
		   X#item.domain =:= Domain,
		   X#item.item =:= item_id(Domain,Item)]));

retrieve(Domain, Item, extended) -> 
    do(qlc:q([{Z#attribute.name,Z#attribute.value} || 
		 Z <- mnesia:table(attribute),
		 X <- mnesia:table(item),
		 X#item.domain =:= Domain,			     
		 X#item.item =:= item_id(Domain,Item),
		 Z#attribute.item =:= X#item.item]));

retrieve(Domain, Item, ["item"]) ->
    [It] = retrieve(Domain, Item, basic),
    {"item",It#item.item};
retrieve(Domain, Item, ["uri"]) ->
    [It] = retrieve(Domain, Item, basic),
    {"uri",It#item.item};
retrieve(Domain, Item, ["created"]) ->
    [It] = retrieve(Domain, Item, basic),
    {"created",It#item.created};
retrieve(Domain, Item, ["modified"]) ->
    [It] = retrieve(Domain, Item, basic),
    {"modified",It#item.modified};
retrieve(Domain, Item, ["title"]) ->
    [It] = retrieve(Domain, Item, basic),
    {"title",It#item.title};
retrieve(Domain, Item, ["description"]) ->
    [It] = retrieve(Domain, Item, basic),
    {"description",It#item.description};
retrieve(Domain, Item, ["author"]) ->
    [It] = retrieve(Domain, Item, basic),
    {"author",It#item.author};
retrieve(Domain, Item, ["type"]) ->
    [It] = retrieve(Domain, Item, basic),
    {"type",It#item.type};
retrieve(Domain, Item, ["status"]) ->
    [It] = retrieve(Domain, Item, basic),
    {"status",It#item.status};
retrieve(Domain, Item, ["users"]) ->
    [It] = retrieve(Domain, Item, basic),
    {"users",It#item.users};
retrieve(Domain, Item, ["groups"]) ->
    [It] = retrieve(Domain, Item, basic),
    {"groups",It#item.groups};
retrieve(Domain, Item, ["revision"]) ->
    [It] = retrieve(Domain, Item, basic),
    {"revision",It#item.revision};
retrieve(Domain, Item, ["sync"]) ->
    [It] = retrieve(Domain, Item, basic),
    {sync,It#item.sync};
retrieve(Domain, Item, ["xref"]) ->
    [It] = retrieve(Domain, Item, basic),
    {sync,It#item.xref};
	
%%	
retrieve(Domain, Item, [Attribute]) ->
    case do(qlc:q([{Z#attribute.name,Z#attribute.value} || 
		      Z <- mnesia:table(attribute),
		      X <- mnesia:table(item),
		      X#item.item =:= item_id(Domain,Item),
		      X#item.domain =:= Domain,
		      Z#attribute.item =:= X#item.item,
		      Z#attribute.name =:= Attribute ])) of
	[It] ->It;
	[] -> []
    end.

%% TODO this is now broken since domain changes etc..				retrieve(Domain, Item, basic) -> 			
q(Domain) -> 
	do(qlc:q([Z#attribute.id || 
		     Z <- mnesia:table(attribute),
		     X <- mnesia:table(item),
		     X#item.domain =:= Domain,
		     Z#attribute.item =:= X#item.item ])).

q(Domain,[]) ->
    q(Domain,[{"status","all"}]);

q(Domain,[{"status","all"}]) ->
    do(qlc:q([X || X <- mnesia:table(item),
		   X#item.domain =:= Domain]));
q(Domain,[{"status",Status}]) ->
    do(qlc:q([X || X <- mnesia:table(item),
		   X#item.domain =:= Domain,
		   X#item.status =:= [Status]]));
q(Domain,[{"from",From}]) ->
    After = date_to_integer(From),
    do(qlc:q([X || X <- mnesia:table(item),
		   X#item.domain =:= Domain,
		   X#item.created >= After]));
q(Domain,[{"from",From},{"to",To}]) ->
    After = date_to_integer(From),
    Before = date_to_integer(To),
    do(qlc:q([X || X <- mnesia:table(item),
		   X#item.domain =:= Domain,
		   X#item.created >= After,
		   X#item.created =< Before]));

q(Domain,[_]) ->
    q(Domain,[{"status","all"}]).

q(Domain, null, Options) -> {error,"Not Implemented"};
q(Domain, qExp, Options) -> {error,"Not Implemented"}.


%% Graph functions
graph(Domain,Uri,[{"related","all"}]) ->
    Item = item_id(Domain,Uri),
    F = fun() -> 
		lists:sort(lists:foldl(fun(I,Acc) -> related(Item,I,Acc) end,[],qlc:e(items(Domain))))
    end,
    mnesia:transaction(F);
%% Useless as is
graph(Domain,Uri,[{"status",Status}]) -> 
    % childrren of Uri status = status
    Item = item_id(Domain,Uri),
    F = fun() -> 
		lists:sort(lists:foldl(fun(I,Acc) -> children(Item,{status,Status},I,Acc) end,[],qlc:e(items(Domain))))
    end,
    mnesia:transaction(F);
graph(Domain,Uri,[{"from",From}]) -> 
    % childrren of Uri status = status
    After = date_to_integer(From),
    Item = item_id(Domain,Uri),
    F = fun() -> 
		lists:sort(lists:foldl(fun(I,Acc) -> children(Item,{modified,After},I,Acc) end,[],qlc:e(items(Domain))))
    end,
    mnesia:transaction(F);
graph(Domain,Uri,[{"from",From},{"to",To}]) -> 
    % childrren of Uri status = status
    After = date_to_integer(From),
    Before = date_to_integer(To),
    Item = item_id(Domain,Uri),
    F = fun() -> 
		lists:sort(lists:foldl(fun(I,Acc) -> children(Item,{modified,Before,After},I,Acc) end,[],qlc:e(items(Domain))))
    end,
    mnesia:transaction(F);
graph(Domain,Uri,[]) -> 
    % all children of uri
    Item = item_id(Domain,Uri),
    F = fun() -> 
		lists:sort(lists:foldl(fun(I,Acc) -> children(Item,I,Acc) end,[],qlc:e(items(Domain))))
    end,
    mnesia:transaction(F);
graph(Domain,Uri,Attributes) -> 
    % relative graph Not supported
    [].

%% native remote query
do(Q) ->
	F = fun() -> qlc:e(Q) end,
	{atomic,Val} = mnesia:transaction(F),
	Val.


do(_Domain,Q) -> % for when we fragment by domain
	F = fun() -> qlc:e(Q) end,
	{atomic,Val} = mnesia:transaction(F),
	Val.


remove_attributes(Domain,Item,Attributes) -> %% todo this should only return a single attribute now, so we may be able to optimise
    Qitem = item_id(Domain,Item),
    F = fun() ->
		Atts = lists:flatten(lists:map(fun(A) -> qlc:e(retrieve_attributes_id_by_name(Qitem,A)) end, Attributes)),
		lists:foreach(fun(A) -> mnesia:delete(A) end, Atts),
		[It] = mnesia:read({item,Qitem}),
		It#item.xref
	end,
    mnesia:transaction(F).

remove_item(Domain,Item) ->
    Qitem = item_id(Domain,Item),
    remove_item(Qitem).

remove_item(Qitem) ->
    F = fun() ->
		Atts = qlc:e(query_attributes_by_item(Qitem)),
		lists:foreach(fun(A) -> mnesia:delete({attribute,A}) end, Atts),
		[It] = mnesia:read({item,Qitem}),
		mnesia:delete({item,Qitem}),
		It#item.xref
	end,
    mnesia:transaction(F).

retrieve_attributes_id_by_name(Domain,Item,Attribute) ->
    qlc:q([{attribute,Z#attribute.id} || 
	      Z <- mnesia:table(attribute),
	      X <- mnesia:table(item),
	      X#item.item =:= item_id(Domain,Item),
	      X#item.domain =:= Domain,
	      Z#attribute.item =:= X#item.item,
	      Z#attribute.name =:= Attribute ]).

retrieve_attributes_id_by_name(Qitem,Attribute) ->
    qlc:q([{attribute,Z#attribute.id} || 
	      Z <- mnesia:table(attribute),
	      X <- mnesia:table(item),
	      X#item.item =:= Qitem,
	      Z#attribute.item =:= X#item.item,
	      Z#attribute.name =:= Attribute ]).

retrieve_attributes_by_name(Domain,Item,Attribute) ->
    qlc:q([Z || Z <- mnesia:table(attribute),
		X <- mnesia:table(item),
		X#item.item =:= item_id(Domain,Item),
		X#item.domain =:= Domain,
		Z#attribute.item =:= X#item.item,
		Z#attribute.name =:= Attribute ]).

retrieve_attributes_by_name(Qitem,Attribute) ->
    qlc:q([Z || Z <- mnesia:table(attribute),
		X <- mnesia:table(item),
		X#item.item =:= Qitem,
		Z#attribute.item =:= X#item.item,
		Z#attribute.name =:= Attribute ]).

query_attributes_by_item(Domain,Item) ->
    qlc:q([Z#attribute.id || 
	      Z <- mnesia:table(attribute),
	      X <- mnesia:table(item),
	      X#item.domain =:= Domain,
	      X#item.item =:= item_id(Domain,Item),
	      Z#attribute.item =:= X#item.item ]).

query_attributes_by_item(Qitem) ->
    qlc:q([Z#attribute.id || 
	      Z <- mnesia:table(attribute),
	      X <- mnesia:table(item),
	      X#item.item =:= Qitem,
	      Z#attribute.item =:= X#item.item ]).

items(Domain) ->
    qlc:q([{X#item.item,X} || X <- mnesia:table(item),
			X#item.domain =:= Domain]).

% Item = item record
related(Uri,{Uri,Item},Items) -> % The item itself
    [{self,Item} | Items];
related(Uri,{Iuri,Item},Items) ->
    case string:str(Iuri,Uri) of
	1 -> 
	    [{child,Item} | Items]; % child of item
	Pos ->
	    case is_ancestor(Uri,Iuri) of
		true ->
		    [{ancestor,Item} | Items]; % ancestor of item
		false ->
		    Items % {void,Pos,Item} Neither ancestor or child. i.e. no relation
	    end
    end.

children(Uri,{Iuri,Item},Items) ->
    case string:str(Iuri,Uri) of
	1 -> 
	    [{child,Item} | Items]; % child of item
	Pos ->
	    Items
    end.

%% Todo need to test this record pattern matching usage!
children(Uri,{status,Status},{Iuri,#item{status=Status} = Item},Items) ->
    case string:str(Iuri,Uri) of
	1 -> 
	    [{child,Item} | Items]; % child of item
	Pos ->
	    Items
    end;

children(Uri,{modified,After},{Iuri,#item{modified=Modified} = Item},Items) when is_integer(After) andalso After =< Modified ->
    case string:str(Iuri,Uri) of
	1 -> 
	    [{child,Item} | Items]; % child of item
	Pos ->
	    Items
    end;

children(Uri,{modified,Before,After},{Iuri,#item{modified=Modified} = Item},Items) when is_integer(After) andalso After =< Modified andalso is_integer(Before) andalso Before >= Modified ->
    case string:str(Iuri,Uri) of
	1 -> 
	    [{child,Item} | Items]; % child of item
	Pos ->
	    Items
    end;

children(_Uri,_Attributes,{_Iuri,_Item},Items) -> % default not recognised returns empty
    Items.

is_ancestor(Uri,Iuri) -> % TODO need to check outside cases, are their exceptions to this
    case string:str(Uri,Iuri) of
	0 ->
	    false;
	_Pos ->
	    true
    end.

% Todo remove now redundant
ensure_item(Domain,Item,Now) ->   
    case mnesia:read({item,item_id(Domain,Item)}) of
	[] ->  mnesia:write(#item{item=item_id(Domain,Item),
				 uri=Domain ++ Item,
				 created=Now,
				 modified=Now,
				 domain=Domain,title=["New"],
				 description=["Created"],
				 author=[Domain ++ ?IDENTITIES ++ "/founder"],
				 type=["plain/text"],
				 status=["live"],
				 users=[Domain ++ ?IDENTITIES ++ "/founder"],
				 groups=["/founders"],
				 revision=0,
				 sync=cache,
				xref=Now});
	[_] -> []
    end.

write_item(Domain,Item,Now,Attribs) ->
    It = #item{item=item_id(Domain,Item),
				 uri=Domain ++ Item,
				 created=Now,
				 modified=Now,
				 domain=Domain,title=["New"],
				 description=["Created"],
				 author=[Domain ++ ?IDENTITIES ++ "/founder"],
				 type=["plain/text"],
				 status=["live"],
				 users=[Domain ++ ?IDENTITIES ++ "/founder"],
				 groups=["/founders"],
				 revision=0,
				 sync=cache,
				xref=Now},
    item_type(Attribs,It,item,[]).

item_type([{K,V,_Replace}|Attribs],It,Type,Atts) ->
    item_type(Attribs,It,Type,[{K,V,_Replace}|Atts]);
item_type([{"created",V}|Attribs],It,Type,Atts) ->
    item_type(Attribs,It#item{created=to_i(V)},Type,Atts);
item_type([{"modified",V}|Attribs],It,Type,Atts) ->
    item_type(Attribs,It#item{modified=to_i(V)},Type,Atts);
item_type([{"title",V}|Attribs],It,Type,Atts) ->
    item_type(Attribs,It#item{title=V},Type,Atts);
item_type([{"description",V}|Attribs],It,Type,Atts) ->
    item_type(Attribs,It#item{description=V},Type,Atts);
item_type([{"author",V}|Attribs],It,Type,Atts) ->
    item_type(Attribs,It#item{author=V},Type,Atts);
item_type([{"type",V}|Attribs],It,Type,Atts) ->
    item_type(Attribs,It#item{type=V},Type,Atts);
item_type([{"status",V}|Attribs],It,Type,Atts) ->
    item_type(Attribs,It#item{status=V},Type,Atts);
item_type([{"uri",V}|Attribs],It,Type,Atts) ->
    item_type(Attribs,It#item{uri=V},Type,Atts);
item_type([{"xref",V}|Attribs],It,Type,Atts) ->
    item_type(Attribs,It#item{xref=V},Type,Atts);
item_type([{"groups",V}|Attribs],It,Type,Atts) ->
    item_type(Attribs,It#item{groups=V},Type,Atts);
item_type([{K,V}|Attribs],It,Type,Atts) -> 
    item_type(Attribs,It,attribute,[{K,V}|Atts]);
item_type([],It,Type,Atts) ->
    {It,Type,Atts}.
					    

%% String representation of today
today() -> 
    {Meg,Sec,Mic} = now(),
    lists:flatten(io_lib:format("~B", [Meg * 1000000000000 + Sec * 1000000 + Mic])).

%% timestamp
ts() -> 
    {Meg,Sec,Mic} = now(),
    Meg * 1000000000000 + Sec * 1000000 + Mic.

store_attributes(Domain,Item,Attributes) ->
    Now = ts(),
    {It,Type,Atts} = write_item(Domain,Item,Now,Attributes),
    As = put_attributes(Atts),
    %io:fwrite("Attribs ~p~n",[Data]),
    case Atts of
	[] ->
	    mnesia:dirty_write(It),
	    {atomic,Now};
	_ ->
	    F = fun() ->
			mnesia:write(It),
			lists:foreach(fun(At) -> store_attribute(Domain,Item,At) end,As),
			Now
		end,
	    mnesia:transaction(F)
	end.

store_attributes(Qitem,Attributes) -> % updating only
    Data = put_attributes(Attributes),
    %io:fwrite("Attribs ~p~n",[Data]),
    F = fun() ->
	lists:foreach(fun(At) -> store_attribute(Qitem,At) end,Data),
 	[It] = mnesia:read({item,Qitem}),
	It#item.xref
    end,
    mnesia:transaction(F).

put_attributes(Attributes) ->
    put_attributes(Attributes,[{update,item,"modified",ts()}]).

put_attributes([{Name,Values}|Attributes],Data) ->
    put_attributes(Attributes,[{write,table(Name),Name,Values}|Data]);
put_attributes([{Name,Values,false}|Attributes],Data) ->
    put_attributes(Attributes,[{write,table(Name),Name,Values}|Data]);
put_attributes([{Name,Values,true}|Attributes],Data) ->
    put_attributes(Attributes,[{update,table(Name),Name,Values}|Data]);
put_attributes([],Data) -> Data.

%% Key attribute table lookups
table("item") -> item;
table("uri") -> item;
table("created") -> item;
table("modified") -> item;
table("title") -> item;
table("description") -> item;
table("author") -> item;
table("type") -> item;
table("status") -> item;
table("users") -> item;
table("groups") -> item;
table("xref") -> item;
table(_) -> attribute.

merge_attributes([Ats1],Ats2) ->
    [Ats1 | Ats2];
merge_attributes([A|Ats1],Ats2) ->
    merge_attributes(Ats1,[A|Ats2]).
    

%% TODO Attribute "description" - to add revision control rather than updating here diff new and old and add the diff to values (don't forget to increment the rev and add a mod date value) could also use sepearet function : store_attribute(Domain,Item,{revise,item,"description",Attributes} or maybe this can happen about attribute store using rev server! Also would proabably need a "modifiedby" attribute on item. Or maybe we should just add a revision attributte that records modified date, modifier and diff of all attributtes modefied)

% Key Attribute storage
store_attribute(Domain,Item,{write,item,"users",Attributes}) ->
    [It] = mnesia:read({item,item_id(Domain,Item)}),
    Nit = It#item{users=merge_attributes(Attributes,It#item.users)},
    mnesia:write(Nit);
store_attribute(Domain,Item,{write,item,"groups",Attributes}) ->
    [It] = mnesia:read({item,item_id(Domain,Item)}),
    Nit = It#item{groups=merge_attributes(Attributes,It#item.groups)}, 
    mnesia:write(Nit);

store_attribute(Domain,Item,{update,item,"created",Attributes}) when is_list(Attributes) ->
    [It] = mnesia:read({item,item_id(Domain,Item)}),
    Nit = It#item{created=to_i(Attributes)}, 
    mnesia:write(Nit);
store_attribute(Domain,Item,{update,item,"created",Attributes}) ->
    [It] = mnesia:read({item,item_id(Domain,Item)}),
    Nit = It#item{created=Attributes}, 
    mnesia:write(Nit);
store_attribute(Domain,Item,{update,item,"modified",Attributes}) when is_list(Attributes) ->
    [It] = mnesia:read({item,item_id(Domain,Item)}),
    Nit = It#item{modified=to_i(Attributes)}, 
    mnesia:write(Nit);
store_attribute(Domain,Item,{update,item,"modified",Attributes}) ->
    [It] = mnesia:read({item,item_id(Domain,Item)}),
    Nit = It#item{modified=Attributes}, 
    mnesia:write(Nit);
store_attribute(Domain,Item,{update,item,"title",Attributes}) ->
    [It] = mnesia:read({item,item_id(Domain,Item)}),
    Nit = It#item{title=Attributes}, 
    mnesia:write(Nit);
store_attribute(Domain,Item,{update,item,"description",Attributes}) ->
    [It] = mnesia:read({item,item_id(Domain,Item)}),
    Nit = It#item{description=Attributes}, 
    mnesia:write(Nit);
store_attribute(Domain,Item,{update,item,"author",Attributes}) ->
    [It] = mnesia:read({item,item_id(Domain,Item)}),
    Nit = It#item{author=Attributes}, 
    mnesia:write(Nit);
store_attribute(Domain,Item,{update,item,"type",Attributes}) ->
    [It] = mnesia:read({item,item_id(Domain,Item)}),
    Nit = It#item{type=Attributes}, 
    mnesia:write(Nit);
store_attribute(Domain,Item,{update,item,"status",Attributes}) ->
    [It] = mnesia:read({item,item_id(Domain,Item)}),
    Nit = It#item{status=Attributes}, 
    mnesia:write(Nit);
store_attribute(Domain,Item,{update,item,"users",Attributes}) ->
    [It] = mnesia:read({item,item_id(Domain,Item)}),
    Nit = It#item{users=Attributes}, 
    mnesia:write(Nit);
store_attribute(Domain,Item,{update,item,"groups",Attributes}) ->
    [It] = mnesia:read({item,item_id(Domain,Item)}),
    Nit = It#item{groups=Attributes}, 
    mnesia:write(Nit);
store_attribute(Domain,Item,{update,item,"xref",Attributes}) ->
    [It] = mnesia:read({item,item_id(Domain,Item)}),
    Nit = It#item{xref=Attributes}, 
    mnesia:write(Nit);
%% If not captured by above must actually either be an update instead of a write (As in item just created, not really updated) or an unknown key attribute. All other attributes are updateable only
store_attribute(Domain,Item,{write,item,Name,Attributes}) ->
    store_attribute(Domain,Item,{update,item,Name,Attributes});

% Attribute storage
store_attribute(Domain,Item,{write,attribute,Name,Attributes}) ->
    mnesia:write(#attribute{id=next_oid(Domain,Item),item=item_id(Domain,Item),name=Name,value=Attributes});
store_attribute(Domain,Item,{update,attribute,Name,Attributes}) ->
    [It] = qlc:e(retrieve_attributes_by_name(Domain,Item,Name)),
    Nit = It#attribute{value=Attributes}, 
    mnesia:write(Nit);
store_attribute(_Domain,_Item,_Data) -> 
    {error,"Unsupported store attribute request"}.

% Key Attribute storage Qualified versions
store_attribute(Qitem,{write,item,"users",Attributes}) ->
    [It] = mnesia:read({item,Qitem}),
    Nit = It#item{users=merge_attributes(Attributes,It#item.users)},
    mnesia:write(Nit);
store_attribute(Qitem,{write,item,"groups",Attributes}) ->
    [It] = mnesia:read({item,Qitem}),
    Nit = It#item{groups=merge_attributes(Attributes,It#item.groups)}, 
    mnesia:write(Nit);

store_attribute(Qitem,{update,item,"created",Attributes}) when is_list(Attributes) ->
    [It] = mnesia:read({item,Qitem}),
    Nit = It#item{created=to_i(Attributes)}, 
    mnesia:write(Nit);
store_attribute(Qitem,{update,item,"created",Attributes}) ->
    [It] = mnesia:read({item,Qitem}),
    Nit = It#item{created=Attributes}, 
    mnesia:write(Nit);
store_attribute(Qitem,{update,item,"modified",Attributes}) when is_list(Attributes) ->
    [It] = mnesia:read({item,Qitem}),
    Nit = It#item{modified=to_i(Attributes)}, 
    mnesia:write(Nit);
store_attribute(Qitem,{update,item,"modified",Attributes}) ->
    [It] = mnesia:read({item,Qitem}),
    Nit = It#item{modified=Attributes}, 
    mnesia:write(Nit);
store_attribute(Qitem,{update,item,"title",Attributes}) ->
    [It] = mnesia:read({item,Qitem}),
    Nit = It#item{title=Attributes}, 
    mnesia:write(Nit);
store_attribute(Qitem,{update,item,"description",Attributes}) ->
    [It] = mnesia:read({item,Qitem}),
    Nit = It#item{description=Attributes}, 
    mnesia:write(Nit);
store_attribute(Qitem,{update,item,"author",Attributes}) ->
    [It] = mnesia:read({item,Qitem}),
    Nit = It#item{author=Attributes}, 
    mnesia:write(Nit);
store_attribute(Qitem,{update,item,"type",Attributes}) ->
    [It] = mnesia:read({item,Qitem}),
    Nit = It#item{type=Attributes}, 
    mnesia:write(Nit);
store_attribute(Qitem,{update,item,"status",Attributes}) ->
    [It] = mnesia:read({item,Qitem}),
    Nit = It#item{status=Attributes}, 
    mnesia:write(Nit);
store_attribute(Qitem,{update,item,"users",Attributes}) ->
    [It] = mnesia:read({item,Qitem}),
    Nit = It#item{users=Attributes}, 
    mnesia:write(Nit);
store_attribute(Qitem,{update,item,"groups",Attributes}) ->
    [It] = mnesia:read({item,Qitem}),
    Nit = It#item{groups=Attributes}, 
    mnesia:write(Nit);
store_attribute(Qitem,{update,item,"xref",Attributes}) ->
    [It] = mnesia:read({item,Qitem}),
    Nit = It#item{xref=Attributes};
%% If not captured by above must actually either be an update instead of a write (As in item just created, not really updated) or an unknown key attribute. All other attributes are updateable only
store_attribute(Qitem,{write,item,Name,Attributes}) ->
    store_attribute(Qitem,{update,item,Name,Attributes});

% Attribute storage
store_attribute(Qitem,{write,attribute,Name,Attributes}) ->
    case qlc:e(retrieve_attributes_by_name(Qitem,Name)) of
	[] -> mnesia:write(#attribute{id=next_oid(Qitem),item=Qitem,name=Name,value=Attributes});
	[It] -> mnesia:write(It#attribute{value=merge_attributes(Attributes,It#attribute.value)})
    end;
store_attribute(Qitem,{update,attribute,Name,Attributes}) ->
    [It] = qlc:e(retrieve_attributes_by_name(Qitem,Name)),
    Nit = It#attribute{value=Attributes}, 
    mnesia:write(Nit);
store_attribute(_Qitem,_Data) -> 
    {error,"Unsupported store attribute request"}.


%% Used for attribute Ids (could use some form of {node(),now()} for less storage)
next_oid(Domain,Item) -> 
    {Meg,Sec,Mic} = now(),
    lists:flatten(io_lib:format("~s#~s/~B", [Domain,Item,Meg * 1000000000000 + Sec * 1000000 + Mic])).
next_oid(Qitem) -> 
    {Meg,Sec,Mic} = now(),
    lists:flatten(io_lib:format("~s/~B", [Qitem,Meg * 1000000000000 + Sec * 1000000 + Mic])).

%% Item ids
item_id(Domain,Item) ->
    (Domain ++ ?DOMAINSEPERATOR ++ Item).

domain_from_qitem(Qitem) ->
    hd(string:tokens(Qitem,?DOMAINSEPERATOR)).

%%Todo implement data converter
date_to_integer(Date) when is_integer(Date) ->
    Date;
date_to_integer({Meg,Sec,Mic}) ->
    Meg * 1000000000000 + Sec * 1000000 + Mic;
date_to_integer(Date) when is_list(Date)-> % Assumes integer date
    string_to_int(Date).

string_to_int(S) ->
    case io_lib:fread("~u", S) of
	 {ok, [Num], _} -> Num;
	{error,{_,Error}} -> 0
    end.

parent(Item) ->
    [_Node|Nodes] = lists:reverse(string:tokens(Item,"/")),
    case Nodes of
	[] ->
	    "/";
	Ns ->
	    parent(Ns,$/,[])
    end.

parent(Domain,Item) ->
    [_Node|Nodes] = lists:reverse(string:tokens(Item,"/")),
    case Nodes of
	[] ->
	    attribute:item_id(Domain,"/");
	Ns ->
	    attribute:item_id(Domain,parent(Ns,$/,[]))
    end.

parent([],_Sep,Path) ->    
    lists:flatten(Path);
parent([Node|Nodes],Sep,Path) ->    
    parent(Nodes,Sep,[[Sep|Node]|Path]).

params_to_string(Sep,Params) ->
    params_to_string(Sep,Params,[]).
params_to_string(Sep,[{K,V}],Out) ->
    params_to_string(Sep,[],[K ++ "=" ++ s(V) |Out]);
params_to_string(Sep,[{K,V,_Replace}],Out) ->
    params_to_string(Sep,[],[K ++ "=" ++ s(V) |Out]);
params_to_string(Sep,[K],Out) ->
    params_to_string(Sep,[],[K |Out]);
params_to_string(Sep,[{K,V}|Params],Out) ->
    params_to_string(Sep,Params,[K ++ "=" ++ s(V) ++ Sep |Out]);
params_to_string(Sep,[{K,V,_Replace}|Params],Out) ->
    params_to_string(Sep,Params,[K ++ "=" ++ s(V) ++ Sep |Out]);
params_to_string(Sep,[K|Params],Out) ->
    params_to_string(Sep,Params,[K ++ Sep |Out]);
params_to_string(_Sep,[],Out) -> lists:flatten(lists:reverse(Out) ++ "\n").


s(V) when is_list(V) ->
     V;
s(V) ->
    io_lib:format("~p",[V]).

to_i(S) when is_integer(S) -> S;
to_i(S) ->
    case string:to_integer(S) of 
	{error,_} ->
	    0;
	{I,_} ->
	    I
    end.
