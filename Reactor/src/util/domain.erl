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

-module(domain).
-include("schema.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([create/2,create/3,retrieve/0,retrieve/1,delete/1]).

%% Create a new domain
%% Spec: create(Domain::string()) ->
%%       {atomic,ok} |
%%       {error,Why}
%%
create(Domain,Matcher) ->
	add_domain(Domain,Matcher,?OWNER).

%%
create(Domain,Matcher,Owner) ->
	add_domain(Domain,Matcher,Owner).

%%
retrieve() -> do(qlc:q([{X#domain.domain,X#domain.matcher} || X <- mnesia:table(domain)])).
	
retrieve(Domain) -> 
   F = fun() -> mnesia:read({domain,Domain}) end,
   mnesia:transaction(F).

delete(Domain) -> %TODO we would need to remove children (items/attribute) of the domain also
    Oid = {domain,Domain},
    F = fun() ->
		mnesia:delete(Oid)
	end,
    mnesia:transaction(F).

add_domain(Domain,Matcher,Owner) ->
    Row = #domain{domain=Domain,owner=Owner,matcher=Matcher},
    F = fun() ->
		mnesia:write(Row)
	end,
    mnesia:transaction(F).

do(Q) ->
	F = fun() -> qlc:e(Q) end,
	{atomic,Val} = mnesia:transaction(F),
	Val.

