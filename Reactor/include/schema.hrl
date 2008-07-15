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

-record(config,{domain,email,password,token,branches}).
-record(branch,{name,type,matcher,description}).
-record(domain,{domain,owner,matcher}).
-record(item,{item,uri,created,modified,domain,title,description,author,type,status,users,groups,revision,sync,xref}).
-record(attribute,{id,item,name,value}).
-record(index,{id,url,modified}).
-record(words,{id,word,lid,score}).
-record(tags,{id,author,tag,lid}).
-record(control,{id,iid,lid,types}).
-record(identity,{id,uri,email,nick,pswd,token}).
-record(usession,{userid,actor,ts}).
-define(OWNER,"reactored.com").
-define(DOMAINSEPERATOR,"|").


% Todo
% I would like to rename some of the item record atoms"
%% description -> content
%% author -> creator
%% users ->  related 
%% groups -> affiliations/affinities can be groups,categories,tags,relations etc..

%% new item attributes - triples
%% X -> v1 -> number()
%% Y -> v2 -> number()
%% z -> v3 -> number()
%% Scenarios :
%% {X,Y,Z} = {It#item.x,It#item.y,It#item.z} where is_position(It);
%% {Price,Tax,Quantity} = {It#item.x,It#item.y,It#item.z} where is_product(It);
%% {Credit,Debit,Currency} = {It#item.x,It#item.y,It#item.z} where is_ledger(It);
%% {Start,End,Repeat} = {It#item.x,It#item.y,It#item.z} where is_event(It);
%% {Start,End,Rate} = {It#item.x,It#item.y,It#item.z} where is_timesheet(It);

% will neet to add the following to attribute server
% cache:update(Domain,Item,Status)
% Will also need to update cache:ensure to handle
% cache:ensure(Domain,Item) -> cache:ensure(Domain,Item,Attributes)
% Along with timestamps for created and modified + defaults for any attributes not provided (title,description,author,type,status)
