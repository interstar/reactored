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

-module(policy).
-export([create/5]).

%%--------------------------------------------------------------------
%%% Controls the creation policy
%%--------------------------------------------------------------------

create(Actor,Service,Domain,Resource,Params) ->
    % Inherit ACLS from parent
    identity_server:controls(Actor,Service,inherit,{qualified(Domain,Resource),Params}),
    % Add creator privilages
     identity_server:controls(Actor,Service,grant,{qualified(Domain,Resource),[{"acl","retrieve,update,delete,create,grant,revoke"}]}),
    void.

qualified(Domain,Item) ->
    attribute:item_id(Domain,Item).
