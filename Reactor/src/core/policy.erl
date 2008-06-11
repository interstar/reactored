-module(policy).
-export([create/5]).

%%--------------------------------------------------------------------
%%% Controls the creation policy
%%--------------------------------------------------------------------

create(Actor,Service,Domain,Resource,Params) ->
    % Inherit ACLS from parent
    identity_server:controls({uri,Actor},Service,inherit,{qualified(Domain,Resource),Params}),
    % Add creator privilages
     identity_server:controls({uri,Actor},Service,add,{qualified(Domain,Resource),[{"acl","retrieve,update,delete,create,grant,revoke"}]}),
    void.

qualified(Domain,Item) ->
    attribute:item_id(Domain,Item).
