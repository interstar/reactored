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

%%%-------------------------------------------------------------------
%%% File    : actor_server.erl
%%% Author  : Alan Wood <awood@alan-woods-macbook.local>
%%% Description : 
%%%
%%% Created : 28 May 2008 by Alan Wood <awood@alan-woods-macbook.local>
%%%-------------------------------------------------------------------
-module(actor_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([start/0,stop/0]).
-export([new/5,remove/3]).
-export([create/5,update/4,delete/3,delete/4,delete/5,retrieve/3,retrieve/4,retrieve/5,q/4,graph/5]).
-export([search/4,tagged/4,profile/4]).
-export([add_acl/4,remove_acl/4]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

%%====================================================================
%% Public Reactor API
%%====================================================================

%%====================================================================
%% types (attributes are the same as proplists)
%% @type credentials() = {uri,string()} | {token,string()}.
%% @type attribute() = {string(),string()}.
%% @type attributes() = [attribute()].
%% @type itemref() = {ok,string()}.
%% @type error() = {error,string()}.
%% @type item() = {item,uri,created,modified,domain,title,description,author,type,status,users,groups,revision,sync,xref}.
%%====================================================================

%%====================================================================
%% Identity/Profile API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: new(Credentials,Service,Domain,Item,Attributes) -> 
%%                         {ok,Xref} |
%%                         {error, Error}
%% Description: Creates a new participant and returns their xref
%%
%% @spec new(Credentials::credentials(),Service::string(),Domain::string(),
%% 	  Item::string(),Attributes::attributes()) ->
%%      {ok,Xref::string()} | {error, Error::string()}
%%--------------------------------------------------------------------
new(Credentials,Service,Domain,Item,Attributes) ->
    gen_server:call(?MODULE,{identity,Credentials,Service,create,Domain,Item,Attributes}).
%%--------------------------------------------------------------------
%% Function: remove(Credentials,Service,Domain,Item,Attributes) -> 
%%                         {ok,Xref} |
%%                         {error, Error}
%% Description: Deletes a participant and ACLs, returns their xref
%%
%% @spec new(Credentials::credentials(),Service::string(),Domain::string(),
%% 	  Item::string(),Attributes::attributes()) ->
%%      {ok,Xref::string()} | {error, Error::string()}
%%--------------------------------------------------------------------
remove(Credentials,Service,Item) ->
    gen_server:call(?MODULE,{identity,Credentials,Service,delete,Item}).

%%====================================================================
%% Resource API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: create(Credentials,Service,Domain, Item, Attributes) -> 
%%                         {ok,Xref} |
%%                         {error, Error}
%% Description: Creates a new Item (resource), returns its xref
%%
%% @spec create(Credentials::credentials(),Service::string(),Domain::string(),
%% 	  Item::string(),Attributes::attributes()) ->
%%      {ok,Xref::string()} | {error, Error::string()}
%%--------------------------------------------------------------------
create(Credentials,Service,Domain, Item, Attributes) ->
     gen_server:call(?MODULE,{Credentials,Service,create,Domain, Item, Attributes}).
%%--------------------------------------------------------------------
%% Function: update(Credentials,Service,Qitem, Attributes) -> 
%%                         {ok,Xref} |
%%                         {error, Error}
%% Description: Update and item (resource), returns its xref
%%
%% @spec update(Credentials::credentials(),Service::string(),Domain::string(),
%% 	  Item::string(),Attributes::attributes()) ->
%%      {ok,Xref::string()} | {error, Error::string()}
%%--------------------------------------------------------------------
update(Credentials,Service,Qitem, Attributes) -> %% Update only
     gen_server:call(?MODULE,{Credentials,Service,update,Qitem, Attributes}).
%%--------------------------------------------------------------------
%% Function: delete(Credentials,Service,Qitem) -> 
%%                         {ok,Xref} |
%%                         {error, Error}
%% Description: deletes an item (resource), returns its xref
%%
%% @spec delete(Credentials::credentials(),Service::string(),
%% 	  Item::string()) ->
%%      {ok,Xref::string()} | {error, Error::string()}
%%--------------------------------------------------------------------
delete(Credentials,Service,Qitem) ->
    gen_server:call(?MODULE,{Credentials,Service,delete,Qitem}).
%%--------------------------------------------------------------------
%% Function: delete(Credentials,Service,Domain, Item) -> 
%%                         {ok,Xref} |
%%                         {error, Error}
%% Description: deletes an item (resource), returns its xref
%%
%% @spec delete(Credentials::credentials(),Service::string(),Domain::string(),
%% 	  Item::string()) ->
%%      {ok,Xref::string()} | {error, Error::string()}
%%--------------------------------------------------------------------
delete(Credentials,Service,Domain, Item) ->
    gen_server:call(?MODULE,{Credentials,Service,delete,Domain, Item}).
%%--------------------------------------------------------------------
%% Function: delete(Credentials,Service,Domain, Item, Attributes) -> 
%%                         {ok,Xref} |
%%                         {error, Error}
%% Description: deletes an items attributes, returns its xref
%%
%% @spec new(Credentials::credentials(),Service::string(),Domain::string(),
%% 	  Item::string(),Attributes::attributes()) ->
%%      {ok,Xref::string()} | {error, Error::string()}
%%--------------------------------------------------------------------
delete(Credentials,Service,Domain, Item, Attributes) ->
    gen_server:call(?MODULE,{Credentials,Service,delete,Domain, Item, Attributes}).
%%--------------------------------------------------------------------
%% Function: retrieve(Credentials,Service,Qitem) -> 
%%                         [Attributes] |
%%                         {error, Error}
%% Description: retrieves an items and it's attributes
%%
%% @spec new(Credentials::credentials(),Service::string(),
%% 	  Item::string()) ->
%%      [Attributes::attributes()] | {error, Error}
%%--------------------------------------------------------------------
retrieve(Credentials,Service,Qitem) ->
    gen_server:call(?MODULE,{Credentials,Service,retrieve,Qitem}).
%%--------------------------------------------------------------------
%% Function: retrieve(Credentials,Service,Domain, Item) -> 
%%                         [Attributes] |
%%                         {error, Error}
%% Description: retrieves an items and it's attributes
%%
%% @spec new(Credentials::credentials(),Service::string(),Domain::string(),
%% 	  Item::string()) ->
%%      [Attributes::attributes()] | {error, Error}
%%--------------------------------------------------------------------
retrieve(Credentials,Service,Domain, Item) ->
     gen_server:call(?MODULE,{Credentials,Service,retrieve,Domain, Item}).
%%--------------------------------------------------------------------
%% Function: retrieve(Credentials,Service,Domain, Item, Attribute) -> 
%%                         [Attribute] |
%%                         {error, Error}
%% Description: retrieves an item's specific attributes
%%
%% @spec new(Credentials::credentials(),Service::string(),Domain::string(),
%% 	  Item::string(),Attributes::attributes()) ->
%%      [Attributes::attribute()] | {error, Error}
%%--------------------------------------------------------------------
retrieve(Credentials,Service,Domain, Item, Attribute) ->
    gen_server:call(?MODULE,{Credentials,Service,retrieve,Domain, Item, Attribute}).

%%====================================================================
%% listing API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: search(Credentials,Service,Domain,Text) -> 
%%                         [Item] |
%%                         {error, Error}
%% Description: retrieves items via plain Text search accross domain items
%%
%% @spec new(Credentials::credentials(),Service::string(),Domain::string(),
%% 	  Text::string()) ->
%%      [Item::item()] | {error, Error}
%%--------------------------------------------------------------------
search(Credentials,Service,Domain,Text) ->
    gen_server:call(?MODULE,{listing,Credentials,Service,search,Domain,Text}).
%%--------------------------------------------------------------------
%% Function: tagged(Credentials,Service,Domain,Tags) -> 
%%                         [Item] |
%%                         {error, Error}
%% Description: retrieves items tagged with tags for the specified domain
%%
%% @spec new(Credentials::credentials(),Service::string(),Domain::string(),
%% 	  tags::string()) ->
%%      [Item::item()] | {error, Error}
%%--------------------------------------------------------------------
tagged(Credentials,Service,Domain,Tags) ->
    gen_server:call(?MODULE,{listing,Credentials,Service,tags,Domain,Tags}).

%does this connect up to q(identity,Iid) via Identity_server
%%--------------------------------------------------------------------
%% Function: profile(Credentials,Service,Domain,Profile) -> 
%%                         [Item] |
%%                         {error, Error}
%% Description: retrieves the users profile (items under their identity)
%%
%% @spec new(Credentials::credentials(),Service::string(),Domain::string(),
%% 	  profile::string()) ->
%%      [Item::item()] | {error, Error}
%%--------------------------------------------------------------------
profile(Credentials,Service,Domain,Profile) ->
    gen_server:call(?MODULE,{listing,Credentials,Service,profile,Domain,Profile}).
%%--------------------------------------------------------------------
%% Function: q(Credentials,Service,Domain, Attributes) -> 
%%                         [Item] |
%%                         {error, Error}
%% Description: retrieves items with matching attributes accross domain
%%
%% @spec new(Credentials::credentials(),Service::string(),Domain::string(),
%% 	  Attributes::attributes()) ->
%%      [Item::item()] | {error, Error}
%%--------------------------------------------------------------------
q(Credentials,Service,Domain, Attributes) ->
    gen_server:call(?MODULE,{listing,Credentials,Service,q,Domain,Attributes}).
%%--------------------------------------------------------------------
%% Function: graph(Credentials,Service,Domain, Uri, Attributes) -> 
%%                         [Item] |
%%                         {error, Error}
%% Description: retrieves the item graph (node tree)
%%
%% @spec new(Credentials::credentials(),Service::string(),Domain::string(),
%% 	  item::string(),Attributes::attributes()) ->
%%      [Item::item()] | {error, Error}
%%--------------------------------------------------------------------
graph(Credentials,Service,Domain, Uri, Attributes) ->
    gen_server:call(?MODULE,{listing,Credentials,Service,graph,Domain,{Uri,Attributes}}).

%%====================================================================
%% ACL Controls {uri,Iuri},{Uri,Attributes}
%%====================================================================

add_acl(Credentials,Service,Qitem,Attributes) ->
    gen_server:call(?MODULE,{Credentials,Service,add_acl,Qitem, Attributes}).
remove_acl(Credentials,Service,Qitem,Attributes) ->
    gen_server:call(?MODULE,{Credentials,Service,remove_acl,Qitem, Attributes}).

%%====================================================================
%% End Public Reactor API
%%====================================================================

%%====================================================================
%% Control API (for testing only not part of the public apis)
%%====================================================================
start() -> start_link().
stop() -> gen_server:call(?MODULE,stop).
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% Identity/Profile calls

handle_call({identity,Credentials,Service,create,Domain,Item,Attributes}, _From, State) ->
    %% Note special case of authority based on parent ACL having 'create' control
    Reply = case identity_server:authorise(Credentials,Service,create,{parent(Domain,Item), Attributes}) of 
		{ok,Actor} -> 
		    case identity_server:create(Domain,Item,Attributes) of
			{ok,Id} ->
			    proplists:delete("email",Attributes),
			    proplists:delete("password",Attributes),
			    case attribute_server:create(Domain,Item,[{"identity",Id},{"type","identity"}] ++ Attributes) of
				{ok,Xref} ->
				    pattern_server:process(Actor,Service,create,Domain,Item,[{"xref",Xref}|Attributes]),
				    {ok,Xref};
				_ -> {error,Actor,error({"Could not create resource",Domain,Item,Attributes})}
			    end; 
			_ -> {error,Actor,error({"Could not create identity",Domain,Item,Attributes})}
		    end;
		{error,Actor,Why} ->
		    pattern_server:process(Actor,Service,autherror,Domain,Item,[{"autherror",Why}|Attributes]),
		    {autherror,Why}
	    end,
    {reply, Reply, State};

handle_call({identity,Credentials,Service,delete,Item}, _From, State) ->
    Reply = case identity_server:authorise(Credentials,Service,delete,{Item,[]}) of 
		{ok,Actor} -> 
		    case identity_server:delete(Item) of
			{ok,_Id} ->
			    case attribute_server:delete(Item) of
				{ok,Xref} ->
				    %% TODO system needs to remove traces of this participant ; links etc..
				    pattern_server:process(Actor,Service,delete,domain(Item),Item,[{"xref",Xref}]),
				    {ok,Xref};
				   _ -> 
				    {error,Actor,error({"Could not delete resource",Item})}
			    end; 
			_ ->{error,Actor,error({"Could not delete identity",Item})}
		    end;
		{error,Actor,Why} ->
		    pattern_server:process(Actor,Service,autherror,[],Item,[{"autherror",Why}]),
		    {autherror,Why}
		end,
    {reply, Reply, State};


%% Resource calls
handle_call({Credentials,Service,create,Domain, Item, Attributes}, _From, State) ->
    %% Note special case of authority based on parent ACL having 'create' control
    Reply = case identity_server:authorise(Credentials,Service,create,{parent(Domain,Item), Attributes}) of 
		{ok,Actor} -> 
		    case attribute_server:create(Domain,Item,Attributes) of
			{ok,Xref} ->
			    pattern_server:process(Actor,Service,create,Domain,Item,[{"xref",Xref}|Attributes]),
			    {ok,Xref};
		    _ -> {error,Actor,error({"Could not create resource",Domain,Item,Attributes})}
		    end;
		{error,Actor,Why} ->
		    pattern_server:process(Actor,Service,autherror,Domain,Item,[{"autherror",Why}|Attributes]),
		    {autherror,Why}
		end,
    {reply, Reply, State};

handle_call({Credentials,Service,update,Qitem, Attributes}, _From, State) ->
    Reply = case identity_server:authorise(Credentials,Service,update,{Qitem,[]}) of 
		{ok,Actor} -> 
		    case attribute_server:update(Qitem,Attributes) of 
			{ok,Xref} ->
			    pattern_server:process(Actor,Service,update,domain(Qitem),Qitem,[{"xref",Xref}|Attributes]),
			    {ok,Xref};
			_ -> {error,Actor,error({"Could not update resource",Qitem,Attributes})}
		    end;
		{error,Actor,Why} ->
		    pattern_server:process(Actor,Service,autherror,domain(Qitem),Qitem,[{"autherror",Why}|Attributes]),
		    {autherror,Why}
		end,
    {reply, Reply, State};

handle_call({Credentials,Service,delete,Qitem}, _From, State) ->
    Reply = case identity_server:authorise(Credentials,Service,delete,{Qitem,[]}) of 
		{ok,Actor} -> 
		    case attribute_server:delete(Qitem) of
			{ok,Xref} ->
			    pattern_server:process(Actor,Service,delete,domain(Qitem),Qitem,[{"xref",Xref}]),
			    {ok,Xref};
			_ -> {error,Actor,error({"Could not delete resource",Qitem})}
		    end;
		{error,Actor,Why} ->
		    pattern_server:process(Actor,Service,autherror,domain(Qitem),Qitem,[{"autherror",Why}]),
		    {autherror,Why}
		end,
    {reply, Reply, State};


handle_call({Credentials,Service,delete,Domain, Item}, _From, State) ->
    Reply = case identity_server:authorise(Credentials,Service,delete,qualified(Domain,Item)) of 
		{ok,Actor} -> 
		    case attribute_server:delete(Domain,Item) of
			{ok,Xref} ->
			    pattern_server:process(Actor,Service,delete,Domain,Item,[{"xref",Xref}]),
			    {ok,Xref};
			_ -> {error,Actor,error({"Could not delete resource",Domain,Item})}
		    end;
		{error,Actor,Why} ->
		    pattern_server:process(Actor,Service,autherror,Domain,Item,[{"autherror",Why}]),
		    {autherror,Why}
		end,
    {reply, Reply, State};

handle_call({Credentials,Service,delete,Domain, Item, Attributes}, _From, State) ->
    Reply = case identity_server:authorise(Credentials,Service,delete,{qualified(Domain,Item), Attributes}) of 
		{ok,Actor} -> 
		    case attribute_server:delete(Domain,Item,Attributes) of
			{ok,Xref} ->
			    pattern_server:process(Actor,Service,delete,Domain,Item,[{"xref",Xref}|Attributes]),
			    {ok,Xref};
			_ -> {error,Actor,error({"Could not delete attributes",Domain,Item,Attributes})}
		    end;
		{error,Actor,Why} ->
		    pattern_server:process(Actor,Service,autherror,Domain,Item,[{"autherror",Why}|Attributes]),
		    {autherror,Why}
		end,
    {reply, Reply, State};

handle_call({Credentials,Service,retrieve,Qitem}, _From, State) ->
    Reply = case identity_server:authorise(Credentials,Service,retrieve,{Qitem,[]}) of 
		{ok,Actor} -> 
		    pattern_server:process(Actor,Service,retrieve,domain(Qitem),Qitem,[]),
		    attribute_server:retrieve(Qitem);
		{error,Actor,Why} ->
		    pattern_server:process(Actor,Service,autherror,domain(Qitem),Qitem,[{"autherror",Why}]),
		    {autherror,Why}
		end,
    {reply, Reply, State};

handle_call({Credentials,Service,retrieve,Domain, Item}, _From, State) ->
    Reply = case identity_server:authorise(Credentials,Service,retrieve,qualified(Domain,Item)) of 
		{ok,Actor} -> 
		    pattern_server:process(Actor,Service,retrieve,Domain,Item,[]),
		    attribute_server:retrieve(Domain,Item);
		{error,Actor,Why} ->
		    pattern_server:process(Actor,Service,autherror,Domain,Item,[{"autherror",Why}]),
		    {autherror,Why}
		end,
    {reply, Reply, State};

handle_call({Credentials,Service,retrieve,Domain, Item, Attributes}, _From, State) ->
    Reply = case identity_server:authorise(Credentials,Service,retrieve,{qualified(Domain,Item), Attributes}) of 
		{ok,Actor} -> 
		    pattern_server:process(Actor,Service,retrieve,Domain,Item,Attributes),
		    attribute_server:retrieve(Domain,Item,Attributes);
		{error,Actor,Why} ->
		    pattern_server:process(Actor,Service,autherror,Domain,Item,[{"autherror",Why}|Attributes]),
		    {autherror,Why}
		end,
    {reply, Reply, State};

handle_call({listing,{uri,Actor},Service,q,Domain,Attributes}, _From, State) ->
    pattern_server:process(Actor,Service,q,Domain,Attributes),
    Reply = identity_server:filter(q,Actor,Domain,Attributes),
    {reply, Reply, State};

handle_call({listing,{uri,Actor},Service,graph,Domain,{Uri,Attributes}}, _From, State) ->
    pattern_server:process(Actor,Service,graph,Domain,Uri,Attributes),
    Reply = identity_server:filter(graph,Actor,Domain,{Uri,Attributes}),
    {reply, Reply, State};

%% listing calls
handle_call({listing,{uri,Actor},Service,Meta,Qitem,Q}, _From, State) ->
    Attrib = {atom_to_list(Meta),Q},
    pattern_server:process(Actor,Service,Meta,[],Qitem,[Attrib]),
    Reply = identity_server:filter(Meta,Actor,domain(Qitem),Q),
    {reply, Reply, State};

%% ACL Controls
handle_call({Credentials,Service,add_acl,Qitem, Attributes}, _From, State) ->
    Reply = case identity_server:authorise(Credentials,Service,add_acl,{Qitem, Attributes}) of 
		{ok,Actor} -> 
		    pattern_server:process(Actor,Service,add_acl,[],Qitem,Attributes),
		    identity_server:controls(Credentials,Service,add_acl,{Qitem, Attributes});
		{error,Actor,Why} ->
		    pattern_server:process(Actor,Service,autherror,[],Qitem,[{"autherror",Why},Attributes]),
		    {autherror,Why}
		end,
    {reply, Reply, State};
handle_call({Credentials,Service,remove_acl,Qitem, Attributes}, _From, State) ->
    Reply = case identity_server:authorise(Credentials,Service,remove_acl,{Qitem, Attributes}) of 
		{ok,Actor} -> 
		    pattern_server:process(Actor,Service,remove_acl,[],Qitem,Attributes),
		    identity_server:controls(Credentials,Service,remove_acl,{Qitem, Attributes});
		{error,Actor,Why} ->
		    pattern_server:process(Actor,Service,autherror,[],Qitem,[{"autherror",Why},Attributes]),
		    {autherror,Why}
		end,
    {reply, Reply, State};

handle_call(stop, _From, State) ->
    {stop,normal,stopped, State};

handle_call(Request, _From, State) ->
    io:format("Actor request not supported ~p",[Request]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
domain(Qitem) ->
    attribute:domain_from_qitem(Qitem).

qualified(Domain,Item) ->
    attribute:item_id(Domain,Item).

parent(Domain,Item) ->
    [Node|Nodes] = lists:reverse(string:tokens(Item,"/")),
    attribute:item_id(Domain,parent(Nodes,$/,[])).

parent([],Sep,Path) ->    
    lists:flatten(Path);
parent([Node|Nodes],Sep,Path) ->    
    parent(Nodes,Sep,[[Sep|Node]|Path]).


%% for full paths including protocol e.g. http://www.rel3.com/rel3/identities
qualified_parent([Node|[]],Sep,Path) ->    
    lists:flatten([Node|Path]);
qualified_parent([Node|Nodes],Sep,Path) ->    
    qualified_parent(Nodes,Sep,[[Sep|Node]|Path]).
    

error(Error) ->
    error_logger:error_msg("Actor server - Says Whoops ~p~n",[Error]),
    Error.
