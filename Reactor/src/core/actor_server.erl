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
-include("schema.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([start/0,stop/0]).
-export([domain/3,domain/5,clear/3]).
-export([new/5,remove/3]).
-export([create/5,update/4,delete/3,delete/4,delete/5,retrieve/3,retrieve/4,retrieve/5,q/4,graph/5]).
-export([search/4,tagged/4,profile/4,tag/5]).
-export([grant/5,revoke/5]).
%% Special functions reactor internal usage
-export([create_id_fork/3]).
-export([lookup/1]).
-export([flush/3]).
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
%% @type Iacl() = {int(),[atom()])
%%====================================================================

%%====================================================================
%% Domain API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: domain(Credentials,Service,Domain,Url) -> 
%%                         {ok,Domains |
%%                         {error, Error}
%% Description: Creates a new participant and returns their xref
%%
%% @spec domain(Credentials::credentials(),Service::string(),
%% 	 Url::string()) ->
%%      {ok,domains::proplist()} | {error, Error::string()}
%%--------------------------------------------------------------------
domain(Credentials,Service,Url) ->
    gen_server:call(?MODULE,{domain,Credentials,Service,Url}).
%%--------------------------------------------------------------------
%% Function: domain(Credentials,Service,Domain,Url,Matcher) -> 
%%                         {ok,Domains |
%%                         {error, Error}
%% Description: Creates a new participant and returns their xref
%%
%% @spec domain(Credentials::credentials(),Service::string(),Domain::string(),
%% 	  Url::string(),Matcher::string()) ->
%%      {ok,domains::proplist()} | {error, Error::string()}
%%--------------------------------------------------------------------
domain(Credentials,Service,Domain,Url,Matcher) ->
    gen_server:call(?MODULE,{domain,Credentials,Service,Domain,Url,Matcher}).

%%--------------------------------------------------------------------
%% Function: domain(Credentials,Service,Domain) -> 
%%                         {ok,Domains |
%%                         {error, Error}
%% Description: clears a domain of all but '/' items
%%
%% @spec domain(Credentials::credentials(),Service::string(),Domain::string()) ->
%%      {ok,domains::proplist()} | {error, Error::string()}
%%--------------------------------------------------------------------
clear(Credentials,Service,Domain) ->
    gen_server:call(?MODULE,{clear,Credentials,Service,Domain}).

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
%%                         {ok,[Attributes]} |
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
%%                         {ok,[Attributes]} |
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
%%                         {ok,{key,val}} |
%%                         {error, Error}
%% Description: retrieves an item's specific attributes
%%
%% @spec new(Credentials::credentials(),Service::string(),Domain::string(),
%% 	  Item::string(),Attributes::attributes()) ->
%%      [Attributes::attribute()] | {error, Error}
%%--------------------------------------------------------------------
retrieve(Credentials,Service,Domain, Item, []) ->
    retrieve(Credentials,Service,Domain, Item);
%% Todo normalise - > This returns {ok{key,val}} val could a be a string or list of strings. We should make this always return a list of strings even if only 1
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
    gen_server:call(?MODULE,{listing,Credentials,Service,tagged,Domain,Tags}).

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
%% Meta Data APIs {uri,Iuri},{Uri,Attributes} 
%%====================================================================

%%--------------------------------------------------------------------
%% Function: tag(Credentials,Service,Destination,Resource,Tags) -> 
%%                         TagRecs 
%% Description: Tag a resources
%%
%% @spec tag(Credentials::credentials(),Service::string(),
%%         Destination::string(),Resource::string(),Tags::string() ->
%%      {ok,[Tags::string()]} | {error, Error::string()}
%%--------------------------------------------------------------------
tag(Credentials,Service,Destination,Resource,Tags) ->
    gen_server:call(?MODULE,{tag,Credentials,Service,Destination,Resource,Tags}).

%%====================================================================
%% ACL Controls {uri,Iuri},{Uri,Attributes} 
%%====================================================================

%%--------------------------------------------------------------------
%% Function: grant(Credentials,Service,Iuri,Qitem,Attributes) -> 
%%                         {ok,Iacl} |
%%                         {error, Error}
%% Description: grant access privelages via ACL attributes
%%
%% @spec new(Credentials::credentials(),Service::string(),Iuri::string(),item::string(),
%% 	  Attributes::attributes()) ->
%%      {ok,Id::string()} | {error, Error::string()}
%%--------------------------------------------------------------------
grant(Credentials,Service,Iuri,Qitem,Attributes) ->
    gen_server:call(?MODULE,{Credentials,Service,grant,Iuri,Qitem, Attributes}).
%%--------------------------------------------------------------------
%% Function: revoke(Credentials,Service,Iuri,Qitem,Attributes) -> 
%%                         {ok,Iacl} |
%%                         {error, Error}
%% Description: revokes access privelages via ACL attributes
%%
%% @spec new(Credentials::credentials(),Service::string(),Iuri::string(),item::string(),
%% 	  Attributes::attributes()) ->
%%      {ok,Id::string()} | {error, Error::string()}
%%--------------------------------------------------------------------
revoke(Credentials,Service,Iuri,Qitem,Attributes) ->
    gen_server:call(?MODULE,{Credentials,Service,revoke,Iuri,Qitem, Attributes}).
lookup(Qitem) ->
    gen_server:call(?MODULE,{lookup,Qitem}).
flush(Credentials,Service,Queue) ->
    gen_server:call(?MODULE,{flush,Credentials,Service,Queue}).

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
    io:format("~p starting~n",[?MODULE]),
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

% Lookup function to retrieve Qitem from Uri
handle_call({lookup,Uri}, _From, State) ->
    Reply = case attribute_server:retrieve({item,Uri}) of
		{ok,[]} ->
		    [];
		{ok,[It]} ->
		    It
	    end,
    {reply, Reply, State};

% fushes system queue
handle_call({flush,Credentials,Service,Queue}, _From, State) ->
    [_|Q] = lists:reverse(Queue),
    Reply = case identity_server:authorise(Credentials,Service,delete,{lists:reverse(Q) ++ "|/",[]}) of 
		{ok,Actor} -> 
		    sink_server:flush();
		{error,Actor,Why} ->
		    pattern_server:process(Actor,Service,autherror,[],Queue ++ "/",[{"autherror",Why}]),
		    {autherror,Why}
		end,
    {reply, Reply, State};

%% Echo REST service calls for testing
handle_call({Credentials,echo,Command,Domain,Item,Attributes}, _From, State) ->
    Reply = "Echo service, REST call " ++ atom_to_list(Command) ++ "," ++ Domain ++ Item ++ echo_cred(Credentials) ++ attribute:params_to_string("|",Attributes) ++ "test",
    {reply, Reply, State};
handle_call({Credentials,echo,Command,Item,Attributes}, _From, State) ->
    Reply = "Echo service, REST call " ++ atom_to_list(Command) ++ "," ++ Item  ++ echo_cred(Credentials) ++ attribute:params_to_string("|",Attributes),
    {reply, Reply, State};
handle_call({Credentials,echo,Command,Item}, _From, State) ->
    Reply = "Echo service, REST call " ++ atom_to_list(Command) ++ "," ++ Item  ++ echo_cred(Credentials),
    {reply, Reply, State};

%% Domain calls
handle_call({domain,Credentials,Service,Url}, _From, State) ->
    Reply = case identity_server:authorise(Credentials,Service,retrieve,{Url,[]}) of 
		{ok,Actor} -> 
		    case domain_server:retrieve() of
			{ok,Domains} ->
			    pattern_server:process(Actor,Service,retrieve,[],Url,[]),
			    {ok,domains_to_prop(Domains)};
			{error,Error} -> 
			    {error,error({"Could not list domains",Url,Error})}
		    end;
		{error,Actor,Why} ->
		    pattern_server:process(Actor,Service,autherror,[],Url,[{"autherror",Why}]),
		    {autherror,Why}
		end,
    {reply, Reply, State};

handle_call({domain,Credentials,Service,Domain,Url,Matcher}, _From, State) ->
    Reply = case identity_server:authorise(Credentials,Service,create,{Url,[]}) of 
		{ok,Actor} -> 
		    case domain_server:create(Domain,list_to_atom(Matcher),Actor) of
			{ok,Domain} ->
			    pattern_server:process(Actor,Service,create,Domain,Url,[]),
			    {ok,[{Domain,list_to_atom(Matcher)}]};
			{error,Error} -> 
			    {error,error({"Could not create domain",Domain,Error})}
		    end;
		{error,Actor,Why} ->
		    pattern_server:process(Actor,Service,autherror,Domain,Url,[{"autherror",Why}]),
		    {autherror,Why}
		end,
    {reply, Reply, State};

handle_call({clear,Credentials,Service,Domain}, _From, State) ->
    Reply = case identity_server:authorise(Credentials,Service,delete,{qualified(Domain,"/"),[]}) of
		{ok,Actor} ->
		    case listing_q(Actor,Service,Domain,[]) of
			[] ->
			    {error,error({"Could not clear domain, no items ",Domain})};
			Items ->
			    %delete them all items from this domain except root
			    {ok,[delete_filter(Credentials,Service,lists:reverse(Item#item.item)) || Item <- Items]}
		    end;
		{error,Actor,Why} ->
		    pattern_server:process(Actor,Service,autherror,Domain,"/",[{"autherror",Why}]),
		    {autherror,Why}
		end,
    {reply, Reply, State};

%% Identity/Profile calls

handle_call({identity,Credentials,Service,create,Domain,Item,Attributes}, _From, State) ->
    %% Note special case of authority based on parent ACL having 'create' control
    Parent = attribute:parent(Domain,Item),
    Attribs = [{"groups",Parent}|Attributes], %groups = parent
    Reply = case identity_server:authorise(Credentials,Service,create,{Parent, Attributes}) of 
		{ok,Actor} -> 
		    case identity_server:create(Domain,Item,Attribs) of
			{ok,Id} ->
			    Attribs1 = proplists:delete("password",Attribs),
			    case attribute_server:create(Domain,Item,[{"identity",Id},{"type","identity"}] ++ Attribs1) of
				{ok,Xref} ->
				    pattern_server:process(Actor,Service,create,Domain,Item,[{"xref",Xref}|Attribs1]),
				    Title = case proplists:get_value("title",Attribs1) of
						undefined -> Item;
						T -> T
					    end,
				    create_identity_fork(Service,Actor,Domain,Item,Title),
				    {ok,Xref};
				_ -> {error,Actor,error({"Could not create resource",Domain,Item,Attribs})}
			    end; 
			_ -> {error,error({"Could not create identity",Domain,Item,Attribs})}
		    end;
		{error,Actor,Why} ->
		    pattern_server:process(Actor,Service,autherror,Domain,Item,[{"autherror",Why}|Attribs]),
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
				    delete_id_fork(Item),
				    {ok,Xref};
				   _ -> 
				    {error,error({"Could not delete resource",Item})}
			    end; 
			_ ->{error,error({"Could not delete identity",Item})}
		    end;
		{error,Actor,Why} ->
		    pattern_server:process(Actor,Service,autherror,[],Item,[{"autherror",Why}]),
		    {autherror,Why}
		end,
    {reply, Reply, State};


%% Resource calls
handle_call({Credentials,Service,create,Domain, Item, Attributes}, _From, State) ->
    %% Note special case of authority based on parent ACL having 'create' control
    Parent = attribute:parent(Domain,Item),
    Attribs = [{"groups",Parent}|Attributes],
    Reply = case identity_server:authorise(Credentials,Service,create,{Parent, Attributes}) of 
		{ok,Actor} -> 
		    create_resource(Actor,Service,Domain,Item,Attribs);
		{error,Actor,Why} ->
		    pattern_server:process(Actor,Service,autherror,Domain,Item,[{"autherror",Why}|Attribs]),
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
			_ -> {error,error({"Could not update resource",Qitem,Attributes})}
		    end;
		{error,Actor,Why} ->
		    pattern_server:process(Actor,Service,autherror,domain(Qitem),Qitem,[{"autherror",Why}|Attributes]),
		    {autherror,Why}
		end,
    {reply, Reply, State};

handle_call({Credentials,Service,delete,Qitem}, _From, State) ->
    Reply = delete_item(Credentials,Service,Qitem),
    {reply, Reply, State};


handle_call({Credentials,Service,delete,Domain, Item}, _From, State) ->
    Reply = case identity_server:authorise(Credentials,Service,delete,qualified(Domain,Item)) of 
		{ok,Actor} -> 
		    case attribute_server:delete(Domain,Item) of
			{ok,Xref} ->
			    pattern_server:process(Actor,Service,delete,Domain,Item,[{"xref",Xref}]),
			    {ok,Xref};
			_ -> {error,error({"Could not delete resource",Domain,Item})}
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
			_ -> {error,error({"Could not delete attributes",Domain,Item,Attributes})}
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
    Reply = listing_q(Actor,Service,Domain,Attributes),
    {reply, Reply, State};
handle_call({listing,{token,Token},Service,q,Domain,Attributes}, _From, State) ->
    Actor = identity_server:actor_from_token(Token),
    Reply = listing_q(Actor,Service,Domain,Attributes),
    {reply, Reply, State};


handle_call({listing,{uri,Actor},Service,graph,Domain,{Uri,Attributes}}, _From, State) ->
    Reply = listing_graph(Actor,Service,Domain,Uri,Attributes),
    {reply, Reply, State};
handle_call({listing,{token,Token},Service,graph,Domain,{Uri,Attributes}}, _From, State) ->
    Actor = identity_server:actor_from_token(Token),
    Reply = listing_graph(Actor,Service,Domain,Uri,Attributes),
    {reply, Reply, State};


%% listing calls
handle_call({listing,{uri,Actor},Service,Meta,Qitem,Q}, _From, State) ->
    Reply = filter(Actor,Service,Meta,Qitem,Q),
    {reply, Reply, State};
handle_call({listing,{token,Token},Service,Meta,Qitem,Q}, _From, State) ->
    Actor = identity_server:actor_from_token(Token),
    Reply = filter(Actor,Service,Meta,Qitem,Q),
    {reply, Reply, State};

handle_call({tag,Credentials,Service,Dest,Qitem,Tags}, _From, State) ->
    %io:format("About to Tag ~p~n",[{Dest,Qitem,Tags}]),
    Reply = case identity_server:authorise(Credentials,Service,update,{Dest,[]}) of 
		{ok,Actor} -> 
		    pattern_server:process(Actor,Service,tag,[],Dest,[]),
		    identity_server:tag(Actor,Qitem,Tags);
		{error,Actor,Why} ->
		    pattern_server:process(Actor,Service,autherror,[],Dest,[{"autherror",Why}]),
		    {autherror,Why}
		end,
    {reply, Reply, State};
%% ACL Controls
handle_call({Credentials,Service,grant,Iuri,Qitem,Attributes}, _From, State) ->
    Reply = case identity_server:authorise(Credentials,Service,grant,{Qitem, Attributes}) of 
		{ok,Actor} -> 
		    pattern_server:process(Actor,Service,grant,[],Qitem,Attributes),
		    identity_server:controls(Iuri,Service,grant,{Qitem, Attributes});
		{error,Actor,Why} ->
		    pattern_server:process(Actor,Service,autherror,[],Qitem,[{"autherror",Why},Attributes]),
		    {autherror,Why}
		end,
    {reply, Reply, State};

handle_call({Credentials,Service,revoke,Iuri,Qitem,Attributes}, _From, State) ->
    Reply = case identity_server:authorise(Credentials,Service,revoke,{Qitem, Attributes}) of 
		{ok,Actor} -> 
		    pattern_server:process(Actor,Service,revoke,[],Qitem,Attributes),
		    identity_server:controls(Iuri,Service,revoke,{Qitem, Attributes});
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
create_resource(Actor,Service,Domain,Item,Attribs) ->
    case attribute_server:create(Domain,Item,[{"author",Actor}|Attribs]) of
	{ok,Xref} ->
	    pattern_server:process(Actor,Service,create,Domain,Item,[{"xref",Xref}|Attribs]),
	    {ok,Xref};
	_ -> {error,error({"Could not create resource",Domain,Item,Attribs})}
    end.

create_resource(Actor,Service,Domain,Item,Parent,Attribs) ->
    create_resource(Actor,Service,Domain,Item,[{"groups",Parent}|Attribs]).
create_identity_fork(Service,Actor,Domain,Item,Title) ->
    Parent = qualified(Domain,Item),
    ACL = Item ++ "/acl",
    ACL_attribs = [{"type","acl"},{"title",Title ++ " ACL"}],
    Tags = Item ++ "/tags",
    Tags_attribs = [{"type","tag"},{"title",Title ++ " Tags"}],
    Profile = Item ++ "/profile",
    Profile_attribs = [{"type","profile"},{"title",Title ++ " Profile"}],
    io:format("Creating identity fork ~p~n",[{Domain,Item,Title}]),
    case create_resource(Actor,Service,Domain,ACL,Parent,ACL_attribs) of
	{ok,_Xref0} ->
	    io:format("Creating ACL fork ~p~n",[{Domain,ACL,ACL_attribs}]);
	_ ->
	    io:format("Error creating ACL fork ~p~n",[{Domain,ACL,ACL_attribs}])
    end,
    case create_resource(Actor,Service,Domain,Tags,Parent,Tags_attribs) of
	{ok,_Xref1} ->
	    io:format("Creating Tag fork ~p~n",[{Domain,Tags,Tags_attribs}]);
	_ ->
	    io:format("Error creating Tag fork ~p~n",[{Domain,Tags,Tags_attribs}])
    end,
    case create_resource(Actor,Service,Domain,Profile,Parent,Profile_attribs) of
	{ok,_Xref2} ->
	    io:format("Creating Tag fork ~p~n",[{Domain,Profile,Profile_attribs}]);
	_ ->
	    io:format("Error creating Tag fork ~p~n",[{Domain,Profile,Profile_attribs}])
    end.

listing_q(Actor,Service,Domain,Attributes) ->
    pattern_server:process(Actor,Service,q,Domain,Domain ++ "/",Attributes),
    identity_server:filter(q,Actor,Domain,Attributes).

listing_graph(Actor,Service,Domain,Uri,Attributes) ->
    pattern_server:process(Actor,Service,graph,Domain,Uri,Attributes),
    identity_server:filter(graph,Actor,Domain,{Uri,Attributes}).

filter(Actor,Service,Meta,Qitem,Q) ->
    Attrib = {atom_to_list(Meta),Q},
    pattern_server:process(Actor,Service,Meta,[],Qitem,[Attrib]),
    identity_server:filter(Meta,Actor,domain(Qitem),Q).

domain(Qitem) ->
    attribute:domain_from_qitem(Qitem).

qualified(Domain,Item) ->
    attribute:item_id(Domain,Item).




%% for full paths including protocol e.g. http://www.rel3.com/rel3/identities
qualified_parent([Node|[]],_Sep,Path) ->    
    lists:flatten([Node|Path]);
qualified_parent([Node|Nodes],Sep,Path) ->    
    qualified_parent(Nodes,Sep,[[Sep|Node]|Path]).
    

error(Error) ->
    error_logger:error_msg("Actor server - Says Whoops ~p~n",[Error]),
    Error.

echo_cred({annonymous})->
    " Annonymously\n";
echo_cred({uri,Uri}) ->
    " logged in as " ++ Uri ++ "\n";
echo_cred({token,Token}) ->
    " Using Token " ++ Token ++ "\n".

create_id_fork(Domain,Item,{"title",Title}) ->
    io:format("Creating if fork ~p~n",[{Domain,Item,Title}]),
    attribute_server:create(Domain,Item ++ "/acl",[{"type","acl"},{"title",Title ++ " ACL"}]),
    attribute_server:create(Domain,Item ++ "/tags",[{"type","tag"},{"title",Title ++ " Tags"}]),
    attribute_server:create(Domain,Item ++ "/profile",[{"type","profile"},{"title",Title ++ " Profile"}]);

create_id_fork(Domain,Item,Attribs) ->
    Title = case proplists:get_value("title",Attribs) of
	      undefined -> "";
	      T -> T
	    end,
    create_id_fork(Domain,Item,{"title",Title}).

delete_id_fork(Item) ->
    attribute_server:delete(Item ++ "/acl"),
    attribute_server:delete(Item ++ "/tags"),
    attribute_server:delete(Item ++ "/profile").


domains_to_prop(Domains) ->
    lists:map(fun({D,M}) -> {D,atom_to_list(M)} end,Domains).

    
delete_filter(Credentials,Service,"/|" ++ Meti) ->
    {ignore,lists:reverse("/|" ++ Meti)};

delete_filter(Credentials,Service,Meti) ->
    delete_item(Credentials,Service,lists:reverse(Meti)).

delete_item(Credentials,Service,Qitem) ->
    case identity_server:authorise(Credentials,Service,delete,{Qitem,[]}) of
	{ok,Actor} -> 
	    case attribute_server:delete(Qitem) of
		{ok,Xref} ->
		    clean_up_removal(Xref),
		    pattern_server:process(Actor,Service,delete,domain(Qitem),Qitem,[{"xref",Xref}]),
		    {ok,Xref};
		_ -> {error,error({"Could not delete resource",Qitem})}
	    end;
	{error,Actor,Why} ->
	    pattern_server:process(Actor,Service,autherror,domain(Qitem),Qitem,[{"autherror",Why}]),
	    {autherror,Why}
    end.

clean_up_removal(Xref) ->
    search_util:delete(Xref),
    tag_util:delete(Xref), 
    control_util:delete(location,Xref).
