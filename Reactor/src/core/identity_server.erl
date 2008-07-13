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
%%% File    : identity_server.erl
%%% Author  : Alan Wood <awood@alan-woods-macbook.local>
%%% Description : 
%%%
%%% Created :  1 Jun 2008 by Alan Wood <awood@alan-woods-macbook.local>
%%%-------------------------------------------------------------------
-module(identity_server).
-include("schema.hrl").
-include("system.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([start/0,stop/0]).
-export([authenticate/2,create/3,delete/1,tag/3,filter/4,authorise/4,controls/4,actor_from_token/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
authenticate(Id,Pswd) ->
    gen_server:call(?MODULE,{authenticate,Id,Pswd}).

create(Domain,Item,Attributes) ->
    gen_server:call(?MODULE,{create,Domain,Item,Attributes}).

delete(Item) ->
    gen_server:call(?MODULE,{delete,Item}).

tag(Author,Resource,Tags) ->
    gen_server:call(?MODULE,{tag,Author,Resource,Tags}).

filter(Meta,Actor,Domain,Q) ->
    gen_server:call(?MODULE,{filter,Meta,Actor,Domain,Q}).

authorise(Credentials,Service,Command,Request) ->
    gen_server:call(?MODULE,{authorise,Credentials,Service,Command,Request}).

%% ACL controls
controls(Iuri,Service,Command,Request) ->
    gen_server:call(?MODULE,{controls,Iuri,Service,Command,Request}).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start() -> start_link().
stop() -> gen_server:call(?MODULE,stop).
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
handle_call({authenticate,Id,Pswd}, _From, State) ->
    Reply = case apply(identity_adaptor(),authenticate,[Id,Pswd]) of
	{ok,Actor} -> 
		    {ok,Actor};
	Bad -> 
	    % Identity module error/ not loaded?
	    {error,error({"Identity module returned authentication error, or could, be a missing module. Reported error ",Bad})}
    end,
    {reply, Reply, State};

handle_call({create,Domain,Item,Attributes}, _From, State) ->
    Reply = case apply(identity_adaptor(),create,[qualified(Domain,Item),Attributes]) of
	{ok,Id} -> 
	    {ok,Id};
	Bad -> 
	    % Identity module error/ not loaded?
	    {error,error({"Identity module return error, could, be a missing module",Bad})}
    end,
    {reply, Reply, State};
handle_call({delete,Item}, _From, State) ->
    Reply = case apply(identity_adaptor(),delete,[Item]) of
		{ok,Id} -> % Delete privileges associated with this participant
		    index_server:delete_controls(Item),
		    {ok,Id};
		Bad -> 
		% Identity module error/ not loaded?
		    {error,error({"Identity module return error, could, be a missing module",Bad})}
    end,
    {reply, Reply, State}; 

handle_call({tag,Author,Resource,Tags}, _From, State) ->
    Reply = index_server:tag(Author,Resource,Tags),
    {reply, Reply, State};
handle_call({filter,tagged,{uri,Author},_Domain,{Tags,Author}}, _From, State) ->
    Reply = summarise(index_server:tagged(Tags,get_index_id(Author))),
    {reply, Reply, State};
handle_call({filter,tagged,Actor,_Domain,Tags}, _From, State) ->
    Reply = screen(Actor,index_server:tagged(Tags)),
    {reply, Reply, State};
handle_call({filter,search,Actor,_Domain,Q}, _From, State) ->
    Reply = screen(Actor,index_server:search(Q)),
    {reply, Reply, State};
%% Todo Need to double check the restrictions on this action
handle_call({filter,profile,_Actor,_Domain,Profile}, _From, State) ->
    Reply = summarise(index_server:profile(Profile)),
    {reply, Reply, State};
handle_call({filter,q,Actor,Domain,Attributes}, _From, State) ->
    Reply = screen(Actor,Domain, Attributes),
    {reply, Reply, State};
%% this needs to point to index_server
handle_call({filter,graph,Actor,Domain,{Uri,Attributes}}, _From, State) ->
    Reply = screen(Actor,Domain, Uri, Attributes),
    {reply, Reply, State};

handle_call({authorise,Credentials,Service,Command,Request}, _From, State) ->
%%     {Uri,Attribs} = Request,
%%     Reply = case attribute_server:retrieve({uri,Uri}) of
%% 		{ok,[]} -> 
%% 		    {error,Uri,"Authorisation, could not identify location " ++ Uri};
%% 		{ok,[It]} ->
%% 		    case check_access(Credentials,Service,Command,{It#item.item,Attribs}) of
%% 			{ok,Iuri}-> 
%% 			    {ok,Iuri};
%% 			{error,Iuri,Why} -> % Authosrisation failure
%% 			    {error,error({"could not authorise. " ++ Iuri}),Why}
%% 		    end
%% 	    end,
    Reply = case check_access(Credentials,Service,Command,Request) of
		{ok,Iuri}-> 
		    {ok,Iuri};
		{error,Iuri,Why} -> % Authosrisation failure
		    {error,error({"could not authorise. " ++ Iuri}),Why}
	    end,
    {reply, Reply, State};

%% Todo fix - not credentials its the actor one is granting privs to!!
handle_call({controls,Iuri,_Service,grant,Request}, _From, State) ->
    Reply = grant(Iuri,Request),
    {reply, Reply, State};
handle_call({controls,Iuri,_Service,revoke,Request}, _From, State) ->
    Reply = revoke(Iuri,Request),
    {reply, Reply, State};
handle_call({controls,Iuri,_Service,inherit,Request}, _From, State) ->
    Reply = inherit(Iuri,Request),
    {reply, Reply, State};


handle_call(stop, _From, State) ->
    {stop,normal,stopped, State};
handle_call(_Request, _From, State) ->
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

%% This stuff needs to be much clearer visually (seperate files)
check_access({annonymous},_Service,Command,_Request) ->
    Reason = "Credentials (annonymous) not suitable, authentication failure",
    {error,"/Users/annonymous",atom_to_list(Command) ++ " failed dues to :"  ++ Reason};
%% A participant can do all of this things with their own profile
check_access({uri,Iuri},_Service,retrieve,{Iuri,_Attributes}) -> {ok,Iuri};
check_access({uri,Iuri},_Service,q,{Iuri,_Attributes}) -> {ok,Iuri};

check_access({uri,Iuri},_Service,Command,{Uri,Attributes}) -> 
    check_privileges(Iuri,Uri,Command,get_privileges(Iuri,{Uri,Attributes}));
check_access({token,Token},Service,Command,{Uri,Attributes}) -> 
    case actor_from_token(Token) of
	annonymous ->
	    {error,"",error({"Badly formed Credentials for command ",atom_to_list(Command) ++ ",Token not recognised ",Service,Command,Uri,Attributes})};
	Iuri ->
	    check_privileges(Iuri,Uri,Command,get_privileges(Iuri,{Uri,Attributes}))
    end;
check_access({_,I},Service,Command,Request) ->
    {error,I,error({"Badly formed Credentials for command ",atom_to_list(Command) ++ ",Credentials not recognised ",Service,Command,Request})}.

get_privileges(Iuri,{Uri,_Attributes}) ->
    index_server:controls(Iuri,Uri).

check_privileges(Iuri,_Uri,Command,[Command|_Controls]) ->
    {ok,Iuri};
check_privileges(Iuri,Uri,Command,[_Control|Controls]) ->
    check_privileges(Iuri,Uri,Command,Controls);
check_privileges(Iuri,Uri,Command,[]) ->
    {error,Iuri,"No control allowing " ++ Iuri ++ " to " ++ atom_to_list(Command)  ++ " " ++ Uri}.

screen(Actor,Lids) ->
    summarise(index_server:controls(Actor,retrieve,Lids)).

screen(Actor,Domain, Attributes) ->
    Results = case attribute_server:q(Domain,Attributes) of
		  {ok,[]} -> [];
		  {ok,Items} -> Items
	      end,
    Uris = index_server:controls(Actor,retrieve,[It#item.xref || It <- Results]),
    lists:filter(fun(I) -> lists:member(I#item.xref,Uris) end,Results).

screen(Actor,Domain, Uri, Attributes) ->
    Results = case attribute_server:graph(Domain, Uri, Attributes) of
		  {atomic,{_Item,Items}} -> 
		      Items;
		  _ -> 
		      error({"No results for graph query ",Domain, Uri, Attributes}),
		      []
	      end,
    Uris = index_server:controls(Actor,retrieve,[It#item.created || It <- Results]),
    lists:filter(fun(I) -> lists:member(I#item.item,Uris) end,Results).

grant(Iuri,{Uri,Attributes}) ->
    index_server:grant(Iuri,Uri,get_acl(Attributes)).

revoke(Iuri,{Uri,Attributes}) ->
    index_server:revoke(Iuri,Uri,get_acl(Attributes)).

inherit(Iuri,{Uri,Attributes}) ->
    {_,Parent} = proplists:lookup("parent",Attributes),
    index_server:inherit(Iuri,Uri,Parent).

identity_adaptor() ->
    ?DEFAULTIDADAPTOR.

summarise(Locations) -> 
    summarise(Locations,[]).

summarise([{_id,_iid,Lid,_types}|Locations],Summary) -> % Profile returns control records
    case attribute_server:retrieve(Lid) of
	{ok,[]} -> % not located, should not happen
	    summarise(Locations,Summary);
	{ok,[Item]} ->
	    summarise(Locations,[Item|Summary])
    end;
summarise([Lid|Locations],Summary) -> % Tagged Auth
    case attribute_server:retrieve(Lid) of
	{ok,[]} -> % not located, should not happen
	    summarise(Locations,Summary);
	{ok,[Item]} ->
	    summarise(Locations,[Item|Summary]);
	{ok,Items} -> % Todo shouldn't happen but unporcessed xref can be duped by it's sink entries! This is a hack, need to fix the fact that xrefs arn't unique!!
	    summarise(Locations,[original_item(Items)|Summary])
	    
    end;
summarise([],Summary) ->
    Summary.

original_item([Item|Items]) ->
    case string:str(Item#item.item, "/sink/") of
	0 ->
	    Item;
	_ ->
	    original_item(Items)
    end;
original_item([]) -> [].    

%% redundant now    
%% summarise([Location|Locations],Summary) ->
%%     L = attribute_server:retrieve(Location, basic)
%%     summarise(Locations,[{L#item.title,limit(L#item.descrption),L#item.uri} | Summary])


%% limit(Text) ->
%%     sublist(markup_stripper:parse(Text), ?SUMCHARS).

%% domain(Qitem) ->
%%     attribute:domain_from_qitem(Qitem).

%% qualified(Domain,Item) ->
%%     attribute:item_id(Domain,Item).

get_acl(Attributes) ->
    case proplists:lookup("acl",Attributes) of
	none -> [];
	{_,undefined} -> []; 
	{_,Acl} -> lists:map(fun list_to_atom/1,string:tokens(Acl,","))
    end.

%% Todo implement tokens
actor_from_token(Token) ->
    Ia = identity_adaptor(),
    case Ia:identify(Token) of
	{error,_Error} ->
	    annonymous;
	{ok,Uri} ->
	    Uri
    end.

get_index_id(Uri) ->
    index_server:get_index_id(Uri).

qualified(Domain,Item) ->
    attribute:item_id(Domain,Item).

error(Error) ->
    error_logger:error_msg("Identity server - Says Whoops ~p~n",[Error]),
    Error.

