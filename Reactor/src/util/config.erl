-module(config).
-include("schema.hrl").
-include("system.hrl").
-export([start/0,stop/0,debug/1,create_live/1,start/1]).

% Do this only once!
create_live(Config) ->
    create_live_storage(),
    initialise_live_storage(Config).

start() ->
    load_storage(),
    load_reactor().

debug(Config) ->
    create_test_storage(),
    load_reactor(),
    load_domain(Config).

stop() ->
    unload_reactor() ,
    unload_storage().
    
load_reactor() ->
    application:load(crypto),
    application:start(crypto),
    application:load(reactor),
    application:start(reactor),
    application:load(index),
    application:start(index).

unload_reactor() ->
    application:stop(index),
    application:unload(index),
    application:stop(reactor),
    application:unload(reactor),
    application:stop(crypto),
    application:unload(crypto).

load_storage() ->
    mnesia:start(),
    mnesia:wait_for_tables([domain,item,attribute,identity,tags,words,control,usession],1000).
    

create_test_storage() ->
    mnesia:start(),
    identity_adaptor:create_identities(),
    tag_util:create_tags(),
    search_util:create_words(),
    control_util:create_control(),
    storage_tests:create_store().

%% do this only once!!!
create_live_storage()->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(domain,[{attributes, record_info(fields,domain)},
				{record_name,domain},{disc_copies,[node()]}]),
    mnesia:create_table(item,[{attributes, record_info(fields,item)},
			      {record_name,item},{disc_copies,[node()]}]),
    mnesia:create_table(attribute,[{attributes,record_info(fields,attribute)},
				   {record_name,attribute},{disc_copies,[node()]}]),
    mnesia:create_table(identity,[{attributes, record_info(fields,identity)},
				  {record_name,identity},{disc_copies,[node()]}]),
    mnesia:create_table(tags,[{attributes, record_info(fields,tags)},
			      {record_name,tags},{disc_copies,[node()]}]),
    mnesia:create_table(words,[{attributes, record_info(fields,words)},
			       {record_name,words},{disc_copies,[node()]}]),
    mnesia:create_table(control,[{attributes, record_info(fields,control)},
				 {record_name,control},{disc_copies,[node()]}]),
    mnesia:create_table(usession,[{attributes, record_info(fields,usession)},
				 {record_name,usession},{disc_copies,[node()]}]),
    mnesia:stop().

start(Config)->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(domain,[{attributes, record_info(fields,domain)},
				{record_name,domain}]),
    mnesia:create_table(item,[{attributes, record_info(fields,item)},
			      {record_name,item}]),
    mnesia:create_table(attribute,[{attributes,record_info(fields,attribute)},
				   {record_name,attribute}]),
    mnesia:create_table(identity,[{attributes, record_info(fields,identity)},
				  {record_name,identity}]),
    mnesia:create_table(tags,[{attributes, record_info(fields,tags)},
			      {record_name,tags}]),
    mnesia:create_table(words,[{attributes, record_info(fields,words)},
			       {record_name,words}]),
    mnesia:create_table(control,[{attributes, record_info(fields,control)},
				 {record_name,control}]),
    mnesia:create_table(usession,[{attributes, record_info(fields,usession)},
				 {record_name,usession}]),
    load_reactor(),
    load_domain(Config).

%%  mnesia:create_table(usession,[{attributes, record_info(fields,usession)},
%% 				 {record_name,usession},
%% 				 {disc_copies,[node()]}]),

%% do this only once!!!
initialise_live_storage(Config) ->
    load_storage(),
    load_reactor(),
    load_domain(Config),
    unload_reactor(),
    unload_storage().

unload_storage() ->
    % backup mnesia?
    mnesia:stop().

load_domain(Config) ->
    case file:consult(Config) of
	{ok,Configs} ->
	    Conf = hd(Configs),
	    setup_domain(Conf#config.domain,Conf#config.email,Conf#config.password,Conf#config.token,Conf#config.branches);
	{error,Why} ->
	    error("Could not parse config file " ++  Config)
    end.

setup_domain(Domain,Email,Password,Token,Branches) ->
    System_Queue = ?DOMAIN ++ ?CONTEXT ++ ?SYSTEM,
    Domain_Queue = ?DOMAIN ++ ?CONTEXT ++ ?QUEUE,
    Domain_Admin = ?DOMAIN ++ ?CONTEXT ++ ?DOMAINS,
    %% Create queues for system and domains
    domain_server:create(System_Queue,matcher,?OWNER),
    domain_server:create(Domain_Queue,matcher,?OWNER),
    domain_server:create(Domain_Admin,matcher,?OWNER),
    % Create Identity branch for domain and founder with profile
    Identities = Domain ++ ?CONTEXT ++ ?IDENTITIES,
    Acls = Domain  ++ ?CONTEXT ++ ?ACLS,
    Founder = Identities ++ "/founder",

    attribute_server:create(System_Queue,"/",
			    [{"description","System Queue Branch"},
			     {"title","System Queue Index"},
			     {"status","fixed"},
			     {"type","trunk"}]),
    attribute_server:create(Domain_Queue,"/",
			    [{"description","Domain Queue Branch"},
			     {"title","Domain Queue Index"},
			     {"status","fixed"},
			     {"type","trunk"}]),
    attribute_server:create(Domain_Admin,"/",
			    [{"description","Domain Queue Branch"},
			     {"title","Domain Queue Index"},
			     {"status","fixed"},
			     {"type","trunk"}]),
    domain_server:create(Identities,identity,Founder), %% identity
    %% Create Domain Root - redundant
%%     attribute_server:create(Domain,"/",
%% 			    [{"description","Welcome"},
%% 			     {"title","Index"},
%% 			     {"type","trunk"}]),

    attribute_server:create(Identities,"/",
			    [{"description","Identities Branch"},
			     {"title","Identities Index"},
			     {"type","trunk"}]),
    identity_server:create(Identities,"/founder",
			   [{"email",Email},
			    {"nick","Founder"},
			    {"password",Password},
			   {"token",Token}]),
    attribute_server:create(Identities,"/founder",
			    [{"description","Founder's details"},
			     {"title","Founder's Profile"},
			     {"type","identity"}]),

    % Grant root permissions to founder and Identity controls
%%     identity_server:controls(qualified(Identities,"/founder"),rel3,grant,
%% 			     {qualified(Domain,"/") ,[ {"acl",?ALLPRIVS}] } ),
    actor_server:create_id_fork(Identities,"/founder",[{"title","Founder"}]),
    identity_server:controls(qualified(Identities,"/founder"),rel3,grant,
			     {qualified(Identities,"/") ,[ {"acl",?ALLPRIVS}] } ),
    identity_server:controls(qualified(Identities,"/founder"),rel3,grant,
			     {qualified(Identities,"/founder") ,[ {"acl",?ALLPRIVS}] } ),
    identity_server:controls(qualified(Identities,"/founder"),rel3,grant,
			     {qualified(Identities,"/founder/acl") ,[ {"acl",?ALLPRIVS}] } ),
    identity_server:controls(qualified(Identities,"/founder"),rel3,grant,
			     {qualified(Identities,"/founder/tags") ,[ {"acl",?ALLPRIVS}] } ),
    identity_server:controls(qualified(Identities,"/founder"),rel3,grant,
			     {qualified(Identities,"/founder/profile") ,[ {"acl",?ALLPRIVS}] } ),
    %% Give Founder domain administration privelages
    identity_server:controls(qualified(Identities,"/founder"),rel3,grant,
			     {qualified(Domain_Admin,"/") ,[ {"acl",?ALLPRIVS}] } ),
    %% Give Founder domain queue administration privelages
    identity_server:controls(qualified(Identities,"/founder"),rel3,grant,
			     {qualified(Domain_Queue,"/") ,[ {"acl",?ALLPRIVS}] } ),
    %% Give Founder system queue administration privelages
    identity_server:controls(qualified(Identities,"/founder"),rel3,grant,
			     {qualified(System_Queue,"/") ,[ {"acl",?ALLPRIVS}] } ),
    %% Give founder ACL manipulation privs
%%     identity_server:controls({uri,qualified(Identities,"/founder") },rel3,grant,
%% 			     {qualified(Acls,"/") ,[ {"acl",?ALLPRIVS}] }

    lists:foreach(fun(B) -> create_branch(Identities,Domain,Founder,B) end, Branches).

create_branch(Identities,Domain,Founder,{Name,Type,Matcher,Description}) ->
    Branch = Domain ++ ?CONTEXT ++ Name,
    domain_server:create(Branch,Matcher,Founder),
    attribute_server:create(Branch,"/",
			    [{"description",Description},
			     {"title",Name},
			     {"type",Type}]),
    identity_server:controls(qualified(Identities,"/founder"),rel3,grant,
			     {qualified(Branch,"/") ,[ {"acl",?ALLPRIVS}] } ).

qualified(Domain,Item) ->
    attribute:item_id(Domain,Item).

error(Error) ->
    error_logger:error_msg("Config module - Says Whoops ~p~n",[Error]),
    Error.

