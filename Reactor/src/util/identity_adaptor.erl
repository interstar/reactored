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

-module(identity_adaptor).
-include("schema.hrl").
-include("system.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([create/2,delete/1,authenticate/2,identify/1,indexes/0,create_identities/0,reset_identities/0]).

authenticate(Id,Pswd) ->
    F = fun() -> 
		qlc:e(qlc:q([X#identity.uri || X <- mnesia:table(identity),
						   X#identity.pswd =:= Pswd,
						  X#identity.email =:= Id]))
	end,
    case mnesia:transaction(F) of
	{atomic,[]} ->
	    {error,error("Could not authenticate identity " ++ Id)};
	{atomic,Uri} when is_list(hd(Uri)) ->
	    {ok,hd(Uri)};
	{atomic,Uri} ->
	    {ok,Uri}
    end.

identify(Token) ->
    F = fun() -> 
		qlc:e(qlc:q([X#identity.uri || X <- mnesia:table(identity),
						   X#identity.token =:= Token]))
	end,
    case mnesia:transaction(F) of
	{atomic,[]} ->
	    {error,error("Could not authenticate Token " ++ Token)};
	{atomic,Uri} when is_list(hd(Uri)) ->
	    {ok,hd(Uri)};
	{atomic,Uri} ->
	    {ok,Uri}
    end.

create(Uri,Attributes) ->
    {_,Email} = proplists:lookup("email",Attributes),
    {_,Nick} = proplists:lookup("nick",Attributes),
    {_,Pswd} = proplists:lookup("password",Attributes),
    Uid = uid(),
    F = fun() -> mnesia:write({identity,Uid,Uri,Email,Nick,Pswd,attribute:today()}) end,
    case mnesia:transaction(F) of
	{atomic,_} -> 
	    {ok,Uid};
	_->
	    {error,error("Could not create identity " ++ Uri)}
    end.

delete(Uri) ->
    F = fun() -> 
		case qlc:e(qlc:q([X#identity.id || X <- mnesia:table(identity),
						  X#identity.uri =:= Uri])) of
		    [] -> 
			{error};
		    Ids -> 
			Id = hd(Ids),
			mnesia:delete({identity,Id}),
			Id
		end
	end,
    case mnesia:transaction(F) of
	{atomic,Id} -> 
	    {ok,Id};
	_->
	    {error,error("Cannot not delete identity " ++ Uri)}
    end.

indexes() ->
    F = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(identity)]))
	end,
    mnesia:transaction(F).

create_identities() ->
    mnesia:delete_table(identity),
    mnesia:create_table(identity,[{attributes, record_info(fields,identity)}]).

reset_identities() ->
    mnesia:clear_table(identity).

uid() ->
    search_util:uid().

error(Error) ->
    error_logger:error_msg("Identity adaptor - Says Whoops ~p~n",[Error]),
    Error.
