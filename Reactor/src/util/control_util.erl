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

-module(control_util).
-include("schema.hrl").
-include("system.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([add/2,add/3,remove/2,remove/3,delete/1,delete/2,q/2,q/3,indexes/0,create_control/0,reset_control/0]).
%-record(control,{id,iid,lid,controls})

add(Id,Control) -> %short cut when control.uid known
    F = fun() -> 
		[Cntrl] = mnesia:read({control,Id}),
		case lists:member(Control,Cntrl#control.types) of
		    true -> 
			Cntrl#control.id;
		    false -> 
			Ucntrls = [Control|Cntrl#control.types],
			Ucntrl = Cntrl#control{types=Ucntrls},
			mnesia:write(Ucntrl),
			Cntrl#control.id
		end
	end,
    case mnesia:transaction(F) of
	{atomic,Id} -> 
	    {ok,Id};
	_ -> 
	    {error,error({"Cannot add " ++ atom_to_list(Control) ++", control not found for id ", Id})}
    end.

add(Iid,Lid,Control) when is_list(Control)->
    Uid = uid(),
    F = fun() -> 
		case qlc:e(qlc:q([X || X <- mnesia:table(control),
						  X#control.iid =:= Iid,
						  X#control.lid =:= Lid])) of
		    [] -> 
			mnesia:write({control,Uid,Iid,Lid,Control}),
			Uid;
		    Controls -> 
			Cntrl = hd(Controls),
			mnesia:write(Cntrl#control{types=lists:usort(Control ++ Cntrl#control.types)}),
			Cntrl#control.id
		end
	end,
    case mnesia:transaction(F) of
	{atomic,Id} -> 
	    {ok,Id};
	_ -> 
	   {error,error({"Cannot add controls ",Control,Iid, Lid})}
    end;

add(Iid,Lid,Control) ->
    Uid = uid(),
    F = fun() -> 
		case qlc:e(qlc:q([X || X <- mnesia:table(control),
						  X#control.iid =:= Iid,
						  X#control.lid =:= Lid])) of
		    [] -> 
			mnesia:write({control,Uid,Iid,Lid,[Control]}),
			Uid;
		    Controls -> 
			Cntrl = hd(Controls),
			case lists:member(Control,Cntrl#control.types) of
			    true -> 
				Cntrl#control.id;
			    false -> 
				Ucntrls = [Control|Cntrl#control.types],
				Ucntrl = Cntrl#control{types=Ucntrls},
				mnesia:write(Ucntrl),
				Cntrl#control.id
			end
		end
	end,
    case mnesia:transaction(F) of
	{atomic,Id} -> 
	    {ok,Id};
	_ -> 
	   {error,error({"Cannot add " ++ atom_to_list(Control) ++", control not found for  ",Iid, Lid})}
    end.

% Delete entire control record
delete(Id) ->
    F = fun() -> 
		mnesia:delete({control,Id})
	end,
    case mnesia:transaction(F) of
	{atomic,ok} -> 
	    ok;
	_ -> 
	    {error,error({"Cannot remove controls for controls  id ", Id})}
    end.
% Delete control records for given location
delete(location,Lid) ->
    F = fun() -> 
		case qlc:e(qlc:q([X || X <- mnesia:table(control),
						  X#control.lid =:= Lid])) of
		    [] -> 
			{error};
		    Controls -> 
			lists:map(fun(Cntrl) -> mnesia:delete({control,Cntrl#control.id}) end,Controls),
			{ok,length(Controls)}
		end
	end,
    case mnesia:transaction(F) of
	{atomic,{ok,Deleted}} -> 
	    {ok,Deleted};
	_-> 
	    {error,error({"Cannot remove controls not found for lid ",Lid})}
    end;
% Delete control records for given identity
delete(identity,Iid) ->
    F = fun() -> 
		case qlc:e(qlc:q([X || X <- mnesia:table(control),
						  X#control.iid =:= Iid])) of
		    [] -> 
			{error};
		    Controls -> 
			lists:foreach(fun(Cntrl) -> mnesia:delete({control,Cntrl#control.id}) end,Controls),
			{ok,length(Controls)}
		end
	end,
    case mnesia:transaction(F) of
	{atomic,{ok,Deleted}} -> 
	    {ok,Deleted};
	_-> 
	    {error,error({"Cannot remove controls not found for iid ",Iid})}
    end;
% Delete entire control record for given identity and location
delete(Iid,Lid) ->
    F = fun() -> 
		case qlc:e(qlc:q([X || X <- mnesia:table(control),
						  X#control.iid =:= Iid,
						  X#control.lid =:= Lid])) of
		    [] -> 
			{error};
		    Controls -> 
			Cntrl = hd(Controls),
			mnesia:delete({control,Cntrl#control.id})
		end
	end,
    case mnesia:transaction(F) of
	{atomic,ok} -> 
	    ok;
	_-> 
	    {error,error({"Cannot remove controls not found for ",Iid,Lid})}
    end.

% remove specific control from control record, short version
remove(Id,Control) ->
    F = fun() -> 
		[Cntrl] = mnesia:read({control,Id}),
		case lists:member(Control,Cntrl#control.types) of
		    false -> 
			Cntrl#control.id;
		    true -> 
			Ucntrls = lists:delete(Control, Cntrl#control.types),
			Ucntrl = Cntrl#control{types=Ucntrls},
			mnesia:write(Ucntrl),
			Cntrl#control.id
		end
	end,
    case mnesia:transaction(F) of
	{atomic,Id} -> 
	    {ok,Id};
	_ -> 
	    {error,error({"Cannot remove " ++ atom_to_list(Control) ++", control not found for id ",Id})}
    end.

remove(Iid,Lid,Control) when is_list(Control) ->
    F = fun() -> 
		case qlc:e(qlc:q([X || X <- mnesia:table(control),
						  X#control.iid =:= Iid,
						  X#control.lid =:= Lid])) of
		    [] -> 
			{error};
		    Controls -> 
			Cntrl = hd(Controls),
			mnesia:write(Cntrl#control{types=Cntrl#control.types -- Control}),
			Cntrl#control.id
		end
	end,
    case mnesia:transaction(F) of
	{atomic,Id} -> 
	    {ok,Id};
	_ -> 
	    {error,error({"Cannot remove controls ",Control,Iid,Lid})}
    end;

% remove specific control from control record
remove(Iid,Lid,Control) ->
    F = fun() -> 
		case qlc:e(qlc:q([X || X <- mnesia:table(control),
						  X#control.iid =:= Iid,
						  X#control.lid =:= Lid])) of
		    [] -> 
			{error};
		    Controls -> 
			Cntrl = hd(Controls),
			case lists:member(Control,Cntrl#control.types) of
			    false -> 
				Cntrl#control.id;
			    true -> 
				Ucntrls = lists:delete(Control, Cntrl#control.types),
				Ucntrl = Cntrl#control{types=Ucntrls},
				mnesia:write(Ucntrl),
				Cntrl#control.id
			end
		end
	end,
    case mnesia:transaction(F) of
	{atomic,Id} -> 
	    {ok,Id};
	_ -> 
	    {error,error({"Cannot remove " ++ atom_to_list(Control) ++", control not found for ",Iid,Lid})}
    end.

% both return either [] or [Qitems]
q(location,Lid) -> % returns all controls for that location
    F = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(control),
						  X#control.lid =:= Lid]))
	end,
    case mnesia:transaction(F) of
	{atomic,[]} -> 
	    [];
	{atomic,Results} -> 
	    lists:flatten(Results)
    end;

q(identity,Iid) -> % returns all controls for that Identity
    F = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(control),
						  X#control.iid =:= Iid]))
	end,
    case mnesia:transaction(F) of
	{atomic,[]} -> 
	    [];
	{atomic,Results} -> 
	    lists:flatten(Results)
    end;

% both return either [] or [Qitems]
q(Iid,Lid) -> % returns list of controls i.e. [retrieve,create,update,delete,]
    F = fun() -> 
		qlc:e(qlc:q([X#control.types || X <- mnesia:table(control),
						  X#control.iid =:= Iid,
						  X#control.lid =:= Lid]))
	end,
    case mnesia:transaction(F) of
	{atomic,[]} -> 
	    [];
	{atomic,Results} -> 
	    lists:flatten(Results)
    end.

q(Iid,Control,Lids) -> % returns Lids that have Command entries  with Iduris of iid
    F = fun() -> 
		lists:map(fun(Lid) -> controls(Iid,Control,Lid) end,Lids)
	end,
    case mnesia:transaction(F) of
	{atomic,[]} -> 
	    [];
	{atomic,Results} -> 
	    lists:flatten(Results)
    end.

controls(Iid,Control,Lid) ->
    case qlc:e(qlc:q([X || X <- mnesia:table(control),
			   X#control.iid =:= Iid,
			   X#control.lid =:= Lid])) of
	[] ->
	    [];
	Controls  ->
	    [Cntrl#control.lid || Cntrl <- lists:filter(fun(C)-> lists:member(Control,C#control.types) end, Controls)]
    end.

    

indexes() ->
    F = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(control)]))
	end,
    mnesia:transaction(F).


create_control() ->
    mnesia:delete_table(control),
    mnesia:create_table(control,[{attributes, record_info(fields,control)}]).

reset_control() ->
    mnesia:clear_table(control).
					    
uid() -> 
    {Meg,Sec,Mic} = now(),
    Meg * 1000000000000 + Sec * 1000000 + Mic.

error(Error) ->
    error_logger:error_msg("Control utility module - Says Whoops ~p~n",[Error]),
    Error.
