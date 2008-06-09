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

-module(plugin).
-export([get_server/1,load/1,start/1,unload/1]).

get_server(Sinkername) -> % the matchers are OTP applications we need to call match on the application server
    list_to_atom(atom_to_list(Sinkername) ++ "_server").

load(Plugin) ->
    %% Start matcher OTP app, if module fails to load/start use error app instead
    case application:load(Plugin) of
	ok -> 
	    start(Plugin);
	{error,{already_loaded,Plugin}} -> 
		   start(Plugin),
		   {ok,Plugin};
	{error,_Why} -> 
	    error({"Plugin not loaded",Plugin}),
	    {error}  %flag an error?
    end.

start(Plugin) ->
    case application:start(Plugin) of
	ok -> {ok,Plugin};
	{error,{already_started,matcher}} -> Plugin;
	{error,_Why} -> 
	    error({"Plugin not started",Plugin}),
	   {error} %flag an error?
    end.

unload({_,Plugin}) ->
    case application:stop(Plugin) of
	ok -> 
	    case application:unload(Plugin) of
		ok -> void;
		_  -> error({"Could not unload ",Plugin})
	    end;
	_  ->
	     error({"Could not stop plugin:",Plugin})
    end.

error(Error) ->
    error_logger:error_msg("plugin module - Says Whoops ~p~n",[Error]),
    Error.
