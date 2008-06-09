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

{application,reactor,
	[{description,"Reactored Systems"},
	{vsn,"1.0"},
	{modules,[attribute_server,attribute,sink_server,action_server,monostable,trigger,queue_server,queue_util,domain_server,domain,pattern_server,pattern_util,actor_server,,identity_server,authenticator,reactor_app,reactor_app,reactor_supervisor]},
	{registered,[reactor_supervisor,attribute_server,action_server,sinker_server,queue_server,domain_server,pattern_server,actor_server,identity_server]},
	{applications,[kernel,stdlib]},
	{mod,{reactor_app,[]}},
	{start_phases,[]}
]}.
