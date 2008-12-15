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

{application,proxy,
	[{description,"Proxy Domain Matcher"},
	{vsn,"1.0"},
	{modules,[proxy_server,proxy_supervisor,proxy_app]},
	{registered,[proxy_server,proxy_supervisor]},
	{applications,[kernel,stdlib]},
	{mod,{proxy_app,[]}},
	{start_phases,[]},
	{env, [{proxy_url,"http://127.0.0.1:9080/"}]}
]}.
