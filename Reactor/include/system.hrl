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

-define(SINKS,"/sink").
-define(ERRORMATCHER,matcher).
-define(ERRORSINKER,sinker).
-define(DEFAULTIDADAPTOR,identity_adaptor).
-define(SUMCHARS,256). %% Todo this may now be redundant
-define(ALLPRIVS,"retrieve,update,delete,create,grant,revoke").
-define(LOGINFORM,"<form id='login' method='post'><input name='identity' value='identity''/><br/><input password='' type='password' name='password'/><input type='submit' value='login'/></form>").
-define(COOKIE,"UserId").
% 90 days cookie timeout.
-define(MAXAGE,7776000). 
% Summary text length for list views
-define(MAXCHARS,256).
% This is for administrative web functions, which can differ from the main REST server, the example here uses a domain only accessable locally on the system which is more secure. 
-define(AUDITFILE,"audit.log").
-define(DOCROOT,"www").
-define(PUBLIC,"/_public/").
-define(LOGIN,"/_login").
-define(LOGOUT,"/_logout").
-define(RESOURCES,"/_resources/").
-define(CONTEXT,"/reactor/"). % TODO change this to -define(ITEMS,"/_items/").
-define(SYSTEM,"_/queue/system").
-define(QUEUE,"_/queue").
-define(DOMAINS,"_").
-define(IDENTITIES,"_/id").
-define(ACLS,"_/acl").
-define(CONFIG,"config.dat").
-define(PROXY,"http://127.0.0.1:9080/").

%  -define(IDS,"Users/"). todo is this replication of identities -> redundant now
