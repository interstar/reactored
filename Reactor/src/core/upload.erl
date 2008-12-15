-module(upload).
-include("schema.hrl").
-include("system.hrl").
-define(DOSSEPERATOR,"\\").
-define(UNIXSEPERATOR,"/").
-export([store/1]).

store(Req) ->
    reactor_parse_form(Req).

reactor_parse_form(Req) ->
    mochiweb_multipart:parse_form(Req,fun reactor_file_handler/2).

reactor_file_handler(Filename,ContentType) ->
    Temp = uid() ++ ".temp",
    case file:open(Temp,[raw,write]) of
	{ok, File} ->
	    reactor_file_handler_1(Filename,ContentType,File,Temp);
	{error, Error} ->
	    error_logger:error_msg("Uploader Couldn't open ~p for writing, error: ~p~n",[Temp,Error]),
	    {error, Error}
    end.
    

reactor_file_handler_1(Filename,ContentType,File,Temp) ->
    fun(eof) ->
	    file:close(File),
            {Filename,ContentType,Temp};
       (Data) ->
	    file:write(File, Data),
            reactor_file_handler_1(Filename,ContentType,File,Temp)
    end.


% Utility functions for uploader
filename("/" ++ Path) -> 
    name_from_path(Path,?UNIXSEPERATOR);
filename([Drive|Path]) when hd(Path) =:= $: -> 
    name_from_path(Path,?DOSSEPERATOR).

name_from_path(Path,Seperator) ->
    hd(list:reverse(string:tokens(Path,Seperator))).

safe(Name) ->
    {_,S,_} = regexp:gsub(Name,"[^a-zA-Z0-9\-_\.]+","_"),
    S.

name_to_path(Name,Parent) ->
    case create_path(string:tokens(Parent,"/"),config_server:path(docroot) ++ ?RESOURCES) of
	{ok,Path} ->
	    {ok,Path ++ "/" ++ uid() ++ extension(Name)};
	{error,Reason} ->
	    {error,Reason}
    end.

create_path([Item|Items],Path) ->
    case file:make_dir(Path ++ Item) of 
	ok ->
	    create_path(Items,Path ++ "/" ++ Item);
	{error,eexist} ->
	    create_path(Items,Path ++ "/" ++ Item);
	{error,Reason} ->
	    error_logger:error_msg("Uploader could not create dir: ~p~n",[{Path,Item,Reason}]),
	    {error,Reason}
    end;
create_path([],Path) ->
    {ok,Path}.

extension(Name) ->
    case string:rchr(Name,$.) of
	0 ->
	    "";
	_ ->
	    hd(list:reverse(string:tokens(Name,".")))
    end.

uid() ->
    attribute:today().
