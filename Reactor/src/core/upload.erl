-module(upload).
-include("schema.hrl").
-include("system.hrl").
-define(DOSSEPERATOR,"\\").
-define(UNIXSEPERATOR,"/").
-export([store/2,filename/1,name_to_path/2]).

store(Path,Req) ->
    reactor_parse_form(Path,Req).

reactor_parse_form(Path,Req) ->
    io:format("Uploader file handler uploading to ~s~n",[Path]),
    case mochiweb_multipart:parse_form(Req,fun reactor_file_handler/2) of
	[{_Field1,{Filename,ContentType,Temp}},{_Field2,Title},{_Submit,_Submitval}] -> 
	    move_upload(Filename,Path,Temp,ContentType,Title);
	[{_Field2,Title},{_Field1,{Filename,ContentType,Temp}},{_Submit,_Submitval}] -> 
	    move_upload(Filename,Path,Temp,ContentType,Title);
	[{_Field2,Title},{_Field1,{Filename,ContentType,Temp}}] -> 
	    move_upload(Filename,Path,Temp,ContentType,Title);
	Res -> {error,"Unknown error",Res}
    end.

move_upload(Filename,Path,Temp,ContentType,Title) ->
    io:format("Moving uploaded file ~s~n",[Filename]),
    case name_to_path(filename(Filename),Path) of
	{ok,Dest} ->
	    case file:rename(Temp, Dest) of
		ok -> 
		    {Type,_} = ContentType,
		    {ok,Dest,Title,Dest -- config_server:path(docroot),Type};
		{error,Error} -> 
		    {error, "File upload internal transfer failed : " ++ Dest ++ ", Error : " ++ atom_to_list(Error)}
	    end;
	{error,Error} ->
	    {error, "File upload failed : " ++ Filename ++ ", Reason : " ++ atom_to_list(Error)}
    end.

reactor_file_handler(Filename,ContentType) ->
    Temp = config_server:path(temp) ++ uid() ++ ".tmp",
    io:format("Creating temp upload file ~s~n",[Temp]),
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
filename("./" ++ Path) ->
    name_from_path(Path,?UNIXSEPERATOR);
filename("../" ++ Path) ->
    name_from_path(Path,?UNIXSEPERATOR);
filename([_Drive|Path]) when hd(Path) =:= $: -> 
    name_from_path(Path,?DOSSEPERATOR);
filename(Path) ->
    name_from_path(Path,?UNIXSEPERATOR).

name_from_path(Path,Seperator) ->
    hd(lists:reverse(string:tokens(Path,Seperator))).

safe(Name) ->
    {_,S,_} = regexp:gsub(Name,"[^a-zA-Z0-9\-_\.]+","_"),
    S.

name_to_path(Name,Parent) ->
    case create_path(string:tokens(Parent,"/"),rtl(config_server:path(docroot)) ++ rtl(?RESOURCES)) of
	{ok,Path} ->
	    {ok,Path ++ "/" ++ uid() ++ extension(Name)};
	{error,Reason} ->
	    {error,Reason}
    end.

create_path([Item|Items],Path) ->
    case file:make_dir(Path ++ "/" ++ Item) of 
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
	    [$.|hd(lists:reverse(string:tokens(Name,".")))]
    end.

uid() ->
    attribute:today().

rtl(L) ->
    lists:reverse(tl(lists:reverse(L))).
