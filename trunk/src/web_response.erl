%%%----------------------------------------------------------------------
%%% Author:		Alireza Pazirandeh, Yavor Paunov
%%% Desc.:		generating teh xml representing the state of
%%				the application
%%%----------------------------------------------------------------------


-module(web_response).

-export([get_data_xml/1, get_files_xml/1]).
-include("torrent_status.hrl").
-include("torrent_db_records.hrl").
get_files_xml(Files) ->
	Header = 
		"<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
		<rows>",
	Body = get_body_files(Files, []),
	Footer =  "</rows>",
	Data = Header ++ Body ++ Footer,
	XmlData = lists:flatten(Data),
	iolist_to_binary(XmlData).
%%----------------------------------------------------------------------
%% Function:	get_data_xml/1
%% Purpose:		Constructs the data to be sent in response to the GET request. 
%% Args:		Filter/binary
%% Returns:     binary
%%----------------------------------------------------------------------
get_data_xml(Filter) ->
	Header = 
		"<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
		<rows>
		<page>1</page>
		<total>1</total>",
	Body = get_body(get_data(Filter), []),
	Footer = 
		"</rows>",
	Data = Header ++ Body ++ Footer,
	XmlData = lists:flatten(Data),
	iolist_to_binary(XmlData).
    
get_body_files([], Body) ->
	Body;
get_body_files([H|T], Body) ->
	{Name, Path} = file_split:path_create(H#file.path, ""),
	io:fwrite("File: ~p~n", [H]),
    SizeKB = utils:bytes_to_kbytes(H#file.length),
    case SizeKB > 1024 of
        true -> Size = io_lib:format("~.2f",[utils:kbytes_to_mbytes(SizeKB)]) ++ " MB";
        _ -> Size = io_lib:format("~.2f",[SizeKB]) ++ " KB"
    end,
    
	Row = "<row>" ++
	"<name>" ++ Path ++ Name ++ "</name>" ++
	"<size>" ++ Size ++ "</size>" ++
	"<done>" ++ io_lib:format("~.2f",[(H#file.length_complete/H#file.length)*100]) ++ "%</done>" ++
	"</row>",
	get_body_files(T, Body ++ Row).
%%----------------------------------------------------------------------
%% Function:	get_data_xml/1
%% Purpose:		Creates the body of the data to be sent in response. 
%% Args:		List/list, Body/string
%% Returns:     binary
%%--------------
get_body([], Body) ->
	Body;
get_body([H|T], Body) ->
    SizeKB = utils:bytes_to_kbytes(H#torrent_status.size),
    if
        SizeKB > 1024 * 1024 ->Size = io_lib:format("~.2f",[utils:kbytes_to_gbytes(SizeKB)]) ++ " GB";
        SizeKB > 1024 -> Size = io_lib:format("~.2f",[utils:kbytes_to_mbytes(SizeKB)]) ++ " MB";
        true -> Size = io_lib:format("~.2f",[SizeKB]) ++ " KB"
    end,
    
    UppedKB = utils:bytes_to_kbytes(H#torrent_status.uploaded),
    case UppedKB > 1024 of
        true -> Upped = io_lib:format("~.2f",[utils:kbytes_to_mbytes(UppedKB)]) ++ " MB";
        _ -> Upped = io_lib:format("~.2f",[UppedKB]) ++ " KB"
    end,
    
    DSpeedKB = utils:bytes_to_kbytes(H#torrent_status.downspeed),
    case DSpeedKB > 1024 of
        true -> DSpeed = io_lib:format("~.2f",[utils:kbytes_to_mbytes(DSpeedKB)]) ++ " MB/s";
        _ -> DSpeed = io_lib:format("~.2f",[DSpeedKB]) ++ " KB/s"
    end,
    
    USpeedKB = utils:bytes_to_kbytes(H#torrent_status.upspeed),
    case USpeedKB > 1024 of
        true -> USpeed = io_lib:format("~.2f",[utils:kbytes_to_mbytes(USpeedKB)]) ++ " MB/s";
        _ -> USpeed = io_lib:format("~.2f",[USpeedKB]) ++ " KB/s"
    end,    
    case H#torrent_status.downspeed of
        0.0 ->  ETA = "N/A";
        _   ->  
            ETABaseSec = round((H#torrent_status.size - H#torrent_status.downloaded) / H#torrent_status.downspeed),
            ETADays = ETABaseSec div 86400,
            Rem1 = ETABaseSec rem 86400,
            ETAHours = Rem1 div 3600,
            Rem2 = Rem1 rem 3600,
            ETAMinutes = Rem2 div 60,
            Rem3 = Rem2 rem 60,
            ETASeconds = Rem3,
            ETA = io_lib:format("~2..0B:~2..0B:~2..0B:~2..0B", [ETADays, ETAHours, ETAMinutes, ETASeconds])
    end,
    
	NewRow = 
		"<row status=\"" ++ atom_to_list(H#torrent_status.status) ++ "\" id=\"" ++ info_hash:to_hex(H#torrent_status.info_hash) ++ "\">
			<cell><![CDATA[" ++ binary_to_list(H#torrent_status.name) ++ "]]></cell>
			<cell><![CDATA[" ++ Size ++ "]]></cell>
			<cell><![CDATA[" ++       
                "   <div id='progressbar' \">
                        <div id='progressMade' style=\"width:" ++ io_lib:format("~.2f",[(H#torrent_status.downloaded/H#torrent_status.size)* 100]) ++ "%;\"\>
                    </div>
                    <div id='progressText' >" ++ io_lib:format("~.2f",[(H#torrent_status.downloaded/H#torrent_status.size)*100]) ++ "%</div>
            </div>]]></cell>
			<cell><![CDATA[" ++ atom_to_list(H#torrent_status.status) ++ "]]></cell>
			<cell><![CDATA[" ++ integer_to_list(H#torrent_status.connected_peers) ++ " (" ++ integer_to_list(H#torrent_status.peers) ++ ")]]></cell>
			<cell><![CDATA[" ++ DSpeed ++ "]]></cell>
            <cell><![CDATA[" ++ USpeed ++ "]]></cell>
			<cell><![CDATA[" ++ ETA ++ "]]></cell>
			<cell><![CDATA[" ++ Upped ++ "]]></cell>
		</row>",
	get_body(T, NewRow ++ Body).
%%----------------------------------------------------------------------
%% Function:	get_data_xml/1
%% Purpose:		Constructs the data to be sent in response to the GET request. 
%% Args:		Filter/binary
%% Returns:     binary
%%--------------
get_data(<<"all">>) ->
    com_central:get_all_torrents();
get_data(Filter) ->
	Data = com_central:get_all_torrents(),
	FilterFun = fun(Record) -> Record#torrent_status.status == binary_to_atom(Filter, utf8) end,
	lists:filter(FilterFun, Data).