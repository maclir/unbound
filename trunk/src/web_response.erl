%%%----------------------------------------------------------------------
%%% Author:		Alireza Pazirandeh
%%% Desc.:		generating teh xml representing the state of
%%				the application
%%%----------------------------------------------------------------------


-module(web_response).

-export([get_data_xml/1]).
-include("torrent_status.hrl").
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
    case SizeKB > 1024 of
        true -> Size = io_lib:format("~.2f",[utils:kbytes_to_mbytes(SizeKB)]) ++ " MB";
        _ -> Size = io_lib:format("~.2f",[SizeKB]) ++ " KB"
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
    io:format("size ~p\n downloaded ~p\n downspeed ~p\n", [H#torrent_status.size, H#torrent_status.downloaded, H#torrent_status.downspeed]),
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
            ETA = io_lib:format("~p:~p:~p:~p", [ETADays, ETAHours, ETAMinutes, ETASeconds])
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
			<cell><![CDATA[" ++ integer_to_list(H#torrent_status.uploaded) ++ "]]></cell>
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

%% temp_get_data() ->
	% Row1 = #torrent_status{info_hash = "12fe3465ef7265238767", priority = 1, name = "S01E01", size = 14,  status = "Stopped", peers = 4, downspeed = 34, upspeed = 640, eta = 101212, uploaded = 15},
	% Row2 = #torrent_status{info_hash = "12ae213465ef726523ae", priority = 2, name = "S01E02", size = 12, status = "Downloading", peers = 14, downspeed = 34, upspeed = 640, eta = 101212, uploaded = 15},
	% Row3 = #torrent_status{info_hash = "43fe34aeb4c7e654f834", priority = 3, name = "S01E03", size = 23, status = "Seeding", peers = 40, downspeed = 34, upspeed = 640, eta = 101212, uploaded = 15},
	% [Row1, Row2, Row3].
