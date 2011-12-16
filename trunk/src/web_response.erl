%%%----------------------------------------------------------------------
%%% Author:		Alireza Pazirandeh
%%% Desc.:		generating teh xml representing the state of
%%				the application
%%%----------------------------------------------------------------------
-module(web_response).

-export([get_data_xml/1]).
-include("torrent_status.hrl").

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

get_body([], Body) ->
	Body;
get_body([H|T], Body) ->
	NewRow = 
		"<row status=\"" ++ atom_to_list(H#torrent_status.status) ++ "\" id=\"" ++ info_hash:to_hex(H#torrent_status.info_hash) ++ "\">
			<cell><![CDATA[" ++ integer_to_list(H#torrent_status.priority) ++ "]]></cell>
			<cell><![CDATA[" ++ binary_to_list(H#torrent_status.name) ++ "]]></cell>
			<cell><![CDATA[" ++ integer_to_list(H#torrent_status.size) ++ "]]></cell>
			<cell><![CDATA[" ++                 
                "<div id='progressbar' \">
                    <div id='progressMade' style=\"width:" ++ io_lib:format("~.2f",[(H#torrent_status.downloaded/H#torrent_status.size)* 100]) ++ "%;\"\>
                </div>
                <div id='progressText' >" ++ io_lib:format("~.2f",[(H#torrent_status.downloaded/H#torrent_status.size)*100]) ++ "%</div>
            </div>]]></cell>
			<cell><![CDATA[" ++ atom_to_list(H#torrent_status.status) ++ "]]></cell>
			<cell><![CDATA[" ++ integer_to_list(H#torrent_status.connected_peers) ++ " (" ++ integer_to_list(H#torrent_status.peers) ++ ")]]></cell>
			<cell><![CDATA[" ++ float_to_list(H#torrent_status.downspeed) ++ "]]></cell>
            <cell><![CDATA[" ++ float_to_list(H#torrent_status.upspeed) ++ "]]></cell>
			<cell><![CDATA[" ++ integer_to_list(H#torrent_status.eta) ++ "]]></cell>
			<cell><![CDATA[" ++ integer_to_list(H#torrent_status.uploaded) ++ "]]></cell>
		</row>",
	get_body(T, NewRow ++ Body).

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
