%%% @author Peter Myllykoski , Nahid Vafaie
%%% Created :  16 Nov 2011


-module(peerpiecemanagement).

-export([connect_to_peer/3,getPeerList/2]).


-include("torrent_db_records.hrl").



getPeerList(Record,Id) ->
    InfoHash = Record#torrent.info_sha,
    InfoHashUrl = info_hash:url_encode(Record#torrent.info_sha),
    Announce = Record#torrent.announce,
    case Announce of
	<<"http",_Rest/binary>> ->
	    tcp:connect_to_server(Announce,InfoHashUrl,Id,"started");
	<<"udp",_Rest/binary>> ->
	    {error,udp_not_supported}
    end.


connect_to_peer([{Ip,Port}|Rest],InfoHash,Id) ->
    [spawn(nettransfer,init,[self(),Ip,Port,InfoHash,Id])|connect_to_peer(Rest,InfoHash,Id)];

connect_to_peer([],InfoHash,Id) ->
    [].






