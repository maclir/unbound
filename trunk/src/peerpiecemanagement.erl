%%% @author Peter Myllykoski , Nahid Vafaie
%%% Created :  16 Nov 2011


-module(peerpiecemanagement).

-export([compare_bitfields/3,connect_to_peer/3,create_dummy_bitfield/1,getPeerList/2]).
-export([bitfield_to_indexlist/1, bitfield_to_peerindexlist/1]).

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

create_dummy_bitfield(Num) ->
    Int = Num div 8,
    case Num rem 8 of
	0 ->
	    ByteLenght = Int*8;
	_ ->
	    ByteLenght = (Int + 1)*8
    end,
    Lenght = (ByteLenght div 8) + 1,


    <<Lenght:32,5:8,0:ByteLenght>>.




