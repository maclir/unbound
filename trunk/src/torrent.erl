%%% @author Peter Myllykoski <peter@UL30JT>, Nahid Vafaie
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%% 
%%% @end
%%% Created :  9 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(torrent).
-export([start_link_loader/1,init_loader/1]).
-export([start_link/3,init/1]).
-export([compare_bitfields/4]).
-include("torrent_db_records.hrl").
-include("torrent_status.hrl").

%% =============================================================================
%% Torrent loader function that is responsible for opening the persistent
%% storage and dynamically add all the torrents found into the supervisor.

start_link_loader(Id) ->
    Self = self(),
    spawn_link(?MODULE,init_loader,[{Self,Id}]),
    receive
	{ok,Pid} ->
	    {ok,Pid}
    after 100 ->
	    {error,time_out}
    end.

init_loader({Pid,Id})->
    io:fwrite("Torrent Loader Started!\n"),
    Pid ! {ok,self()},
    RecordList = torrent_db:size_gt(0),
    start_torrent(Pid,RecordList,Id).

start_torrent(Pid,[Record|Tail],Id) ->
    InfoHash = info_hash:to_hex(Record#torrent.info_sha),
    StartFunc = {torrent,start_link,[InfoHash,Id,Record]},
    ChildSpec = {InfoHash,StartFunc,transient,brutal_kill,worker,[torrent]},
    supervisor:start_child(Pid,ChildSpec),
    start_torrent(Pid,Tail,Id);

start_torrent(_Pid,[],_) ->
    ok.



%% =============================================================================
%% Regular torrent functions

start_link(Var,Id,Record) ->
    {ok,spawn_link(torrent,init,[{Var,Id,Record}])}.

init({Var,Id,Record}) ->
    %% Check integrity of downloaded pieces, create a bitfield according to
    %% the result of the integrity check.
    
    NumPieces = byte_size((Record#torrent.info)#info.pieces) div 20,
    NumBlocks = (Record#torrent.info)#info.piece_length div 16384,
    OurBitfield = create_dummy_bitfield(NumPieces),
    
    Name = Record#torrent.info#info.name,
    PiecesSha = Record#torrent.info#info.pieces,
    Piece_length = Record#torrent.info#info.piece_length,

    case Record#torrent.announce_list of
	%% If the tracker list is empty, only use the main tracker
	[] ->
	    %% Start communication with tracker and peers
	    case getPeerList(Record,Id) of
		{ok,Interval,PeerList} ->
		    connect_to_peer(PeerList,Record#torrent.info_sha, Id, Name, PiecesSha, Piece_length),
		    
		    io:fwrite("~p started by client ~p\n",[Var,Id]),
		    loop(#torrent_status{db_bitfield=OurBitfield});
		{error,Reason} ->
		    io:fwrite("~p",[Reason])
	    end;

	%% Should be changed so that all the trackers are queried, or should
	%% the other trackers be fallback trackers if there is no connection to
	%% the main one?
	AnnounceList ->
	    io:fwrite("Torrent has a announce list"),
	    case getPeerList(Record,Id) of
		{ok,Interval,PeerList} ->
		    connect_to_peer(PeerList,Record#torrent.info_sha,Id, Name, PiecesSha, Piece_length),
		    io:fwrite("~p started by client ~p\n",[Var,Id]),
		    loop(#torrent_status{db_bitfield=OurBitfield});
		{error,Reason} ->
		    io:fwrite("~p",[Reason])
	    end
    end.

loop(StatusRecord) ->
    receive
	{bitfield,Pid,Bitfield} ->
	    TempBitfield = compare_bitfields(OurBitfield,Bitfield,NumPieces,Pid),
	    loop(StatusRecord#torrent_status{temp_bitfield=TempBitfield});
	{downloaded,PieceId,Data} ->
	    %% Validate & write to disk
	    %% Change bitfield in database
	    ok;
	    
	Msg ->
	    io:fwrite("~p\n",[Msg]),
	    loop()
    end.


getPeerList(Record,Id) ->
    InfoHash = Record#torrent.info_sha,
    InfoHashUrl = info_hash:url_encode(Record#torrent.info_sha),
    Announce = Record#torrent.announce,
    case Announce of
	<<"http",_Rest/binary>> ->
	    tcp:connect_to_server(Announce,InfoHashUrl,Id);
	<<"udp",_Rest/binary>> ->
	    {error,udp_not_supported}
    end.


connect_to_peer([{Ip,Port}|Rest],InfoHash,Id, Name, PiecesSha, Piece_length) ->
    [tcp:open_a_socket(Ip,Port,InfoHash,Id, Name, PiecesSha)|connect_to_peer(Rest,InfoHash,Id, Name, PiecesSha, Piece_length)];

connect_to_peer([],InfoHash,Id, _, _, _) ->
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

compare_bitfields(<<Lenght:32,5,OurBitfield/binary>>,<<Lenght:32,5,Bitfield/binary>>,NumPieces,Pid) ->
    compare_bits(0,OurBitfield,Bitfield,NumPieces,Pid).

compare_bits(Index,<<OurFirstBit:1,OurRest/bitstring>>,<<FirstBit:1,Rest/bitstring>>,NumPieces,Pid) ->
    if
	OurFirstBit == 0,FirstBit == 1,Index<NumPieces ->
	    Pid ! {get_piece,Index};
	true ->
	    compare_bits(Index+1,OurRest,Rest,NumPieces,Pid) 
    end;

compare_bits(_Index,<<>>,<<>>,_NumPieces,_Pid) ->
    ok.

	
