%%% @author Peter Myllykoski <peter@UL30JT>, Nahid Vafaie
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created :  9 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(torrent).
-export([start_link_loader/1,init_loader/1]).
-export([start_link/3,init/1]).
-include("torrent_db_records.hrl").
-include("torrent_status.hrl").

-define(PeerLimit, 50).

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
	RecordList = torrent_db:get_all_torrents(),
	start_torrent(Pid,RecordList,Id).

start_torrent(Pid,[Record|Tail],Id) ->
	InfoHash = info_hash:to_hex(Record#torrent.info_sha),
	StatusRecord = #torrent_status{info_hash = Record#torrent.info_sha,
								   name = Record#torrent.info#info.name,
								   size = Record#torrent.info#info.length,
								   status = Record#torrent.status,
								   download_timer = erlang:now(),
								   upload_timer = erlang:now()},  
	StartFunc = {torrent,start_link,[Id,Record, StatusRecord]},
	ChildSpec = {InfoHash,StartFunc,transient,brutal_kill,worker,[torrent]},
	supervisor:start_child(Pid,ChildSpec),
	
	start_torrent(Pid,Tail,Id);

start_torrent(_Pid,[],_) ->
	ok.



%% =============================================================================
%% Regular torrent functions

start_link(Id,Record, StatusRecord) ->
	{ok,spawn_link(torrent,init,[{Id,Record,StatusRecord}])}.

%%TODO Status record should also come from here
init({Id,Record,StatusRecord}) ->
	process_flag(trap_exit,true),
	torrent_mapper:reg(Record#torrent.info_sha),
	init_start(Id, Record, StatusRecord).

init_start(Id, Record, StatusRecord) ->
	DownloadPid = spawn(download,init,[Record,self()]),
	AnnounceList = lists:flatten(Record#torrent.announce_list) -- [Record#torrent.announce],
	Announce = AnnounceList ++ [Record#torrent.announce],
	spawn_trackers(Announce,Record#torrent.info_sha,Id),
	loop(Record,StatusRecord,[],[],DownloadPid,Id,[],[], {0,0}).

loop(Record,StatusRecord, Id) ->
	receive
		{command, resume} when Record#torrent.info#info.length - Record#torrent.info#info.length_complete == 0->
			NewRecord = Record#torrent{status = seeding},
			NewStatusRecord = StatusRecord#torrent_status{status = seeding},
			init_start(Id, NewRecord,NewStatusRecord);
		{command, resume} ->
			NewRecord = Record#torrent{status = downloading},
			NewStatusRecord = StatusRecord#torrent{status = downloading},
			init_start(Id, NewRecord,NewStatusRecord);
		{get_status_record,Sender} ->
			Sender ! {status,StatusRecord},
			loop(Record,StatusRecord, Id);
		_ ->
			loop(Record,StatusRecord, Id)
	end.
	
%%TODO status
loop(Record,StatusRecord,TrackerList,LowPeerList,DownloadPid,Id,ActiveNetList,UnusedPeers, TrackerStats) ->
	receive
		{command, resume} ->
			get_peers(TrackerList),
			loop(Record,StatusRecord,TrackerList,LowPeerList,DownloadPid,Id,ActiveNetList,UnusedPeers, TrackerStats);
		{command, stop} ->
			stop(DownloadPid, TrackerList, ActiveNetList),
			NewRecord = Record#torrent{status = stopped},
			NewStatusRecord = StatusRecord#torrent_status{status = stopped},
			loop(NewRecord,NewStatusRecord,Id);
		{command, delete} ->
			stop(DownloadPid, TrackerList, ActiveNetList),
			torrent_db:delete_by_SHA1(Record#torrent.info_sha),
			torrent_mapper:free(Record#torrent.info_sha);
		{get_status_record,Sender} ->
			Sender ! {status,StatusRecord},
			loop(Record,StatusRecord,TrackerList,LowPeerList,DownloadPid,Id,ActiveNetList,UnusedPeers, TrackerStats);
		{new_upload,TcpPid, IpPort} ->
			NetPid = spawn_link(nettransfer,init_upload,[self(),TcpPid,Record#torrent.info#info.bitfield]),
			NewActiveNetList = [{NetPid,IpPort}|ActiveNetList],
			loop(Record,StatusRecord,TrackerList,LowPeerList,DownloadPid,Id,NewActiveNetList,UnusedPeers, TrackerStats);
		{get_statistics,Pid} ->
			{TrackerDownloaded, TrackerUploaded} = TrackerStats,
			Pid ! {statistics,TrackerUploaded
				   , TrackerDownloaded
				   , Record#torrent.info#info.length - Record#torrent.info#info.length_complete},
			loop(Record,StatusRecord,TrackerList,LowPeerList,DownloadPid,Id,ActiveNetList,UnusedPeers, TrackerStats);
		{im_free, NetPid} ->
			DownloadPid ! {new_free, NetPid},
			loop(Record,StatusRecord,TrackerList,LowPeerList,DownloadPid,Id,ActiveNetList,UnusedPeers, TrackerStats);
		{peer_list,FromPid,ReceivedPeerList} ->
			io:fwrite("Peers: ~p~n", [ReceivedPeerList]),
			TempTrackerList = lists:delete(FromPid, TrackerList),
			NewTrackerList = [FromPid|TempTrackerList],
			TempUnusedPeers = screen_peers(ReceivedPeerList -- LowPeerList -- UnusedPeers,ActiveNetList,[]),
			NewUnusedPeers = TempUnusedPeers ++ UnusedPeers,
			{NewActiveNetList, FinalUnusedPeers} = spawn_connections_init(Record, StatusRecord, ActiveNetList ,NewUnusedPeers ++ LowPeerList, NewUnusedPeers, Id),
			Peers = length(FinalUnusedPeers ++ NewActiveNetList),
			NewStatusRecord = StatusRecord#torrent_status{peers=Peers,connected_peers=length(NewActiveNetList)},
			loop(Record,NewStatusRecord,NewTrackerList,LowPeerList,DownloadPid,Id,NewActiveNetList,FinalUnusedPeers, TrackerStats);
		{bitfield,FromPid,ReceivedBitfield} ->
			NumPieces = byte_size(Record#torrent.info#info.pieces) div 20,
			case (bit_size(ReceivedBitfield) > NumPieces) of
				true ->
					<<Bitfield:NumPieces/bitstring,Rest/bitstring>> = ReceivedBitfield,
					RestSize = bit_size(Rest),
					if
						Rest == <<0:RestSize>> ->
							PeerIndexList = bitfield:to_indexlist(Bitfield,invert),
							DownloadPid ! {net_index_list, FromPid, PeerIndexList};
						true ->
							FromPid ! bad_bitfield
					end;
				false ->
					FromPid ! bad_bitfield
			end,
			loop(Record,StatusRecord,TrackerList,LowPeerList,DownloadPid,Id,ActiveNetList,UnusedPeers, TrackerStats);
		{have,FromPid,Index} ->
			DownloadPid ! {net_index_list, FromPid, [{Index}]},
			loop(Record,StatusRecord,TrackerList,LowPeerList,DownloadPid,Id,ActiveNetList,UnusedPeers, TrackerStats);
		{dowloaded,SenderPid,PieceIndex,Data} ->
			Now = erlang:now(),
			Elapsed = timer:now_diff(Now, StatusRecord#torrent_status.download_timer),
			Speed = byte_size(Data)/(Elapsed/1000),
			TotalDownload = StatusRecord#torrent_status.downloaded + size(Data),
			NewStatusRecord = StatusRecord#torrent_status{downloaded=TotalDownload,
														  downspeed = Speed, download_timer = Now},
			{TrackerDownloaded, TrackerUploaded} = TrackerStats,
			NewTrackerDownloaded = TrackerDownloaded + size(Data),
			NewTrackerStats = {NewTrackerDownloaded, TrackerUploaded},
			%%TODO eta
			Done = bitfield:has_one_zero(Record#torrent.info#info.bitfield),
			case write_to_file:write(PieceIndex,Data,Record,Done) of
				{ok, OldTempRecord} ->
					case Done of
						true when NewStatusRecord#torrent_status.status == downloading ->
							FinalStatusRecord = NewStatusRecord#torrent_status{status=seeding},
							TempRecord = OldTempRecord#torrent{status=seeding},
							send_completed(TrackerList);
						_Other ->
							TempRecord = OldTempRecord,
							FinalStatusRecord = NewStatusRecord
					end,
					SenderPid ! {ok, done},
					DownloadPid ! {piece_done, PieceIndex},
					send_have(PieceIndex,ActiveNetList),
					NewBitField = bitfield:flip_bit(PieceIndex, TempRecord#torrent.info#info.bitfield),
					NewLength = TempRecord#torrent.info#info.length_complete + byte_size(Data),
					%% 					Percentage = NewLength / TempRecord#torrent.info#info.length * 100,
					%% 					io:fwrite("....~n~.2f~n....~n", [Percentage]),
					NewRecord = TempRecord#torrent{info = (TempRecord#torrent.info)#info{bitfield = NewBitField, length_complete = NewLength}},
					io:fwrite("left: ~.3f MegaByte~n", [(TempRecord#torrent.info#info.length - NewLength)/(1024*1024)]),
					torrent_db:delete_by_SHA1(NewRecord#torrent.info_sha),
					torrent_db:add(NewRecord),
					loop(NewRecord,FinalStatusRecord,TrackerList,LowPeerList,DownloadPid,Id,ActiveNetList,UnusedPeers, NewTrackerStats);
				{error, _Reason} ->
					SenderPid ! {error, corrupt_data},
					loop(Record,NewStatusRecord,TrackerList,LowPeerList,DownloadPid,Id,ActiveNetList,UnusedPeers, NewTrackerStats)
			end;
		{upload,SenderPid,PieceIndex,Offset,Length} ->
			%%TODO upspeed uploaded
			io:fwrite("im uploading!!! ~n"),
			{ok, File_Binary} = file_split:request_data(PieceIndex,Offset,Length, Record),
			SenderPid ! {piece,PieceIndex,Offset,File_Binary},
			NewStatusRecord = StatusRecord#torrent_status{uploaded = StatusRecord#torrent_status.uploaded + Length},
			{TrackerDownloaded, TrackerUploaded} = TrackerStats,
			NewTrackerUploaded = TrackerUploaded + Length,
			NewTrackerStats = {TrackerDownloaded, NewTrackerUploaded},
			loop(Record,NewStatusRecord,TrackerList,LowPeerList,DownloadPid,Id,ActiveNetList,UnusedPeers, NewTrackerStats);
		
		{'EXIT',FromPid,Reason} ->
			case Reason of
				stopped ->
					loop(Record,StatusRecord,TrackerList,LowPeerList,DownloadPid,Id,ActiveNetList,UnusedPeers, TrackerStats);
				_ ->
					NewStatusRecord = StatusRecord#torrent_status{connected_peers = StatusRecord#torrent_status.connected_peers - 1},
					{TempActiveNetList ,NewLowPeerList} = ban_net_pid(FromPid, ActiveNetList, LowPeerList, DownloadPid, Reason),
					{FinalActiveNetList, NewUnusedPeers} = spawn_connections_init(Record, StatusRecord, TempActiveNetList, UnusedPeers ++ LowPeerList, UnusedPeers, Id),
					loop(Record,NewStatusRecord,TrackerList,NewLowPeerList,DownloadPid,Id,FinalActiveNetList,NewUnusedPeers, TrackerStats)
			end
	end.

stop(DownloadPid, TrackerList, ActiveNetList) ->
	DownloadPid ! {die},
	send_stopped_tcps(ActiveNetList),
	send_stopped_trackers(TrackerList).

ban_net_pid(FromPid, ActiveNetList, LowPeerList, DownloadPid, Reason) ->
	BadNet = lists:keyfind(FromPid,1,ActiveNetList),
	NewActiveNetList = lists:delete(BadNet, ActiveNetList),
	case Reason of
		_ when Reason == handshake;
			   Reason == port_closed;
			   Reason == bad_bitfield;
			   Reason == econnrefused;
			   Reason == bad_request ->
			NewLowPeerList = lists:delete(element(2,BadNet), LowPeerList);
		_ ->
			NewLowPeerList = lists:delete(element(2,BadNet), LowPeerList)
	end,
	DownloadPid ! {net_exited, FromPid},
	{NewActiveNetList ,NewLowPeerList}.

spawn_trackers([],_,_) ->
	ok;
spawn_trackers([Announce|AnnounceList],InfoHash,Id) ->
	Self = self(),
	spawn(tracker,init,[Self,Announce,InfoHash,Id]),
	spawn_trackers(AnnounceList,InfoHash,Id).

screen_peers([] ,_ActiveNetList, List) ->
	List;
screen_peers([IpPort | PeerList] ,ActiveNetList, List) ->
	case lists:keymember(IpPort, 2, ActiveNetList) of
		true ->
			screen_peers(PeerList, ActiveNetList, List);
		false ->
			screen_peers(PeerList, ActiveNetList, [IpPort|List])
	end.

spawn_connections_init(Record, StatusRecord, ActiveNetList, PeerList, UnusedPeers, Id) ->
	case StatusRecord#torrent_status.status of
		downloading ->
			TempActiveNetList = spawn_connections(PeerList,Record#torrent.info_sha,Id, [],?PeerLimit - length(ActiveNetList),Record);
		_Other ->
			TempActiveNetList = []
	end,
	case length(TempActiveNetList) >= length(UnusedPeers) of
		true ->
			FinalUnusedPeers = [];
		false ->
			FinalUnusedPeers = lists:nthtail(length(TempActiveNetList), UnusedPeers)
	end,
	NewActiveNetList = TempActiveNetList ++ ActiveNetList,
	{NewActiveNetList, FinalUnusedPeers}.

spawn_connections(_,_InfoHash,_Id,NetList,Count,_Record) when Count < 1->
	NetList;
spawn_connections([],_InfoHash,_Id,NetList,_,_Record) ->
	NetList;
spawn_connections([{Ip,Port}|Rest],InfoHash,Id,NetList,Count,Record) ->
	Self = self(),
	Pid = spawn_link(nettransfer,init,[Self,Ip,Port,InfoHash,Id,Record#torrent.info#info.bitfield]),
	spawn_connections(Rest,InfoHash,Id, [{Pid, {Ip,Port}}|NetList],Count - 1, Record).

send_have(_,[]) ->
	ok;
send_have(PieceIndex, [{Pid,_Ip}|Tail]) ->
	Pid ! {have, self(), PieceIndex},
	send_have(PieceIndex,Tail).

send_completed([]) ->
	ok;
send_completed([Pid|Tail]) ->
	Pid ! {completed},
	send_completed(Tail).

get_peers([]) ->
	ok;
get_peers([Pid|Tail]) ->
	Pid ! {get_peers},
	get_peers(Tail).

send_stopped_tcps([]) ->
	ok;
send_stopped_tcps([{Pid,_Ip}|Tail]) ->
	Pid ! {stop, stopped},
	send_stopped_tcps(Tail).


send_stopped_trackers([]) ->
	ok;
send_stopped_trackers([Pid|Tail]) ->
	Pid ! {stopped},
	send_stopped_trackers(Tail).