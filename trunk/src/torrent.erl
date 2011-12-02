%%% @author Peter Myllykoski <peter@UL30JT>, Nahid Vafaie
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created :  9 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(torrent).
-export([start_link_loader/1,init_loader/1]).
-export([start_link/2,init/1]).
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
	RecordList = [torrent_db:get_torrent_by_id(0)],
	start_torrent(Pid,RecordList,Id).

start_torrent(Pid,[Record|Tail],Id) ->
	InfoHash = info_hash:to_hex(Record#torrent.info_sha),
	StartFunc = {torrent,start_link,[Id,Record]},
	ChildSpec = {InfoHash,StartFunc,transient,brutal_kill,worker,[torrent]},
	supervisor:start_child(Pid,ChildSpec),
	start_torrent(Pid,Tail,Id);

start_torrent(_Pid,[],_) ->
	ok.



%% =============================================================================
%% Regular torrent functions

start_link(Id,Record) ->
	{ok,spawn_link(torrent,init,[{Id,Record}])}.

%%TODO Status record should also come from here
init({Id,Record}) ->
	process_flag(trap_exit,true),
	DownloadPid = spawn(download,init,[Record,self()]),
	AnnounceList = lists:flatten(Record#torrent.announce_list) -- [Record#torrent.announce],
	Announce = AnnounceList ++ [Record#torrent.announce],
	spawn_trackers(Announce,Record#torrent.info_sha,Id),
	loop(Record,[],[],[],DownloadPid,Id,[],[]).
%%TODO status
loop(Record,StatusRecord,TrackerList,LowPeerList,DownloadPid,Id,ActiveNetList,UnusedPeers) ->
	receive
		{new_upload,TcpPid, IpPort} ->
%%TODO upspeed uploaded
			NetPid = spawn_link(nettransfer,init_upload,[self(),TcpPid,Record#torrent.info#info.bitfield]),
			NewActiveNetList = [{NetPid,IpPort}|ActiveNetList],
			loop(Record,StatusRecord,TrackerList,LowPeerList,DownloadPid,Id,NewActiveNetList,UnusedPeers);
		{get_statistics,Pid} ->
			Pid ! {statistics,0,0,0},
			loop(Record,StatusRecord,TrackerList,LowPeerList,DownloadPid,Id,ActiveNetList,UnusedPeers);
		{im_free, NetPid} ->
			DownloadPid ! {new_free, NetPid},
			loop(Record,StatusRecord,TrackerList,LowPeerList,DownloadPid,Id,ActiveNetList,UnusedPeers);
		{peer_list,FromPid,ReceivedPeerList} ->
%%TODO peers connected_peers
			io:fwrite("Got Peer List\n"),
			%% why do we need the tracker list?
			TempTrackerList = lists:delete(FromPid, TrackerList),
			NewTrackerList = [FromPid|TempTrackerList],
			TempUnusedPeers = screen_peers(ReceivedPeerList -- LowPeerList -- UnusedPeers,ActiveNetList,[]),
			NewUnusedPeers = TempUnusedPeers ++ UnusedPeers,
			TempActiveNetList = spawn_connections(NewUnusedPeers ++ LowPeerList,Record#torrent.info_sha,Id, [],10 - length(ActiveNetList),Record),
			case length(TempActiveNetList) >= length(NewUnusedPeers) of
				true ->
					FinalUnusedPeers = [];
				false ->
					FinalUnusedPeers = lists:nthtail(length(TempActiveNetList), NewUnusedPeers)
			end,
			NewActiveNetList = TempActiveNetList ++ ActiveNetList,
			loop(Record,StatusRecord,NewTrackerList,LowPeerList,DownloadPid,Id,NewActiveNetList,FinalUnusedPeers);
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
			loop(Record,StatusRecord,TrackerList,LowPeerList,DownloadPid,Id,ActiveNetList,UnusedPeers);
		{have,FromPid,Index} ->
			DownloadPid ! {net_index_list, FromPid, [{Index}]},
			loop(Record,StatusRecord,TrackerList,LowPeerList,DownloadPid,Id,ActiveNetList,UnusedPeers);
		{dowloaded,SenderPid,PieceIndex,Data} ->
%%TODO downspeed downloaded eta 
			Done = bitfield:has_one_zero(Record#torrent.info#info.bitfield),
			case write_to_file:write(PieceIndex,Data,Record,Done) of
				{ok, TempRecord} ->
					SenderPid ! {ok, done},
					DownloadPid ! {piece_done, PieceIndex},
					NewBitField = bitfield:flip_bit(PieceIndex, TempRecord#torrent.info#info.bitfield),
					NewLength = TempRecord#torrent.info#info.length_complete + byte_size(Data),
					%% 					Percentage = NewLength / TempRecord#torrent.info#info.length * 100,
					%% 					io:fwrite("....~n~.2f~n....~n", [Percentage]),
					NewRecord = TempRecord#torrent{info = (TempRecord#torrent.info)#info{bitfield = NewBitField, length_complete = NewLength}},
					torrent_db:delete_by_SHA1(NewRecord#torrent.info_sha),
					torrent_db:add(NewRecord),
					io:fwrite("done:~p~n", [PieceIndex]),
					loop(NewRecord,StatusRecord,TrackerList,LowPeerList,DownloadPid,Id,ActiveNetList,UnusedPeers);
				{error, _Reason} ->
					SenderPid ! {error, corrupt_data},
					loop(Record,StatusRecord,TrackerList,LowPeerList,DownloadPid,Id,ActiveNetList,UnusedPeers)
			end;
		{upload,SenderPid,PieceIndex,Offset,Length} ->
			File_Binary = file_split:request_data(PieceIndex,Offset,Length, Record),
			SenderPid ! {piece,PieceIndex,Offset,Length,File_Binary},
			loop(Record,StatusRecord,TrackerList,LowPeerList,DownloadPid,Id,ActiveNetList,UnusedPeers);
		
		{'EXIT',FromPid,_Reason} ->
%%TODO peers connected_peers
			%% 			io:fwrite("~p Got EXIT: ~p\n", [FromPid, _Reason]),
			{TempActiveNetList ,NewLowPeerList} = ban_net_pid(FromPid, ActiveNetList, LowPeerList, DownloadPid),
			NewActiveNetList = spawn_connections(UnusedPeers ++ NewLowPeerList,Record#torrent.info_sha,Id, [],10 - length(ActiveNetList),Record),
			case length(NewActiveNetList) >= length(UnusedPeers) of
				true ->
					NewUnusedPeers = [];
				false ->
					NewUnusedPeers = lists:nthtail(length(NewActiveNetList), UnusedPeers)
			end,
			FinalActiveNetList = NewActiveNetList ++ TempActiveNetList,
			loop(Record,StatusRecord,TrackerList,NewLowPeerList,DownloadPid,Id,FinalActiveNetList,NewUnusedPeers)
	end.

ban_net_pid(FromPid, ActiveNetList, LowPeerList, DownloadPid) ->
	BadNet = lists:keyfind(FromPid,1,ActiveNetList),
	NewActiveNetList = lists:delete(BadNet, ActiveNetList),
	NewLowPeerList = [element(2,BadNet)|LowPeerList],
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

spawn_connections(_,_InfoHash,_Id,NetList,Count,_Record) when Count < 1->
	NetList;
spawn_connections([],_InfoHash,_Id,NetList,_,_Record) ->
	NetList;
spawn_connections([{Ip,Port}|Rest],InfoHash,Id,NetList,Count,Record) ->
	Pid = spawn_link(nettransfer,init,[self(),Ip,Port,InfoHash,Id,Record#torrent.info#info.bitfield]),
	spawn_connections(Rest,InfoHash,Id, [{Pid, {Ip,Port}}|NetList],Count - 1, Record).
