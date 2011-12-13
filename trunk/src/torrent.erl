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

-define(PeerLimit, 40).

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
	StatusRecord = #torrent_status{info_hash = Record#torrent.info_sha,
								   %				   priority = Record#torrent.priority,
								   name = Record#torrent.info#info.name,
								   size = Record#torrent.info#info.length,
								   status = Record#torrent.status,
								   download_timer = erlang:now(),
								   upload_timer = erlang:now()},   
	spawn_trackers(Announce,Record#torrent.info_sha,Id),
	loop(Record,StatusRecord,[],[],DownloadPid,Id,[],[]).
%%TODO status
loop(Record,StatusRecord,TrackerList,LowPeerList,DownloadPid,Id,ActiveNetList,UnusedPeers) ->
	receive
		{new_upload,TcpPid, IpPort} ->
			NetPid = spawn_link(nettransfer,init_upload,[self(),TcpPid,Record#torrent.info#info.bitfield]),
			NewActiveNetList = [{NetPid,IpPort}|ActiveNetList],
			loop(Record,StatusRecord,TrackerList,LowPeerList,DownloadPid,Id,NewActiveNetList,UnusedPeers);
		{get_statistics,Pid} ->
			Pid ! {statistics,StatusRecord#torrent_status.uploaded
				   , StatusRecord#torrent_status.downloaded
				   , Record#torrent.info#info.length - Record#torrent.info#info.length_complete},
			loop(Record,StatusRecord,TrackerList,LowPeerList,DownloadPid,Id,ActiveNetList,UnusedPeers);
		{im_free, NetPid} ->
			DownloadPid ! {new_free, NetPid},
			loop(Record,StatusRecord,TrackerList,LowPeerList,DownloadPid,Id,ActiveNetList,UnusedPeers);
		{peer_list,FromPid,ReceivedPeerList} ->
			
			io:fwrite("Got Peer List: ~p\n", [length(ReceivedPeerList)]),
			TempTrackerList = lists:delete(FromPid, TrackerList),
			NewTrackerList = [FromPid|TempTrackerList],
			TempUnusedPeers = screen_peers(ReceivedPeerList -- LowPeerList -- UnusedPeers,ActiveNetList,[]),
			NewUnusedPeers = TempUnusedPeers ++ UnusedPeers,
			case StatusRecord#torrent_status.status of
				downloading ->
					TempActiveNetList = spawn_connections(NewUnusedPeers ++ LowPeerList,Record#torrent.info_sha,Id, [],?PeerLimit - length(ActiveNetList),Record);
				_Other ->
					TempActiveNetList = []
			end,
			case length(TempActiveNetList) >= length(NewUnusedPeers) of
				true ->
					FinalUnusedPeers = [];
				false ->
					FinalUnusedPeers = lists:nthtail(length(TempActiveNetList), NewUnusedPeers)
			end,
			NewActiveNetList = TempActiveNetList ++ ActiveNetList,
			Peers = length(FinalUnusedPeers ++ NewActiveNetList),
			NewStatusRecord = StatusRecord#torrent_status{peers=Peers,connected_peers=NewActiveNetList},
			loop(Record,NewStatusRecord,NewTrackerList,LowPeerList,DownloadPid,Id,NewActiveNetList,FinalUnusedPeers);
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
			Now = erlang:now(),
			Elapsed = timer:now_diff(Now, StatusRecord#torrent_status.download_timer),
			Speed = byte_size(Data)/(Elapsed/1000),
			TotalDownload = StatusRecord#torrent_status.downloaded + size(Data),
			NewStatusRecord = StatusRecord#torrent_status{downloaded=TotalDownload,
														  downspeed = Speed, download_timer = Now},
			%%TODO eta
			Done = bitfield:has_one_zero(Record#torrent.info#info.bitfield),
			case write_to_file:write(PieceIndex,Data,Record,Done) of
				{ok, TempRecord} ->
					case Done of
						true when NewStatusRecord#torrent_status.status == downloading ->
							FinalStatusRecord = NewStatusRecord#torrent_status{status=seeding},
							send_completed(TrackerList);
						_Other ->
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
					loop(NewRecord,FinalStatusRecord,TrackerList,LowPeerList,DownloadPid,Id,ActiveNetList,UnusedPeers);
				{error, _Reason} ->
					SenderPid ! {error, corrupt_data},
					loop(Record,NewStatusRecord,TrackerList,LowPeerList,DownloadPid,Id,ActiveNetList,UnusedPeers)
			end;
		{upload,SenderPid,PieceIndex,Offset,Length} ->
			%%TODO upspeed uploaded
			io:fwrite("im uploading!!! ~n"),
			File_Binary = file_split:request_data(PieceIndex,Offset,Length, Record),
			SenderPid ! {piece,PieceIndex,Offset,Length,File_Binary},
			NewStatusRecord = StatusRecord#torrent_status{uploaded = StatusRecord#torrent_status.uploaded + Length},
			loop(Record,NewStatusRecord,TrackerList,LowPeerList,DownloadPid,Id,ActiveNetList,UnusedPeers);
		
		{'EXIT',FromPid,Reason} ->
			%%TODO peers connected_peers
			io:fwrite("~p Got EXIT: ~p\n", [FromPid, Reason]),
			{TempActiveNetList ,NewLowPeerList} = ban_net_pid(FromPid, ActiveNetList, LowPeerList, DownloadPid, Reason),
			case StatusRecord#torrent_status.status of 
				downloading ->
					NewActiveNetList = spawn_connections(UnusedPeers ++ LowPeerList,Record#torrent.info_sha,Id, [],?PeerLimit - length(ActiveNetList),Record);
				_Other ->
					NewActiveNetList = []
			end,
			case length(NewActiveNetList) >= length(UnusedPeers) of
				true ->
					NewUnusedPeers = [];
				false ->
					NewUnusedPeers = lists:nthtail(length(NewActiveNetList), UnusedPeers)
			end,
			FinalActiveNetList = NewActiveNetList ++ TempActiveNetList,
			loop(Record,StatusRecord,TrackerList,NewLowPeerList,DownloadPid,Id,FinalActiveNetList,NewUnusedPeers)
	end.

ban_net_pid(FromPid, ActiveNetList, LowPeerList, DownloadPid, Reason) ->
	BadNet = lists:keyfind(FromPid,1,ActiveNetList),
	NewActiveNetList = lists:delete(BadNet, ActiveNetList),
	case Reason of
		_ when Reason == handshake;
			   Reason == port_closed;
			   Reason == bad_bitfield;
			   Reason == econnrefused ->
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
send_have(PieceIndex, [{Pid,_}|Tail]) ->
	Pid ! {have, self(), PieceIndex},
	send_have(PieceIndex,Tail).

send_completed([]) ->
	ok;
send_completed([Pid|Tail]) ->
	Pid ! {completed},
	send_completed(Tail).

