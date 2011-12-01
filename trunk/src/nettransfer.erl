%%%author: Nahid Vafaie ,Peter Myllykoski
%%% created: 18 Nov 2011

-module(nettransfer).

-export([init/5]).


init(TorrentPid,DestinationIp,DestinationPort,InfoHash,ClientId)->
	TcpPid=tcp:open_a_socket(DestinationIp, DestinationPort,InfoHash,ClientId),
	Choked = true,
	Interested = false,
	Status= {Choked,Interested},
	loop(Status,TcpPid,TorrentPid,0,[],idle).


loop(Status,TcpPid,TorrentPid,StoredBitfield,Que,free) ->
	TorrentPid ! {im_free, self()},
	receive
		{download_block,FromPid,Index,Offset,Length} ->
			TcpPid ! {request, Index,Offset,Length},
			loop(Status,TcpPid,TorrentPid,StoredBitfield,Que,FromPid);
		{continue} ->
			loop(Status,TcpPid,TorrentPid,StoredBitfield,Que,idle)
		after 2000 ->
			loop(Status,TcpPid,TorrentPid,StoredBitfield,Que,free)
	end;
loop(Status,TcpPid,TorrentPid,StoredBitfield,Que,PiecePid) ->
	receive
		check_free ->
			case PiecePid of
				idle when (not element(1,Status) and element(2,Status)) ->
					loop(Status,TcpPid,TorrentPid,StoredBitfield,Que,free);
				_ ->
					loop(Status,TcpPid,TorrentPid,StoredBitfield,Que,PiecePid)
			end;
		is_interested ->
			{OldChoked, OldInterested} = Status,
			if not OldInterested ->
				TcpPid ! interested;
			   true ->
				   ok
			end,
			NewStatus = {OldChoked,true},
			loop(NewStatus,TcpPid,TorrentPid,StoredBitfield,Que,PiecePid);
		not_interested ->
			case Status of
				{true,_} ->
					TcpPid ! not_interested ,
					NewStatus = {true,false} ;
				{false, _} ->
					TcpPid ! not_interested,
					NewStatus = {false,false}
			end,
			loop(NewStatus,TcpPid,TorrentPid,StoredBitfield,Que,PiecePid);
		
		{got_unchoked, _FromPid} ->
			case Status of
				{_,true}->
					NewStatus= {false,true};
				{_,false} ->
					NewStatus = {false,false}
			end,
			loop(NewStatus,TcpPid,TorrentPid,StoredBitfield,Que,PiecePid);
		
		{got_choked, _FromPid} ->
			case Status of
				{_,true} ->
					NewStatus ={true,true};
				{_,false} ->
					NewStatus = {true,false}
			end,
			loop(NewStatus,TcpPid,TorrentPid,StoredBitfield,Que,PiecePid);
		{have,SenderPid,Piece_Index} ->
			case SenderPid of
				TcpPid ->
					TorrentPid ! {have,self(),Piece_Index};
				TorrentPid ->
					TcpPid ! { have,self(), Piece_Index}
			end,
			loop(Status,TcpPid,TorrentPid,StoredBitfield,Que,PiecePid);
		
		{client_bitfield,SenderPid, Bitfield} ->
			case SenderPid of
				TcpPid ->
					TorrentPid ! {bitfield,self(),Bitfield};
				TorrentPid ->
					TcpPid ! {bitfield,Bitfield}
			end,
			loop(Status,TcpPid,TorrentPid,Bitfield,Que,PiecePid);
		
		{got_block,Offset,Length,Data} ->
			PiecePid ! {block,self(),Offset,Length,Data},
			case Que of
				[] ->
					loop(Status,TcpPid,TorrentPid,StoredBitfield,Que,free);
				[{FromPid,Index,Offset,Length}|T] ->
					TcpPid ! {request, Index,Offset,Length},	
					loop(Status,TcpPid,TorrentPid,StoredBitfield,T,FromPid)
			end;
		{download_block,FromPid,Index,Offset,Length} ->
			loop(Status,TcpPid,TorrentPid,StoredBitfield,Que ++ [{FromPid,Index,Offset,Length}],PiecePid)
		after 120000 ->
			%% shouldnt there be a loop after this line??
			TcpPid ! keep_alive
	end.