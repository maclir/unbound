%%%author: Nahid Vafaie ,Peter Myllykoski
%%% created: 18 Nov 2011

-module(nettransfer).

-export([init/5,loop/6]).


init(TorrentPid,DestinationIp,DestinationPort,InfoHash,ClientId)->
	TcpPid=tcp:open_a_socket(DestinationIp, DestinationPort,InfoHash,ClientId),
	Choked = true,
	Interested = false,
	Status= {Choked,Interested},
	loop(Status,TcpPid,{0,0,0},TorrentPid,0,idle).


loop(Status,TcpPid,NextBlock,TorrentPid,StoredBitfield,free) ->    
	TorrentPid ! {im_free, self()},
	receive
		{download_block,FromPid,Index,Offset,Length} ->
			FromPid ! {ok, downloading},
			TcpPid ! {request, Index,Offset,Length},
			loop(Status,TcpPid,{Index,Offset,Length},TorrentPid,StoredBitfield,FromPid);
		{continue} ->
			loop(Status,TcpPid,NextBlock,TorrentPid,StoredBitfield,idle)
		after 2000 ->
			loop(Status,TcpPid,NextBlock,TorrentPid,StoredBitfield,free)
	end;
loop(Status,TcpPid,NextBlock,TorrentPid,StoredBitfield,PiecePid) ->
	receive
		check_free ->
			case PiecePid of
				idle when (not element(1,Status) and element(2,Status)) ->
					loop(Status,TcpPid,NextBlock,TorrentPid,StoredBitfield,free);
				_ when element(1,Status) ->
					TorrentPid ! {choked,self()},
					loop(Status,TcpPid,NextBlock,TorrentPid,StoredBitfield,PiecePid);
				_ ->
					loop(Status,TcpPid,NextBlock,TorrentPid,StoredBitfield,PiecePid)
			end;
		
		
		is_interested ->
			{OldChoked, OldInterested} = Status,
			if not OldInterested ->
				TcpPid ! interested;
			   true ->
				   ok
			end,
			NewStatus = {OldChoked,true},
			loop(NewStatus,TcpPid,NextBlock,TorrentPid,StoredBitfield,PiecePid);
		
		not_interested ->
			case Status of
				{true,_} ->
					TcpPid ! not_interested ,
					NewStatus = {true,false} ;
				{false, _} ->
					TcpPid ! not_interested,
					NewStatus = {false,false}
			end,
			loop(NewStatus,TcpPid,NextBlock,TorrentPid,StoredBitfield,PiecePid);
		
		{got_unchoked, _FromPid} ->
			case Status of
				{_,true}->
					NewStatus= {false,true};
				{_,false} ->
					NewStatus = {false,false}
			end,
			case PiecePid of
				idle when (element(2, NewStatus))->
					loop(NewStatus,TcpPid,NextBlock,TorrentPid,StoredBitfield,free);
				_ ->
					loop(NewStatus,TcpPid,NextBlock,TorrentPid,StoredBitfield,PiecePid)
			end;
		
		{got_choked, _FromPid} ->
			case Status of
				{_,true} ->
					NewStatus ={true,true};
				{_,false} ->
					NewStatus = {true,false}
			end,
			loop(NewStatus,TcpPid,NextBlock,TorrentPid,StoredBitfield,PiecePid);
		{have,SenderPid,Piece_Index} ->
			case SenderPid of
				TcpPid ->
					TorrentPid ! {have,self(),Piece_Index};
				TorrentPid ->
					TcpPid ! { have,self(), Piece_Index}
			end,
			loop(Status,TcpPid,NextBlock,TorrentPid,StoredBitfield,PiecePid);
		
		{client_bitfield,SenderPid, Bitfield} ->
			case SenderPid of
				TcpPid ->
					TorrentPid ! {bitfield,self(),Bitfield};
				TorrentPid ->
					TcpPid ! {bitfield,Bitfield}
			end,
			loop(Status,TcpPid,NextBlock,TorrentPid,Bitfield,PiecePid);
		
		{got_block,Offset,Length,Data} ->
			PiecePid ! {block,self(),Offset,Length,Data},
			loop(Status,TcpPid,NextBlock,TorrentPid,StoredBitfield,free);
		{download_block,FromPid,Index,Offset,Length} ->
			case PiecePid of
				idle ->
					FromPid ! {ok, downloading},
					TcpPid ! {request, Index,Offset,Length},
					loop(Status,TcpPid,{Index,Offset,Length},TorrentPid,StoredBitfield,FromPid);
				_ ->
					FromPid ! {busy, self(),Offset},
					io:fwrite("~p: ~p  busy~n" ,[PiecePid, self()]),
					loop(Status,TcpPid,{Index,Offset,Length},TorrentPid,StoredBitfield,PiecePid)
			end	
		after 120000 ->
			TcpPid ! keep_alive
	end.
