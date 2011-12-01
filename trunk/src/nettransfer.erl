%%%author: Nahid Vafaie ,Peter Myllykoski
%%% created: 18 Nov 2011

-module(nettransfer).

-export([init/5]).


init(TorrentPid,DestinationIp,DestinationPort,InfoHash,ClientId)->
    TcpPid=tcp:open_a_socket(DestinationIp, DestinationPort,InfoHash,ClientId),
    Choked = true,
    Interested = false,
    DownloadStatus= {Choked,Interested},
    UploadStatus = {Choked,Interested},
    loop(DownloadStatus,TcpPid,TorrentPid,0,[]).


loop(DownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que,UploadStatus) ->
    receive
	check_free ->
	    case PiecePid of
		idle when (not element(1,DownloadStatus) and element(2,DownloadStatus)) ->
		    loop(DownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que,UploadStatus);
		_ ->
		    loop(DownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que,UploadStatus)
	    end;
	is_interested ->
	    {OldChoked, OldInterested} = DownloadStatus,
	    if not OldInterested ->
			TcpPid ! interested;
		   true ->
			ok
		end,
	    NewDownloadStatus = {OldChoked,true},
	    loop(NewDownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que,UploadStatus);
	not_interested ->
	    case DownloadStatus of
		{true,_} ->
		    TcpPid ! not_interested,
		    NewDownloadStatus = {true,false},
		    case UploadStatus of
			{_,false} ->
			    TcpPid ! stop;
			{_,true} ->
			    ok
		    end;
		{false,_} ->
		    TcpPid ! not_interested,
		    NewDownloadStatus = {false,false},
		    case UploadStatus of
			{_,false} ->
			    TcpPid ! stop;
			{_,true} ->
			    ok
		    end;
	    end,
 	    loop(NewDownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que,UploadStatus);
	    
	    {got_unchoked, _FromPid} ->
		case DownloadStatus of
			    {_,true}->
				NewDownloadStatus= {false,true};
			    {_,false} ->
				NewDownloadStatus = {false,false}
			end,
		loop(NewDownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que,UploadStatus);
	    
	    {got_choked, _FromPid} ->
		case DownloadStatus of
			    {_,true} ->
				NewDownloadStatus ={true,true};
			    {_,false} ->
				NewDownloadStatus = {true,false}
			end,
		loop(NewDownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que,UploadStatus);
	    {have,SenderPid,Piece_Index} ->
		case SenderPid of
			    TcpPid ->
				TorrentPid ! {have,self(),Piece_Index};
			    TorrentPid ->
				TcpPid ! { have,self(), Piece_Index}
			end,
		loop(DownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que,UploadStatus);
	    
	    {client_bitfield,SenderPid, Bitfield} ->
		case SenderPid of
			    TcpPid ->
				TorrentPid ! {bitfield,self(),Bitfield};
			    TorrentPid ->
				TcpPid ! {bitfield,Bitfield}
			end,
		loop(DownloadStatus,TcpPid,TorrentPid,Bitfield,Que,UploadStatus);
	    
	    {got_block,Offset,Length,Data} ->
	    [H,NewQue] = Que,
	    element(1,H) ! {block,self(),Offset,Length,Data},
		case NewQue of
			    [] ->
				loop(DownloadStatus,TcpPid,TorrentPid,StoredBitfield,NewQue,UploadStatus);
			    [{FromPid,Index,Offset,Length}|T] ->
				TcpPid ! {request, Index,Offset,Length},	
				loop(DownloadStatus,TcpPid,TorrentPid,StoredBitfield,NewQue,UploadStatus)
			end;
	    {download_block,FromPid,Index,Offset,Length} ->
		loop(DownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que ++ [{FromPid,Index,Offset,Length}],UploadStatus);
	    bad_bitfield ->
		TcpPid ! stop;
	after 120000 ->
		%% shouldnt there be a loop after this line??
		TcpPid ! keep_alive
	end.
