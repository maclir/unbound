%%%author: Nahid Vafaie ,Peter Myllykoski, Alireza Pazirandeh
%%% created: 18 Nov 2011

-module(nettransfer).

-export([init/6,init_upload/3]).

%%----------------------------------------------------------------------
%% Function:  init/6
%% Purpose:
%% Args:      TorrentPid(pid),DestinationIp(pid),DestinationPort(integer),InfoHash(stringt),ClientId(string),
%%            Bitfield(binary)
%% Returns:
%%----------------------------------------------------------------------
init(TorrentPid,DestinationIp,DestinationPort,InfoHash,ClientId,<<Bitfield/bitstring>>)->
	ZeroPaddedBitfield = pad_bitfield(Bitfield),
	TcpPid = spawn_link(tcp,open_a_socket,[DestinationIp, DestinationPort,InfoHash,ClientId,self()]),
	TcpPid ! {send_bitfield,ZeroPaddedBitfield},
	Choked = true,
	Interested = false,
	DownloadStatus= {Choked,Interested},
	UploadStatus = {Choked,Interested},
	loop(DownloadStatus,TcpPid,TorrentPid,0,[],UploadStatus).

%%----------------------------------------------------------------------
%% Function: init_upload/3
%% Purpose:  initializes the downloading process
%% Args:     TorrentPid(pid),TcpPid(pid),Bitfield(binary)
%%----------------------------------------------------------------------
init_upload(TorrentPid,TcpPid,<<Bitfield/bitstring>>) ->
	ZeroPaddedBitfield = pad_bitfield(Bitfield),
	TorrentPid ! {ok,self()},
	TcpPid ! {new_master_pid, self()},
	TcpPid ! {send_bitfield,ZeroPaddedBitfield},
	Choked = true,
	Interested = false,
	DownloadStatus = {Choked,Interested},
	UploadStatus = {Choked,Interested},
	loop(DownloadStatus,TcpPid,TorrentPid,0,[],UploadStatus).

%%----------------------------------------------------------------------
%% Function:  pad_bitfield/1
%% Purpose:   adds zeris to the bitfield so it becomes binary
%% Args:      Bitfield(binary)
%% Returns:   binary
%%----------------------------------------------------------------------
pad_bitfield(<<Bitfield/bitstring>>) ->
	BitLength = bit_size(Bitfield),
	case BitLength rem 8 of
		0 ->
			Bitfield;
		Rem ->
			Padding = 8 - Rem,
			<<Bitfield/bitstring,0:Padding>>
	end.

%%----------------------------------------------------------------------
%% Function:  loop/6
%% Purpose:   it exchanges messages with tcp process, torrent process and piece
%%            process and also keeps the state of one connection and filters the
%%            messages
%% Args:      DownloadStatus(tuple),TcpPid(pid),TorrentPid(pid),StoredBitfield(binary),Que(list),UploadStatus(tuple)
%%----------------------------------------------------------------------
loop(DownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que,UploadStatus) ->
	receive
		check_free ->
			case Que of
				[] when (not element(1,DownloadStatus) and element(2,DownloadStatus)) ->
					TorrentPid! {im_free,self()},
					receive
						{download_block,FromPid,Index,Offset,Length} ->
							TcpPid ! {request, Index,Offset,Length},
							loop(DownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que ++ [{FromPid,Index,Offset,Length}],UploadStatus)
						after 500 ->
							self() ! check_free,
							loop(DownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que,UploadStatus)
					end;
				_ ->
					loop(DownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que,UploadStatus)
			end;
		is_interested ->
			{OldChoked, OldInterested} = DownloadStatus,
			if not OldInterested ->
				   TcpPid ! interested,
					self() ! check_free;
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
%					case UploadStatus of
%						{_,false} ->
%							TcpPid ! stop;
%						{_,true} ->
							loop(NewDownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que,UploadStatus);

%					end;
				{false,_} ->
					TcpPid ! not_interested,
					NewDownloadStatus = {false,false},
%% 					case UploadStatus of
%% 						{_,false} ->
%% 							TcpPid ! stop;
%% 						{_,true} ->
							loop(NewDownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que,UploadStatus)
%% 					end
			end;


		choke ->
			TcpPid ! choke,
			{_Choked,Interested}= UploadStatus ,
			NewUploadStatus = {true,Interested},
			loop(DownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que,NewUploadStatus);

		{got_unchoked, _FromPid} ->
			case DownloadStatus of
				{_,true}->
					NewDownloadStatus= {false,true},
					self() ! check_free,
					loop(NewDownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que,UploadStatus);
				{_,false} ->
					NewDownloadStatus = {false,false},
					loop(NewDownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que,UploadStatus)
			end;


		{got_choked, _FromPid} ->
			case DownloadStatus of
				{_,true} ->
					NewDownloadStatus ={true,true},
					loop(NewDownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que,UploadStatus);
				{_,false} ->
					TcpPid ! {stop, got_choked},
					loop(DownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que,UploadStatus)
			end;

		{have,SenderPid,Piece_Index} ->
			case SenderPid of
				TcpPid ->
					TorrentPid ! {have,self(),Piece_Index};
				TorrentPid ->
					TcpPid ! { send_have, Piece_Index}
			end,
			loop(DownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que,UploadStatus);

		{client_bitfield,SenderPid, Bitfield} ->
			case SenderPid of
				TcpPid ->
					TorrentPid ! {bitfield,self(),Bitfield};
				TorrentPid ->
					TcpPid ! {send_bitfield,Bitfield}
			end,
			loop(DownloadStatus,TcpPid,TorrentPid,Bitfield,Que,UploadStatus);

		{got_block,Offset,Length,Data} ->
			[H|NewQue] = Que,
			element(1,H) ! {block,self(),Offset,Length,Data},
			case NewQue of
				[] ->
					self() ! check_free,
					loop(DownloadStatus,TcpPid,TorrentPid,StoredBitfield,NewQue,UploadStatus);
				[BlockInfo| _] ->
					TcpPid ! {request, element(2, BlockInfo), element(3, BlockInfo), element(4, BlockInfo)},
					loop(DownloadStatus,TcpPid,TorrentPid,StoredBitfield,NewQue,UploadStatus)
			end;

		{download_block,FromPid,Index,Offset,Length} ->
			case Que of
				[] ->
					TcpPid ! {request, Index,Offset,Length},
					loop(DownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que ++ [{FromPid,Index,Offset,Length}],UploadStatus);
				_ ->
					loop(DownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que ++ [{FromPid,Index,Offset,Length}],UploadStatus)
			end;

		bad_bitfield ->
			TcpPid ! {stop, bad_bitfield},
			loop(DownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que,UploadStatus);

		{got_interested ,TcpPid} ->
			case UploadStatus of
				{true,_} ->
					TcpPid ! unchoke,
					NewUploadStatus = {false,true};
				{false,_} ->
					NewUploadStatus={false,true}
			end,
			loop(DownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que,NewUploadStatus);


		{got_not_interested , TcpPid} ->
			case UploadStatus of
				{true,_} ->
					NewUploadStatus = {true,false},
					case DownloadStatus of
						{_,false} ->
							TcpPid ! {stop, got_not_interested},
							loop(DownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que,NewUploadStatus);
						{_,true} ->
							loop(DownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que,NewUploadStatus)
					end;
				{false,_} ->
					NewUploadStatus = {false,false},
					case DownloadStatus of
						{_,false} ->
							TcpPid ! {stop, got_not_interested},
							loop(DownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que,NewUploadStatus);
						{_,true} ->
							loop(DownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que,NewUploadStatus)
					end
			end;

		{got_request,TcpPid,Index,Offset,Length} ->
			Self = self(),
			TorrentPid ! {upload, Self,Index,Offset,Length},
			loop(DownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que,UploadStatus);

		{piece,Index,Offset,Binary} ->
		io:fwrite("Sending!!!"),
			TcpPid ! {send_piece,Index,Offset,Binary},
			loop(DownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que,UploadStatus)

		after 120000 ->
			TcpPid ! keep_alive,
			loop(DownloadStatus,TcpPid,TorrentPid,StoredBitfield,Que,UploadStatus)
	end.
