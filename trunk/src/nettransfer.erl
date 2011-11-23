%%%author: Nahid Vafaie ,Peter Myllykoski
%%% created: 18 Nov 2011

-module(nettransfer).

-export([init/5,loop/6]).


init(TorrentPid,DestinationIp,DestinationPort,InfoHash,ClientId)->
    TcpPid=tcp:open_a_socket(DestinationIp, DestinationPort,InfoHash,ClientId),
    Choked = true,
    Interested = false,
    Status= {Choked,Interested},
    loop(Status,TcpPid,{0,0,0},TorrentPid,0,piecepid).


loop(Status,TcpPid,NextBlock,TorrentPid,StoredBitfield,PiecePid) ->
    receive
        got_unchoked ->
            case Status of
                {_,true}->
%                    {_Index,_Offset,_Length} = NextBlock,
                    TorrentPid ! {get_block,StoredBitfield},
                    NewStatus= {false,true};
                {_,false} ->
                    NewStatus = {false,false}
            end,
            loop(NewStatus,TcpPid,NextBlock,TorrentPid,StoredBitfield,PiecePid);

        got_choked ->
            case Status of
                {_,true} ->
                    NewStatus ={true,true};
                {_,false} ->
                    NewStatus = {true,false}
            end,
            loop(NewStatus,TcpPid,NextBlock,TorrentPid,StoredBitfield,PiecePid);

        is_interested ->
            case Status of
                {true,_} ->
                    TcpPid ! interested ,
                    NewStatus = {true,true};
                {false, _} ->
                    TorrentPid ! interested,
                    TorrentPid ! {get_block,StoredBitfield},
                    NewStatus = {false,true}
            end,
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

        {have,SenderPid,Piece_Index} ->
            case SenderPid of
                TcpPid ->
                    TorrentPid ! {have,Piece_Index};
                TorrentPid ->
                    TcpPid ! { have, Piece_Index}
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
	    loop(Status,TcpPid,NextBlock,TorrentPid,StoredBitfield,piecepid);

        {download_block,FromPid,Index,Offset,Length} ->
            case PiecePid of
                piecepid ->
		    FromPid ! {ok, downloading},
                    TcpPid ! {piece, Index,Offset,Length},
                    loop(Status,TcpPid,{Index,Offset,Length},TorrentPid,StoredBitfield,FromPid);
                _ ->
                    FromPid ! {busy, self(),Offset},
                    loop(Status,TcpPid,{Index,Offset,Length},TorrentPid,StoredBitfield,PiecePid)

            end

    after 120000 ->
            TcpPid ! keep_alive

    end.






