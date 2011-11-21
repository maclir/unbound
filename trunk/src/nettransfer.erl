%%%author: Nahid Vafaie ,Peter Myllykoski
%%% created: 18 Nov 2011

-module(nettransfer).

-export([init/8,loop/5]).


init(MasterPid,DestinationIp,DestinationPort,InfoHash,ClientId,Name,Shas,Piece_length)->
    Pid = tcp:open_a_socket(DestinationIp, DestinationPort,InfoHash,ClientId, Name, Shas, Piece_length),
    Choked = true,
    Interested = false,
    Status= {Choked,Interested},
    loop(Status,Pid,{0,0,0},MasterPid,0).


loop(Status,Pid,NextBlock,MasterPid,StoredBitfield) ->
    receive
        got_unchoked ->
            case Status of
                {_,true}->
                    {Index,Offset,Length} = NextBlock,
		    MasterPid ! {get_block,Bitfield},
		    NewStatus= {false,true};
                {_,false} ->
                    NewStatus = {false,false}
            end,
            loop(NewStatus,Pid,NextBlock,MasterPid,StoredBitfield);
        got_choked ->
            case Status of
                {_,true} ->
                    NewStatus ={true,true};
                {_,false} ->
                    NewStatus = {true,false}
            end,
            loop(NewStatus,Pid,NextBlock,MasterPid,StoredBitfield);
        is_interested ->
            case Status of
                {true,_} ->
                    Pid ! interested ,
                    NewStatus = {true,true};
                {false, _} ->
		    Pid ! interested,
		    MasterPid ! {get_block,Bitfield}
		    NewStatus = {false,true}
            end,
                    loop(NewStatus,Pid,NextBlock,MasterPid,StoredBitfield);

        not_interested ->
          case Status of
              {true,_} ->
                  Pid ! not_interested ,
                  NewStatus = {true,false} ;
              {false, _} ->
                  Pid ! not_interested,
                  NewStatus = {false,false}
          end,
            loop(NewStatus,Pid,NextBlock,MasterPid,StoredBitfield);

        {have,SenderPid,Piece_Index} ->
            case SenderPid of
                Pid ->
                    MasterPid ! {have,Piece_Index};
                MasterPid ->
                    Pid ! { have, Piece_Index}
            end,
	    loop(Status,Pid,NextBlock,MasterPid,StoredBitfield);
        {bitfield,SenderPid, Bitfield} ->
            case SenderPid of
                Pid ->
                    MasterPid ! {bitfield,self(),Bitfield},
                MasterPid ->
                    Pid ! {bitfield,Bitfield},
            end,
	    loop(Status,Pid,NextBlock,MasterPid,Bitfield);
	{start_download,{Index,Offset,Lenght}} ->
	    Pid ! {request, Index,Offset,Lenght},
	    loop(Status,Pid,{Index,Offset,Lenght},MasterPid,Bitfield);
    after 120000 ->
            Pid ! keep_alive

    end.






