-module(nettransfer).

-export([init/8,loop/4]).


init(MasterPid,DestinationIp,DestinationPort,InfoHash,ClientId,Name,Shas,Piece_length)->
    Pid = tcp:open_a_socket(DestinationIp, DestinationPort,InfoHash,ClientId, Name, Shas, Piece_length),
    Choked = true,
    Interested = false,
    Status= {Choked,Interested},
    loop(Status,Pid,{0,0,0},MasterPid).


loop(Status,Pid,NextBlock,MasterPid) ->
    receive
        got_unchoked ->
            case Status of
                {_,true}->
                    {Index,Offset,Length} = NextBlock,
                   Pid ! {piece,Index,Offset,Length} ,
                   NewStatus= {false,true};
                {_,false} ->
                    NewStatus = {false,false}
            end,
            loop(NewStatus,Pid,NextBlock,MasterPid);
        got_choked ->
            case Status of
                {_,true} ->
                    NewStatus ={true,true};
                {_,false} ->
                    NewStatus = {true,false}
            end,
            loop(NewStatus,Pid,NextBlock,MasterPid);
        is_interested ->
            case Status of
                {true,_} ->
                    Pid ! interested ,
                    NewStatus = {true,true};
                {false, _} ->
                      Pid ! interested ,
                    {Index,Offset,Length} = NextBlock,
                      Pid ! { piece,Index,Offset,Length},
                     NewStatus = {false,true}
            end,
                    loop(NewStatus,Pid,NextBlock,MasterPid);


        not_interested ->
          case Status of
              {true,_} ->
                  Pid ! not_interested ,
                  NewStatus = {true,false} ;
              {false, _} ->
                  Pid ! not_interested,
                  NewStatus = {false,false}
          end,
            loop(NewStatus,Pid,NextBlock,MasterPid)


    after 120000 ->
            Pid ! keep_alive

end.






