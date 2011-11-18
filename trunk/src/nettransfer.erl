-module(nettransfer).

-export([init/0,loop/1]).


init()->
    %%tcp:connect_to_client(self(), Socket,InfoHash,ClientId, Name, Shas, Piece_length),
    Choked = true,
    Interested = false,
    Status= {Choked,Interested},
    loop(Status).


loop(Status) ->
    receive
        got_unchoked ->
            case Status of
                {_,true}->
                   NewStatus= {false,true};
                {_,false} ->
                    NewStatus = {false,false}
            end,
            loop(NewStatus);
        got_choked ->
            case Status of
                {_,true} ->
                    NewStatus ={true,true};
                {_,false} ->
                    NewStatus = {true,false}
            end,
            loop(NewStatus);
        is_interested ->
            {Choked,_} =Status,
            NewStatus = {Choked,true},
            loop(NewStatus);

        not_interested ->
            {Choked,_} =Status,
            NewStatus = {Choked , false} ,
            loop(NewStatus)

    end.




