-module(peerpiecemanagement).

-export([compare_bitfields/4,connect_to_peer/6,create_dummy_bitfield/1,getPeerList/2]).

-include("torrent_db_records.hrl").



getPeerList(Record,Id) ->
    InfoHash = Record#torrent.info_sha,
    InfoHashUrl = info_hash:url_encode(Record#torrent.info_sha),
    Announce = Record#torrent.announce,
    case Announce of
	<<"http",_Rest/binary>> ->
	    tcp:connect_to_server(Announce,InfoHashUrl,Id);
	<<"udp",_Rest/binary>> ->
	    {error,udp_not_supported}
    end.


connect_to_peer([{Ip,Port}|Rest],InfoHash,Id, Name, PiecesSha, Piece_length) ->
    [tcp:open_a_socket(Ip,Port,InfoHash,Id, Name, PiecesSha)|connect_to_peer(Rest,InfoHash,Id, Name, PiecesSha, Piece_length)];

connect_to_peer([],InfoHash,Id, _, _, _) ->
    [].

create_dummy_bitfield(Num) ->
    Int = Num div 8,
    case Num rem 8 of
	0 ->
	    ByteLenght = Int*8;
	_ ->
	    ByteLenght = (Int + 1)*8
    end,
    Lenght = (ByteLenght div 8) + 1,

    <<Lenght:32,5:8,0:ByteLenght>>.

compare_bitfields(<<Lenght:32,5,OurBitfield/binary>>,<<Lenght:32,5,Bitfield/binary>>,NumPieces,Pid) ->
    Size = bit_size(OurBitfield),
    case compare_bits(0,OurBitfield,Bitfield,NumPieces,Pid) of
	{result,nothing_needed} ->
	    ok;
	{result,Index} ->
	    IndexChanged = trunc(math:pow(2,Index)),
	    BitPattern = <<IndexChanged:Size>>,
	    ReversedBitPattern = reverse_bits(BitPattern),
	    ReversedBitPattern,
	    <<X:Size>> = OurBitfield,
	    <<Y:Size>> = ReversedBitPattern,
	    <<Lenght:32,5,(X bor Y):Size>>
    end.



compare_bits(Index,<<OurFirstBit:1,OurRest/bitstring>>,<<FirstBit:1,Rest/bitstring>>,NumPieces,Pid) ->
    if
	OurFirstBit == 0,FirstBit == 1,Index<NumPieces ->
	    Pid ! {get_piece,Index},
	    {result,Index};
	true ->
	    compare_bits(Index+1,OurRest,Rest,NumPieces,Pid)
    end;

compare_bits(_Index,<<>>,<<>>,_NumPieces,_Pid) ->
    {result,nothing_needed}.

reverse_bits(<<X:1,Rest/bits>>) ->
<<(reverse_bits(Rest))/bits,X:1>>;
reverse_bits(<<>>) ->
<<>>.

