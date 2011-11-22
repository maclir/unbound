-module(bitfield).

-export([bitfield_to_indexlist/2, compare_bitfields/2, count_zeros/1]).

count_zeros(BitField) ->
	length(bitfield_to_indexlist(BitField, normal)).

bitfield_to_indexlist(<<Bitfield/binary>>, invert) ->
    bitfield_to_indexlist(0,Bitfield, true);
bitfield_to_indexlist(<<Bitfield/binary>>, _) ->
    bitfield_to_indexlist(0,Bitfield, false).

bitfield_to_indexlist(_Index, <<>>, _) ->
    [];
bitfield_to_indexlist(Index, <<H:1,Rest/bitstring>>, Invert) ->
    case H of
		0 when not(Invert) ->
			[{Index}|bitfield_to_indexlist(Index+1,Rest)];
		1 when not(Invert)->
			bitfield_to_indexlist(Index+1,Rest);
		0 when Invert ->
			bitfield_to_indexlist(Index+1,Rest);
		1 when Invert->
			[{Index}|bitfield_to_indexlist(Index+1,Rest)]
    end.

compare_bitfields(FirstBitfield, SecondBitfield) ->
	compare_bitfields(0, FirstBitfield, SecondBitfield, []).

compare_bitfields(_Index,<<>>,<<>>, List) ->
    {result, List};
compare_bitfields(Index,<<FirstBit:1,Rest1/bitstring>>,<<FirstBit:1,Rest2/bitstring>>, List) ->
	compare_bitfields(Index+1, Rest1, Rest2, [Index|List]);
compare_bitfields(Index,<<_FirstBit:1,Rest1/bitstring>>,<<_SecondtBit:1,Rest2/bitstring>>, List) ->
	compare_bitfields(Index+1, Rest1, Rest2, List).