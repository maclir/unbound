
-module(bitfield).

-export([to_indexlist/2, compare/2, count_zeros/1, has_one_zero/1, flip_bit/2]).

has_one_zero(Bitfield) ->
	count_zeros(Bitfield) == 1.

count_zeros(BitField) ->
	length(to_indexlist(BitField, normal)).

to_indexlist(<<Bitfield/bitstring>>, invert) ->
    to_indexlist(0,Bitfield, true);
to_indexlist(<<Bitfield/bitstring>>, _) ->
    to_indexlist(0,Bitfield, false).

to_indexlist(_Index, <<>>, _) ->
    [];
to_indexlist(Index, <<H:1,Rest/bitstring>>, Invert) ->
    case H of
		0 when not(Invert) ->
			[{Index}|to_indexlist(Index+1,Rest,Invert)];
		1 when not(Invert)->
			to_indexlist(Index+1,Rest,Invert);
		0 when Invert ->
			to_indexlist(Index+1,Rest,Invert);
		1 when Invert->
			[{Index}|to_indexlist(Index+1,Rest,Invert)]
    end.

compare(FirstBitfield, SecondBitfield) ->
	compare(0, FirstBitfield, SecondBitfield, []).

compare(_Index,<<>>,<<>>, List) ->
    {result, List};
compare(Index,<<FirstBit:1,Rest1/bitstring>>,<<FirstBit:1,Rest2/bitstring>>, List) ->
	compare(Index+1, Rest1, Rest2, [Index|List]);
compare(Index,<<_FirstBit:1,Rest1/bitstring>>,<<_SecondtBit:1,Rest2/bitstring>>, List) ->
	compare(Index+1, Rest1, Rest2, List).

flip_bit(Index, <<Bitfield/bitstring>>) ->
	BitSize = bit_size(Bitfield),
	<<OrBitString:BitSize>> = math:pow(2, Index),
	<<BitfieldInt:BitSize>> = Bitfield,
	OrBitString bxor BitfieldInt.