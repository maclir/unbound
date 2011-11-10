%%% @author Peter Myllykoski <peter@UL30JT>
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created : 10 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(info_hash).
-export([to_hex/1,url_encode/1]).

to_hex(<<C1:4,C2:4,Rest/binary>>) ->
    if 
	C1<10 ->
	    Char1 = C1+48;
	C1>=10 ->
	    Char1 = C1+55
    end,
    if 
	C2<10 ->
	    Char2 = C2+48;
	C2>=10 ->
	    Char2 = C2+55
    end,
    [Char1,Char2|to_hex(Rest)];

to_hex(<<>>) ->
    [].

url_encode(<<Byte:8,Rest/binary>>) ->
    if
	Byte>=$0,$9=<Byte ->
	    Enc = <<Byte>>;
	Byte>=$A,$Z=<Byte ->
	    Enc = <<Byte>>;
	Byte>=$a,$z=<Byte ->
	    Enc = <<Byte>>;
	Byte==$-;Byte==$_;Byte==$.;Byte==$~ ->
	    Enc = <<Byte>>;
	true ->
	    Percent = <<"%">>,
	    Enc = <<Percent/binary,Byte>>
    end,
    if
	Rest == <<>> ->
	    Enc;
	Rest == <<Rest/binary>> ->
	    EncRest = url_encode(Rest),
	    <<Enc/binary,EncRest/binary>>
    end.
