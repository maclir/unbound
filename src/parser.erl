%%% @author Peter Myllykoski and Alireza Pazirandeh
%%% Based on code found at wiki.theory.org/Decoding_encoding_bencoded_data_with_erlang

-module(parser).

-export([decode/1]).
-define(DICT, orddict).
-include("torrent_db_records.hrl").

%%----------------------------------------------------------------------
%% Function: decode/1
%% Purpose:  API function for the module
%% Args:     Data(list)
%% Returns:  {ok,Record} or {error, unparsed}
%%----------------------------------------------------------------------
decode(Data) ->
    case catch dec(Data,#torrent{}) of
	{'EXIT', _} ->
	    {error, unparsed};
	{Res, _} ->
	    InfoSha = calculateInfoSha(Data),
	    NewRecord = Res#torrent{info_sha=InfoSha},
	    {ok, NewRecord}
    end.

%%----------------------------------------------------------------------
%% Function:   dec/2
%% Purpose:    determine what type of decoding should be done
%% Args:       Data(list),Record(list)
%%----------------------------------------------------------------------
dec(<<$l, Tail/binary>>,Record) ->
    dec_list(Tail, [],Record);
dec(<<$d, Tail/binary>>,Record) ->
    dec_dict(Tail,Record);
dec(<<$i, Tail/binary>>,_Record) ->
    dec_int(Tail, []);
dec(Data,_Record) ->
    dec_string(Data, []).

%%----------------------------------------------------------------------
%% Function:    dec_int/2
%% Purpose:     decoding a bencoded integer
%% Args:        <<X,Tail/binary,Acc(list)
%% Returns:     integer
%%----------------------------------------------------------------------
dec_int(<<$e, Tail/binary>>, Acc) ->
    {list_to_integer(lists:reverse(Acc)), Tail};
dec_int(<<X, Tail/binary>>, Acc) ->
    dec_int(Tail, [X|Acc]).

%%----------------------------------------------------------------------
%% Function:    dec_string/2
%% Purpose:     decode a string
%% Args:        <<X, Tail/binary>>, Acc (list)
%% Returns:     string
%%----------------------------------------------------------------------
dec_string(<<$:, Tail/binary>>, Acc) ->
    Int = list_to_integer(lists:reverse(Acc)),
    <<Str:Int/binary, Rest/binary>> = Tail,
    {Str, Rest};
dec_string(<<X, Tail/binary>>, Acc) ->
    dec_string(Tail, [X|Acc]).

%%----------------------------------------------------------------------
%% Function:  dec_list/3
%% Purpose:   decode a list
%% Args:      Data(list),Acc(list),Record(list of records)
%% Returns:   list
%%----------------------------------------------------------------------
dec_list(<<$e, Tail/binary>>, Acc,_Record) ->
    {lists:reverse(Acc), Tail};
dec_list(Data, Acc, Record) ->
    {Res, Tail} = dec(Data, Record),
    dec_list(Tail, [Res|Acc],Record).

%%----------------------------------------------------------------------
%% Function:	dec_dict/2
%% Purpose:     decode a dictionary and insert the key value pair into the record
%% Args:		Data(list),Record(list of records)
%%----------------------------------------------------------------------
dec_dict(<<$e, Tail/binary>>, Record) ->
    {Record, Tail};
dec_dict(Data, Record) ->
    {Key, Tail1} = dec(Data,Record),
    case Key of
	<<"info">> ->
	    NewRecord = #info{};
	<<"files">> ->
	    NewRecord = #file{};
	_ ->
	    NewRecord = Record
    end,
    {Val, Tail2} = dec(Tail1,NewRecord),
    dec_dict(Tail2, record:store(Key,Val,Record)).

%%----------------------------------------------------------------------
%% Function:  calculateInfoSha/1
%% Purpose:   calculates sha hash for the given binary data
%% Args:      binary
%% Returns:   sha hash
%%----------------------------------------------------------------------
calculateInfoSha(Binary) ->
    case Binary of
	<<$4,$:,$i,$n,$f,$o,A/binary>> ->
	    Info = binary:part(A,0,byte_size(A)-1),
	    SHA1 = crypto:sha(binary_to_list(Info)),
	    SHA1;
	_ ->
	    <<_,End/binary>> = Binary,
	    calculateInfoSha(End)
    end.
