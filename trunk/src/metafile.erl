%%% @author  <Peter@NIDINGEN>
%%% @copyright (C) 2011, 
%%% @doc
%%% 
%%% @end
%%% Created : 19 Oct 2011 by  <Peter@NIDINGEN>

-module(metafile).
-export([parse/1]).
-import(binary).
-import(crypto).
-include("torrent_db_records.hrl").

%% Function that takes a bencoded binary metafile stream and extracts the 
%% information into a record that is returned.
parse(File) ->
    Record = calculateSHA1(File),
    FinalRecord = parseData(bencode:decode(File),Record),
    {ok,FinalRecord}.

%% Function that finds the info part of the metafile, calculates a SHA1 and
%% inserts it into a new record
calculateSHA1(Binary) ->
    case Binary of
	<<$4,$:,$i,$n,$f,$o,A/binary>> ->
	    Info = binary:part(A,0,byte_size(A)-1),
	    SHA1 = crypto:sha(binary_to_list(Info)),
	    #torrent{info_sha=SHA1};
	_ ->
	    <<_,End/binary>> = Binary,
	    calculateSHA1(End)
    end.


parseData({ok,DecodedInformation},Record) ->
    parseData(DecodedInformation,Record);

parseData({dict,Dictionary},Record) ->
    parseData(Dictionary,Record);

parseData({list,List},Record) ->
    parseData(List,Record);

parseData([{<<"announce">>,Value}|Tail],Record) ->
    NewRecord = Record#torrent{announce=Value},
    parseData(Tail,NewRecord);

parseData([{<<"announce-list">>,Value}|Tail],Record) ->
    AnnounceList = lists:flatten(parseData(Value,[])),
    NewRecord = Record#torrent{announce_list=AnnounceList},
    parseData(Tail,NewRecord);

parseData([{<<"comment">>,Value}|Tail],Record) ->
    NewRecord = Record#torrent{comment=Value},
    parseData(Tail,NewRecord);

parseData([{<<"created by">>,Value}|Tail],Record) ->
    NewRecord = Record#torrent{created_by=Value},
    parseData(Tail,NewRecord);

parseData([{<<"creation date">>,Value}|Tail],Record) ->
    NewRecord = Record#torrent{creation_date=Value},
    parseData(Tail,NewRecord);

parseData([{<<"encoding">>,Value}|Tail],Record) ->
    NewRecord = Record#torrent{encoding=Value},
    parseData(Tail,NewRecord);

parseData([{<<"info">>,Value}|Tail],Record) ->
    InfoRecord = #info{},
    NewInfoRecord = parseData(Value,InfoRecord),
    NewRecord = Record#torrent{info=NewInfoRecord},
    parseData(Tail,NewRecord);

parseData([{<<"piece length">>,Value}|Tail],Record) ->
    NewRecord = Record#info{piece_length=Value},
    parseData(Tail,NewRecord);

parseData([{<<"pieces">>,Value}|Tail],Record) ->
    NumPieces = byte_size(Value) div 20,
    Bitfield = <<0:NumPieces>>,
    NewRecord = Record#info{pieces=Value,bitfield = <<Bitfield/bitstring>>},
    parseData(Tail,NewRecord);

parseData([{<<"private">>,Value}|Tail],Record) ->
    NewRecord = Record#info{private=Value},
    parseData(Tail,NewRecord);
    
parseData([{<<"name">>,Value}|Tail],Record) ->
    NewRecord = Record#info{name=Value},
    parseData(Tail,NewRecord);

parseData([{<<"files">>,Value}|Tail],Record) ->
    FilesRecord = #file{},
    NewFilesRecord = parseData(Value,FilesRecord),
    NewRecord = Record#info{files=NewFilesRecord},
    parseData(Tail,NewRecord);


parseData([{<<"length">>,Value}|Tail],Record) ->
    if
	is_record(Record,info) ->
	    NewRecord = Record#info{length=Value};
        is_record(Record,file) ->
	    NewRecord = Record#file{length=Value}
    end,
    parseData(Tail,NewRecord);


parseData([{<<"path">>,{list,Value}}|Tail],Record) ->
    PathList = buildPathList(Value),
    NewRecord = Record#file{path=PathList},
    parseData(Tail,NewRecord);

parseData([{<<"locale">>,_Value}|Tail],Record) ->
    parseData(Tail,Record);

parseData([{<<"modification date">>,_Value}|Tail],Record) ->
    parseData(Tail,Record);

parseData([{<<"title">>,_Value}|Tail],Record) ->
    parseData(Tail,Record);

parseData([Head|Tail],Record) ->
    HeadValue = parseData(Head,Record),
    TailValue = parseData(Tail,Record),
    [HeadValue|TailValue];

parseData([],Record) ->
    Record;

parseData(<<Value/binary>>,_Record) ->
    Value.

%% parseData(<<Value/binary>>) ->
%%    Value.

buildPathList([Head|Tail]) ->
    [Head|buildPathList(Tail)];

buildPathList([]) ->
    [].

