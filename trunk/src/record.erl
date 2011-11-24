-module(record).
-export([store/3]).
-include("torrent_db_records.hrl").

store(Key,Value,Record) ->
    case Key of
	<<"announce">> ->
	    NewRecord = Record#torrent{announce=Value};
	<<"announce-list">> ->
	    NewRecord = Record#torrent{announce_list=Value};
	<<"comment">> ->
	    NewRecord = Record#torrent{comment=Value};
	<<"created by">> ->
	    NewRecord = Record#torrent{created_by=Value};
	<<"creation date">> ->
	    NewRecord = Record#torrent{creation_date=Value};
	<<"encoding">> ->
	    NewRecord = Record#torrent{encoding=Value};
	<<"info">> ->
	    NewRecord = Record#torrent{info=Value};
	<<"piece length">> ->
	    NewRecord = Record#info{piece_length=Value};
	<<"pieces">> ->
	    NumberOfPieces = byte_size(Value) div 20,
	    Bitfield = <<0:NumberOfPieces>>,
	    NewRecord = Record#info{pieces=Value,bitfield = <<Bitfield/bitstring>>};
	<<"private">> ->
	    NewRecord = Record#info{private=Value};
	<<"name">> ->
	    NewRecord = Record#info{name=Value};
	<<"files">> ->
	    Sizes = [X || {_,X,_,_} <- Value],
	    TotalSize = lists:sum(Sizes),
	    NewRecord = Record#info{files=Value,length=TotalSize};
	<<"length">> ->
	    if 
		is_record(Record,info) ->
		    NewRecord = Record#info{length=Value};
		is_record(Record,file) ->
		    NewRecord = Record#file{length=Value}
	    end;
	<<"path">> ->
	    NewRecord = Record#file{path=Value};
	<<_>> ->
	    NewRecord = Record
    end,
    NewRecord.
