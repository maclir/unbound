-module(torrent_db).
-export([init/0, init_table/1, add/1, add/8, get_torrent_by_id/1, create_info_record/7, 
	 create_file_record/3, get_size_by_id/1, num_torrents/0, size_gt/1, size_lt/1, delete/1, delete_by_SHA1/1, find_by_SHA1/1]).
-import(utils_yavor).
-include("torrent_db_records.hrl").
-include_lib("stdlib/include/qlc.hrl").

% Initializes mnesia and creates the download table. TESTEDy
init()->
    mnesia:start(),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    init_table(false).
   
% Creates a schema and the download table. TESTED
init_table(false) ->
    mnesia:create_schema([node()]),
    mnesia:create_table(torrent, [{attributes, record_info(fields, torrent)}, {disc_copies, [node()]}, {type, ordered_set}]);
init_table(true) ->
    mnesia:delete_table(torrent),
    init_table(false),
    record_info(fields, torrent).

%% Adds a new entry to the torrent table.
%% Valid attributes:
%% -Info: record(#info)
%% -Announce: String
%% -AnnounceList: List of strings
%% -CreationDate: Integer
%% -Comment: String
%% -CreatedBy: String
%% -Encoding: String
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add(Info, InfoSHA, Announce, AnnounceList, CreationDate, Comment, CreatedBy, Encoding) ->
    Torrent = #torrent {
            info=Info, info_sha=InfoSHA,
            announce=Announce, 
            announce_list=AnnounceList, 
            creation_date=CreationDate,
            comment=Comment, 
            created_by=CreatedBy,
            encoding=Encoding
        },
    add(Torrent).

%% Adds a new entry to the torrent table.
%% Valid attributes:
%% -Torrent: #torrents
%% The id value is automatically assigned in the function.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add(Torrent) -> 
   % Torrent#torrent.id = num_torrents(),
    Result = mnesia:dirty_write(Torrent#torrent{id=num_torrents()}),
    Result.
    
%% Returns the #torrent with the specified id.
get_torrent_by_id(Id) ->
    [Result] = mnesia:dirty_read(torrent, Id),
    Result.
    

%% Creates and returns an #info record.
create_info_record(PieceLength, Pieces, Private, Name, Length, Md5, Files)->
    #info{piece_length=PieceLength, pieces=Pieces, private=Private, name=Name, length=Length, md5sum=Md5, files=Files}.

%% Creates and returns a #file record.
create_file_record(Length, Md5, Path)->
    #file{path=Path, md5sum=Md5, length=Length}.

%% Returns a list of all downloads that match the specified pattern. 
%% The Pattern should be a #torrent.
%% match(Pairs) ->
%%     Result = mnesia:dirty_match_object(match(#torrent{}, Pairs)),
%%     Result.
%% match(Pattern, [{K, V}|T]) ->
%%     match(Pattern#torrent{K=V}, T);
%% match(Pattern, []) ->
%%     Pattern.

%% Returns the number of torrents added to the database.
num_torrents()->
     case mnesia:dirty_last(torrent) of
	'$end_of_table' -> 
 	    0;
 	Last -> 
 	    Last + 1
     end.    
%% Returns the sum of the sizes of all files described 
%% in the torrent at the specified index in the table.
get_size_by_id(Id) -> 
    Torrent = get_torrent_by_id(Id),
    case Torrent#torrent.info#info.length of
        0 ->
	    get_size(Torrent, Torrent#torrent.info#info.files);
	Size ->
	    Size
    end.

get_size(Torrent, [H|T])->
    H#file.length + get_size(Torrent,T);
get_size(_, [])->
    0.

%% Returns a list of all downloads greater 
%% in size (in bytes) than the specified number.
size_gt(Size) -> 
    Transaction = fun() ->
			  Query = qlc:q([Torrent || Torrent <- mnesia:table(torrent), get_size_by_id(Torrent#torrent.id) > Size]),
			  qlc:eval(Query)
		  end,
    {atomic, Result} = mnesia:transaction(Transaction),
    Result.

%% Returns a list of all downloads lesser 
%% in size (in bytes) than the specified number.    
size_lt(Size) ->
    Transaction = fun() ->
			  Query = qlc:q([Torrent || Torrent <- mnesia:table(torrent), get_size_by_id(Torrent#torrent.id) < Size]),
			  qlc:eval(Query)
		  end,
    {atomic, Result} = mnesia:transaction(Transaction),
    Result.


delete([H|T])->
    Id = H#torrent.id,
    delete(Id),
    delete(T);
delete([])->
    ok;
delete(Id) ->
    mnesia:dirty_delete({torrent, Id}).

delete_by_SHA1(SHA)->
    Match = mnesia:dirty_match_object(torrent, #torrent{id='_', info='_', announce='_', announce_list='_', encoding='_',
							creation_date='_', comment='_', created_by='_', info_sha=SHA, dir='_', status='_'}),
    delete(Match).

finb_by_SHA1(SHA)->
    mnesia:dirty_match_object(torrent, #torrent{id='_', info='_', announce='_', announce_list='_', encoding='_',
							creation_date='_', comment='_', created_by='_', info_sha=SHA, dir='_', status='_'}).
