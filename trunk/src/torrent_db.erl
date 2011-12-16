-module(torrent_db).
-export([init/0, init_table/1, add/1, add/8, get_torrent_by_id/1, create_info_record/7, 
	 create_file_record/3, get_size/1, get_size_by_id/1, num_torrents/0, size_gt/1, size_lt/1, delete/1, 
	 delete_by_SHA1/1, find_by_SHA1/1, get_all_torrents/0, get_last/0, hash_exists/1]).
-import(utils_yavor).
-include("torrent_db_records.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% 
%%----------------------------------------------------------------------
%% Function:	init/0
%% Purpose:		Initializes mnesia and creates the download table. 
%% Returns:     ok | {error, Reason}
%%----------------------------------------------------------------------
init()->
    mnesia:start(),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    init_table(false).
   
%%----------------------------------------------------------------------
%% Function:	init_table/1
%% Purpose:		Creates a schema and the download table.
%% Args:		ForceRestart/binary
%% Returns:     ok
%%----------------------------------------------------------------------
init_table(false) ->
    mnesia:create_schema([node()]),
    mnesia:create_table(torrent, [{attributes, record_info(fields, torrent)}, {disc_copies, [node()]}, {type, ordered_set}]),
    ok;
init_table(true) ->
    mnesia:delete_table(torrent),
    init_table(false),
    ok.

%%----------------------------------------------------------------------
%% Function:	add/7
%% Purpose:		Creates a schema and the download table.
%% Args:		Info/#info, Announce/string, AnnounceList/list, CreationDate/string,
%%                  Comment/string, CreatedBy/string, Encoding:string
%% Returns:     ok
%%----------------------------------------------------------------------
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

%%----------------------------------------------------------------------
%% Function:	add/1
%% Purpose:		Adds a new entry to the torrent table.
%% Args:		Torrent/#torrent
%% Returns:     ok
%%----------------------------------------------------------------------
add(Torrent) -> 
   % Torrent#torrent.id = num_torrents(),
    {atomic, Result} = mnesia:transaction(fun()-> mnesia:write(Torrent#torrent{id=get_last()+1}) end),
    Result.
    
%% 
%%----------------------------------------------------------------------
%% Function:	get_torrent_by_id/1
%% Purpose:		Returns the #torrent with the specified id.
%% Args:		Id/integer
%% Returns:     Result/list
%%----------------------------------------------------------------------
get_torrent_by_id(Id) ->
    {atomic, [Result]} = mnesia:transaction(fun()-> mnesia:read(torrent, Id) end),
    Result.
    
%%----------------------------------------------------------------------
%% Function:	create_info_record/7
%% Purpose:		Creates and returns an #info record.
%% Args:		PieceLength/integer, Pieces/list, Private/integer, Name/string, Length/integer, Md5/string, Files/list
%% Returns:     Record/#info
%%----------------------------------------------------------------------
create_info_record(PieceLength, Pieces, Private, Name, Length, Md5, Files)->
    #info{piece_length=PieceLength, pieces=Pieces, private=Private, name=Name, length=Length, md5sum=Md5, files=Files}.

%%----------------------------------------------------------------------
%% Function:	create_file_record/3
%% Purpose:		Creates and returns an #file record.
%% Args:		Length/integer, Md5/string, Path/string
%% Returns:     Record/#file
%%----------------------------------------------------------------------
create_file_record(Length, Md5, Path)->
    #file{path=Path, md5sum=Md5, length=Length}.

%%----------------------------------------------------------------------
%% Function:	num_torrents/0
%% Purpose:		Returns the number of torrents added to the database.
%% Returns:     Num/integer
%%----------------------------------------------------------------------
num_torrents()->
    num_torrents(0, -1).
num_torrents(Num, Prev)->
    case mnesia:transaction(fun()-> mnesia:next(torrent, Prev) end) of
	{atomic, '$end_of_table'}->
	    Num;
	{atomic, Next} when Next =/= Prev ->
	    num_torrents(Num+1, Next);
	{atomic, Next} ->
	    num_torrents(Num, Next)
    end.
    
%%----------------------------------------------------------------------
%% Function:	get_last/0
%% Purpose:		Returns the last id in the table.
%% Returns:     Id/integer
%%----------------------------------------------------------------------
get_last()->
    case mnesia:dirty_last(torrent) of
	'$end_of_table' ->
	    -1;
	Last ->
	    Last
    end.

%%----------------------------------------------------------------------
%% Function:	get_size_by_id/1
%% Purpose:		Returns the total size of the torrent at the row with
%%              the given Id.
%% Args:        Id/integer
%% Returns:     Size/integer
%%----------------------------------------------------------------------
get_size_by_id(Id) -> 
    Torrent = get_torrent_by_id(Id),
    case Torrent#torrent.info#info.length of
        0 ->
	    get_size(Torrent, Torrent#torrent.info#info.files);
	Size ->
	    Size
    end.

%%----------------------------------------------------------------------
%% Function:	get_size/1
%% Purpose:		Returns the total size of the given torrent.
%% Args:        Torrent/#torrent
%% Returns:     Size/integer
%%----------------------------------------------------------------------
get_size(Torrent)->
    get_size(Torrent, Torrent#torrent.info#info.files).
get_size(Torrent, [H|T])->
    H#file.length + get_size(Torrent,T);
get_size(_, [])->
    0.  
    
%%----------------------------------------------------------------------
%% Function:	size_gt/1
%% Purpose:		Returns a list of all downloads greater 
%%              in size (in bytes) than the specified number.
%% Args:        Size/integer
%% Returns:     Result/list
%%----------------------------------------------------------------------
size_gt(Size) -> 
    Transaction = fun() ->
			  Query = qlc:q([Torrent || Torrent <- mnesia:table(torrent), get_size_by_id(Torrent#torrent.id) > Size]),
			  qlc:eval(Query)
		  end,
    {atomic, Result} = mnesia:transaction(Transaction),
    Result.
%%----------------------------------------------------------------------
%% Function:	size_lt/1
%% Purpose:		Returns a list of all downloads lesser 
%%              in size (in bytes) than the specified number. 
%% Args:        Size/integer
%% Returns:     Result/list
%%----------------------------------------------------------------------
size_lt(Size) ->
    Transaction = fun() ->
			  Query = qlc:q([Torrent || Torrent <- mnesia:table(torrent), get_size_by_id(Torrent#torrent.id) < Size]),
			  qlc:eval(Query)
		  end,
    {atomic, Result} = mnesia:transaction(Transaction),
    Result.

%%----------------------------------------------------------------------
%% Function:	delete/1
%% Purpose:		Deletes entries from the torrent table.
%% Args:        Id/integer | List/list
%% Returns:     ok | {error, Reason}
%%----------------------------------------------------------------------

%% If a list is 
delete([H|T])->
    Id = H#torrent.id,
    delete(Id),
    delete(T);
delete([])->
    ok;
delete(Id) ->
    {atomic, Result} = mnesia:transaction(fun()-> mnesia:delete({torrent, Id}) end),
    Result.

%%----------------------------------------------------------------------
%% Function:	delete/1
%% Purpose:		Deletes the entry in the table with the given hash value.
%% Args:        SHA/String
%% Returns:     ok | {error, Reason}
%%----------------------------------------------------------------------
delete_by_SHA1(SHA)->
    {atomic, Match} = mnesia:transaction(fun()-> mnesia:match_object(torrent, #torrent{id='_', info='_', announce='_', announce_list='_', encoding='_',
							creation_date='_', comment='_', created_by='_', info_sha=SHA, dir='_', status='_'}, read) end),
    delete(Match).

find_by_SHA1(SHA)->
    {atomic, Match} = mnesia:transaction(fun()-> mnesia:match_object(torrent, 
									   #torrent{id='_', info='_', announce='_', announce_list='_', encoding='_',
										    creation_date='_', comment='_', created_by='_', info_sha=SHA, 
										    dir='_', status='_'}, read) end),
    
    Match.
%%----------------------------------------------------------------------
%% Function:	get_all_torrents/1
%% Purpose:		Returns a list of all torrents in the table.
%% Returns:     List/list
%%----------------------------------------------------------------------
get_all_torrents()->
	    A = mnesia:transaction(fun()-> mnesia:match_object(torrent, 
									   #torrent{id='_', info='_', announce='_', announce_list='_', encoding='_',
										    creation_date='_', comment='_', created_by='_', info_sha='_', 
										    dir='_', status='_'}, read) end),
		{atomic, Match} = A,
    Match.
%%----------------------------------------------------------------------
%% Function:	hash_exists/1
%% Purpose:		Check if the given hash value exists in the table.
%% Args:        Hash/string
%% Returns:     true | false
%%----------------------------------------------------------------------
hash_exists(Hash) ->
    hash_exists(Hash, get_all_torrents()).
hash_exists(_, [])->
    false;
hash_exists(Hash, [H|T])->
    case H#torrent.info_sha == Hash of
        true -> true;
        _ -> hash_exists(Hash, T)
    end.
%% Test Code:
-include_lib("eunit/include/eunit.hrl").

init_test_()->
    [?_assert(init() == ok),
     ?_assert(init_table(true) == ok),
     ?_assert(init_table(false) == ok),
     ?_assertException(error, function_clause,  init_table(non_bool)),
     ?_assert(add( #info{piece_length=512, pieces=999, bitfield='_', private=0, name="first_torrent", length=10000, md5sum="md5sum", files=[]}, 
			    "first_sha", "announce", ["announce", "list"], 
			    "date", "comment", "created by", "encoding")== ok), %% Add a single-file entry to torrent table, id is 0. 
     ?_assert(num_torrents()==1), %% Get the number of torrent entries currently in the table.
     ?_assertMatch(#torrent{info=#info{name="first_torrent"}}, get_torrent_by_id(0)), %% Get torrent with id=0, should match the one that was added.
     ?_assertException(error, {badmatch,_}, get_torrent_by_id(1)), %% Trying to access torrent which doesn't exist. Crashes with badmatch error.
     ?_assert(add( #info{piece_length=512, pieces=999, bitfield='_', private=0, name="second_torrent", length=0, md5sum="md5sum", 
			 files=[#file{path="filepath1", length=2500}, #file{path="filepath2", length=4500}, #file{path="filepath3", length=13500}]}, 
		   "second_sha", "announce", ["announce", "list"], 
		   "date", "comment", "created by", "encoding")== ok), %% Add a multi-file entry to torrent table, id is 1.
     ?_assert(num_torrents()==2), %% Get the number of torrent entries currently in the table.
     ?_assert(get_size_by_id(0)==10000), %% Get correct size of single file entry.
     ?_assert(get_size_by_id(1)==20500), %% Get correct size of multi-file entry.
     ?_assertException(error, {badmatch,_}, get_size_by_id(2)), %% Trying to get the size of a torrent which doesn't exist.
     ?_assert(add( #info{piece_length=512, pieces=999, bitfield='_', private=0, name="third_torrent", length=5000, md5sum="md5sum", files=[]}, 
			    "third_sha", "announce", ["announce", "list"], 
			    "date", "comment", "created by", "encoding")== ok), %% Add another single-file entry to the torrent table, id is 2.
     ?_assert(num_torrents()==3), %% Get the number of torrent entries currently in the table.
     ?_assertMatch([#torrent{info=#info{name="first_torrent"}}, #torrent{info=#info{name="second_torrent"}}], size_gt(8000)),
     ?_assertMatch([#torrent{info=#info{name="first_torrent"}}, #torrent{info=#info{name="third_torrent"}}], size_lt(20500)),
     ?_assertMatch([#torrent{info=#info{name="first_torrent"}}], find_by_SHA1("first_sha")),
     ?_assert(delete(1)==ok), %% Delete torrent entry with id==1 (the one added second).
     ?_assert(num_torrents()==2),
     ?_assert(delete_by_SHA1("first_sha")==ok),
     ?_assert(num_torrents()==1),
     ?_assertMatch([_], get_all_torrents())
    ]. 
     
