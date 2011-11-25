%% Author: Evelina Vorobyeva
%% Created: Nov 18, 2011

-module(file_split).
-export([start/5, path_create/2, merge_data/4]).
-include("torrent_db_records.hrl").

%% Starting function
start(Data, StartPos, PieceLength, TempPath, Records) ->
	Files = calc_files(StartPos, PieceLength, Records#torrent.info#info.files, 0, []),
	write_to_file(Files, Data, TempPath, Records).

%% Function to write the data to the file
write_to_file([], _, _, _) ->
	{error, no_file_match};
write_to_file([{BinaryPath, StartPos, Length}|[]], Data, TempPath, Records) ->
	{Name, Path} = path_create(BinaryPath, ""),
	FilePath = TempPath ++ Path,
	filelib:ensure_dir(FilePath),
	{ok, Index} = file:open(FilePath ++ Name, [read, write]),	
	file:pwrite(Index, StartPos + 1, Data),
	file:close(Index),
	change_length_db(Length, Path, Records),
	{ok, done};
write_to_file([{BinaryPath, StartPos, Length}|T], AllData, TempPath, Records) ->
	{Name, Path} = path_create(BinaryPath, ""),
	<<Data:Length/binary, Rest/binary>> = AllData,
	FilePath = TempPath ++ Path,
	filelib:ensure_dir(FilePath),
	{ok, Index} = file:open(FilePath ++ Name, [read, write]),	
	file:pwrite(Index, StartPos + 1, Data),
	file:close(Index),
	change_length_db(Length, Path, Records),
	write_to_file(T, Rest, TempPath, Records).

%% Function to change the binary data into Strings and create the path for saving the data on the HD
path_create([H|[]], String) ->
	Name = binary_to_list(H),
 	{Name, String};
path_create([H|T], String) ->
	Path = String ++ binary_to_list(H) ++ "/",
	path_create(T, Path).

%% Merging the binary data from the files
%% Files = Records#torrent.info#info.files
merge_data(StartPos, Length, Files, TorrentPath) ->
	FileMap = calc_files(StartPos, Length, Files, 0, []),
	merge_data(TorrentPath, FileMap, <<>>).

merge_data(_, [], Data) ->
	Data;
merge_data(TorrentPath, [{Path, Name, StartPos, Length}|T], BinaryData) ->
	{ok, File} = file:open(TorrentPath ++ Path ++ Name, [read]),
	{ok, Data} = file:pread(File, StartPos, Length),
	file:close(File),
	NewData = <<BinaryData, Data>>,
	merge_data(TorrentPath, T, NewData).


%% Updating the downloaded file's length in the db
change_length_db(Length, Path, Records) ->
	Torrent = torrent_db:find_by_SHA1(Records#torrent.info_sha),
	Files = Torrent#torrent.info#info.files,
	Tuple = lists:keyfind(Path, 4, Files),
	{file, FileLength, MD5, Path, LengthComplete} = Tuple,
	NewLength = LengthComplete + Length,
	NewTuple = {file, FileLength, MD5, Path, NewLength},
	NewList = lists:keyreplace(Path, 4, Files, NewTuple),
	torrent_db:delete_by_SHA1(Records#torrent.info_sha),
	torrent_db:add(Records#torrent{info = (Records#torrent.info)#info{files = NewList}}).
			
												
%% co-author: Alireza Pazirandeh
%% The function for calculating the positions of of the pieces inside the files
%% the startPos < startFilePos, it is over
calc_files(StartPos, Length, _, StartFilePos, Files)
	when (StartPos + Length =< StartFilePos) ->
		lists:reverse(Files);
%% the startPos and endPos < endFilePos, all the block belongs here
calc_files(StartPos, Length, [H|_], StartFilePos, Files)
	when ((StartPos < StartFilePos + H#file.length) and (StartPos + Length =< StartFilePos + H#file.length)) ->
		NewFile = {H#file.path, StartPos - StartFilePos, Length},
		[NewFile|Files];
%% the startPos < endFilePos but endPos > endFilePos, starts in this file but still continues
calc_files(StartPos, Length, [H|T], StartFilePos, Files)
	when ((StartPos < StartFilePos + H#file.length) and (StartPos + Length > StartFilePos + H#file.length)) ->
		NewFile = {H#file.path, StartPos - StartFilePos, StartFilePos + H#file.length - StartPos},
		calc_files(StartPos, Length, T, StartFilePos + H#file.length, [NewFile|Files]);
%% the endPos > endFilePos, started before and it ends later
calc_files(StartPos, Length, [H|T], StartFilePos, Files)
	when (StartPos + Length > StartFilePos + H#file.length) ->
		NewFile = {H#file.path, 0, H#file.length},
		calc_files(StartPos, Length, T, StartFilePos + H#file.length, [NewFile|Files]);
%% the endPos < endFilePos, started before and it ends here
calc_files(StartPos, Length, [H|_], StartFilePos, Files)
	when (StartPos + Length =< StartFilePos + H#file.length) ->
		NewFile = {H#file.path, 0, StartPos + Length - StartFilePos},
		[NewFile|Files];
calc_files(StartPos, Length, [H|T], StartFilePos, Files) ->
	calc_files(StartPos, Length, T, StartFilePos + H#file.length, Files).