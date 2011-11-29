%% Author: Evelina Vorobyeva
%% Created: Nov 18, 2011

-module(file_split).
-export([start/5, path_create/2, request_data/3, test/0]).
-include("torrent_db_records.hrl").


test() ->
	io:fwrite("~p~n" ,[torrent_db:get_all_torrents()]).

%% Starting function
start(Data, StartPos, PieceLength, TempPath, Records) ->
	if
		is_list(Records#torrent.info#info.files) ->
			Files = calc_files(StartPos, PieceLength, Records#torrent.info#info.files, 0, []);
		true ->
			Files = [{[Records#torrent.info#info.name], StartPos, PieceLength}]
	end,
	write_to_file(Files, Data, TempPath, Records).

%% Function to write the data to the file
write_to_file(_, <<>>, _, Record) ->
	{ok, Record};
write_to_file([{BinaryPath, StartPos, Length}|T], AllData, TempPath, Records) ->
	{Name, Path} = path_create(BinaryPath, ""),
	<<Data:Length/binary, Rest/binary>> = AllData,
	FilePath = TempPath ++ Path,
	filelib:ensure_dir(FilePath),
	{ok, Index} = file:open(FilePath ++ Name, [read, write, raw]),	
	file:pwrite(Index, StartPos, Data),
	file:close(Index),
	NewRecord = alter_record(Length, BinaryPath, Records),
	write_to_file(T, Rest, TempPath, NewRecord).

%% Function to change the binary data into Strings and create the path for saving the data on the HD
path_create([H|[]], String) ->
	Name = binary_to_list(H),
 	{Name, String};
path_create([H|T], String) ->
	Path = String ++ binary_to_list(H) ++ "/",
	path_create(T, Path).

%% Merging the binary data from the files
%% Files = Records#torrent.info#info.files(
request_data(StartPos, Length, Record) ->
	request_data(StartPos, Length, Record#torrent.info#info.files, Record#torrent.dir).

request_data(StartPos, Length, Files, TorrentPath) ->
	FileMap = calc_files(StartPos, Length, Files, 0, []),
	merge_data(TorrentPath, FileMap, <<>>).

merge_data(_, [], Data) ->
	{ok, Data};
merge_data(TorrentPath, [{BinPath, StartPos, Length}|T], BinaryData) ->
	{Name, Path} = path_create(BinPath, ""),
	{ok, File} = file:open(TorrentPath ++ Path ++ Name, [read, binary]),
	{ok, Data} = file:pread(File, StartPos, Length),
	file:close(File),
	NewData = <<BinaryData, Data>>,
	merge_data(TorrentPath, T, NewData).


%% Updating the downloaded file's length in the db
alter_record(Length, Path, Records) ->
	[Torrent|_] = torrent_db:find_by_SHA1(Records#torrent.info_sha),
	Files = Torrent#torrent.info#info.files,
	Tuple = lists:keyfind(Path, 4, Files),
	{file, FileLength, MD5, Path, LengthComplete} = Tuple,
	NewLength = LengthComplete + Length,
	NewTuple = {file, FileLength, MD5, Path, NewLength},
	NewList = lists:keyreplace(Path, 4, Files, NewTuple),
	Records#torrent{info = (Records#torrent.info)#info{files = NewList}}.
												
%% co-author: Alireza Pazirandeh
%% The function for calculating the positions of of the pieces inside the files
calc_files(_, 0, _, _, Files) ->
		lists:reverse(Files);
%% the startPos < startFilePos and endPos < endFilePos, this is the last piece
calc_files(StartPos, Length, [H|T], StartFilePos, Files)
	when ((StartPos < StartFilePos + H#file.length) and (StartPos >= StartFilePos)) ->
		MaxAllowedLength = StartFilePos + H#file.length - StartPos,
		if 
			(Length > MaxAllowedLength) ->
				NewLength = Length - MaxAllowedLength,
				FileLength = MaxAllowedLength,
				NewStartPos = StartPos + MaxAllowedLength;
			true ->
				NewLength = 0,
				FileLength = Length,
				NewStartPos = StartPos
		end,
		NewFile = {H#file.path, StartPos - StartFilePos, FileLength},
		calc_files(NewStartPos, NewLength, T, StartFilePos + H#file.length, [NewFile|Files]);
calc_files(StartPos, Length, [H|T], StartFilePos, Files) ->
	calc_files(StartPos, Length, T, StartFilePos + H#file.length, Files).