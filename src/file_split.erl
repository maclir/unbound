%%%----------------------------------------------------------------------
%%% Author:		Evelina Vorobyeva
%%% Desc.:		Calculating the data's position inside the file for 
%%%				both uploading and downloading
%%%----------------------------------------------------------------------

-module(file_split).
-export([start/5, path_create/2, request_data/4]).
-include("torrent_db_records.hrl").


%%----------------------------------------------------------------------
%% Function:	start/5
%% Purpose:		starts the calculation process
%% Args:		Data(binary), StartPos, PieceLength(integers),
%%				TempPath (String), Records (list of records)
%%----------------------------------------------------------------------	
start(Data, StartPos, PieceLength, TempPath, Records) ->
	if
		(is_list(Records#torrent.info#info.files) and (Records#torrent.info#info.files /= []) and is_record(hd(Records#torrent.info#info.files), file)) ->
			Files = calc_files(StartPos, PieceLength, Records#torrent.info#info.files, 0, []);
		true ->
			Files = [{[Records#torrent.info#info.name], StartPos, PieceLength}]
	end,
	
	write_to_file(Files, Data, TempPath, Records).


%%----------------------------------------------------------------------
%% Function:	write_to_file/4
%% Purpose:		Write the data to the file
%% Returns:		ok and changed DB record with the changed LengthComplete entry
%% Args:		BinaryPath(binary), StartPos(integer), Length(integer)
%%				AllData(binary), TempPath(String), Records(List of records)
%%----------------------------------------------------------------------	
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
	if
		(is_list(Records#torrent.info#info.files) and (Records#torrent.info#info.files /= []) and is_record(hd(Records#torrent.info#info.files), file)) ->
			NewRecord = alter_record(Length, BinaryPath, Records);
		true ->
			NewRecord = Records
	end,
	write_to_file(T, Rest, TempPath, NewRecord).

%%----------------------------------------------------------------------
%% Function:	path_create/2
%% Purpose:		Creates the path string for saving data on HD from the binary data
%% Returns:		Name of the file and a path string
%% Args:		BinaryPath (binary), empty String
%%----------------------------------------------------------------------
path_create([H|[]], String) ->
	Name = binary_to_list(H),
 	{Name, String};
path_create([H|T], String) ->
	Path = String ++ binary_to_list(H) ++ "/",
	path_create(T, Path);
path_create(H, String) ->
	Name = binary_to_list(H),
 	{Name, String}.

%%----------------------------------------------------------------------
%% Function:	request_data/4
%% Purpose:		Calculates the start position of the data in the file (for uploading) 
%% Args:		PieceIndex(integer), Offset(integer), Length(integer)
%%				Record(record)
%%----------------------------------------------------------------------
request_data(PieceIndex, Offset, Length, Record) ->
	StartPos = PieceIndex * Record#torrent.info#info.piece_length + Offset,
	case Record#torrent.info#info.length - Record#torrent.info#info.length_complete of
		0 ->
			TorrentPath = Record#torrent.dir;
		_ ->
			{ok, Dir} = file:get_cwd(),
			TorrentPath = Dir ++ "/Unbound_Temp/" ++ info_hash:to_hex(Record#torrent.info_sha) ++ "/"
	end,
	request_data_start(StartPos, Length, Record, TorrentPath).

%%----------------------------------------------------------------------
%% Function:	request_data_start/4
%% Purpose:		Merges the binary data from the files (for uploading)
%%				Files = Records#torrent.info#info.files
%% Args:		StartPos(integer), Length(integer)
%%				Records(record), TorrentPath(String)
%%----------------------------------------------------------------------
request_data_start(StartPos, Length, Record, TorrentPath) ->
	if
		(is_list(Record#torrent.info#info.files) and (Record#torrent.info#info.files /= []) and is_record(hd(Record#torrent.info#info.files), file)) ->
			FileMap = calc_files(StartPos, Length, Record#torrent.info#info.files, 0, []);
		true ->
			FileMap = [{[Record#torrent.info#info.name], StartPos, Length}]
	end,
	merge_data(TorrentPath, FileMap, <<>>).

%%----------------------------------------------------------------------
%% Function:	merge_data/3
%% Purpose:		Reading the requested binary data from the files 
%% Returns:		Binary Data needed to be uploaded			
%% Args:		TorrentPath(String), FileMap(List), BinaryData(binary)
%%----------------------------------------------------------------------
merge_data(_, [], Data) ->
	{ok, Data};
merge_data(TorrentPath, [{BinPath, StartPos, Length}|T], BinaryData) ->
	{Name, Path} = path_create(BinPath, ""),
	{ok, File} = file:open(TorrentPath ++ Path ++ Name, [read, binary]),
	{ok, Data} = file:pread(File, StartPos, Length),
	file:close(File),
	NewData = <<BinaryData/binary, Data/binary>>,
	merge_data(TorrentPath, T, NewData).

%%----------------------------------------------------------------------
%% Function:	alter_record/3
%% Purpose:		Changing the downloaded file's length in the DB
%% Returns:		Changed DB record
%% Args:		Length(integer), Path(String), Record(record)
%%----------------------------------------------------------------------
%% Updating the downloaded file's length in the db
alter_record(Length, Path, Record) ->
	Files = Record#torrent.info#info.files,
	Tuple = lists:keyfind(Path, 4, Files),
	{file, FileLength, MD5, Path, LengthComplete} = Tuple,
	NewLength = LengthComplete + Length,
	NewTuple = {file, FileLength, MD5, Path, NewLength},
	NewList = lists:keyreplace(Path, 4, Files, NewTuple),
	NewRecord = Record#torrent{info = (Record#torrent.info)#info{files = NewList}},
	NewRecord.
												
%%----------------------------------------------------------------------
%% co-author: 	Alireza Pazirandeh
%% Function:	calc_files/5
%% Purpose:		Calculating the positions of the piece inside the file
%% Returns:		A record with the Path string, start position and the data's length
%% Args:		StartPos(integer), Length(integer), Record(List of records), 
%%				StartFilePos(Integer), Files(List)
%%----------------------------------------------------------------------
calc_files(_, 0, _, _, Files) ->
		lists:reverse(Files);
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