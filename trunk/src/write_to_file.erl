%%%----------------------------------------------------------------------
%%% Author:		Evelina Vorobyeva
%%% Desc.:		Creating the files and folders and writing the downloaded
%%%				data there
%%%----------------------------------------------------------------------
-module(write_to_file).
-export([write/4]).
-include("torrent_db_records.hrl").

%%----------------------------------------------------------------------
%% Function:	write/4
%% Purpose:		writing the data in the write position in file
%%				in the temporary folder
%% Args:		PieceId(Integer), Data(binary), Records(record),
%%				Done(boolean)
%%----------------------------------------------------------------------	
write(PieceId, Data, Records, Done) ->
	case (check_sha(Records#torrent.info#info.pieces, PieceId, Data)) of
		true ->
			%%TODO get the TempFolder from settings db
			{ok, Dir} = file:get_cwd(),
			TempFolder = Dir ++ "/Unbound_Temp/" ++ info_hash:to_hex(Records#torrent.info_sha) ++ "/" ,
			DestFolder = Records#torrent.dir,
			
			StartPos = (PieceId * Records#torrent.info#info.piece_length),
			PieceLength = byte_size(Data),		
			Result = file_split:start(Data, StartPos, PieceLength, TempFolder, Records),
			if 
				Done == true ->
					if
						(is_list(Records#torrent.info#info.files) and (Records#torrent.info#info.files /= []) and is_record(hd(Records#torrent.info#info.files), file)) ->
							Files = Records#torrent.info#info.files;
						true ->
							Files = [#file{path = Records#torrent.info#info.name}]
					end,
					move_to_folder(Files, TempFolder, DestFolder),
					Result;
				Done == false ->	
					Result
			end;
		_ ->
			{error, sha_did_not_match}
	end.

%%----------------------------------------------------------------------
%% Function:	check_sha/3
%% Purpose:		Validating the piece's sha1 
%% Args:		Shas(String), PieceId(Integer), Data(Binary)
%% Returns:		true, if the validation succesful, otherwise false
%%----------------------------------------------------------------------
check_sha(Shas, PieceId, Data) ->
	hashcheck:compare(shas_split(Shas, PieceId), Data).

%%----------------------------------------------------------------------
%% Function:	shas_split/2
%% Purpose:		Spliting Sha1 String into 20-bit piece for exact piece of Data 
%% Args:		Shas(String), Index(Integer)
%% Returns:		Sha1 20-bit binary
%%----------------------------------------------------------------------
%% Split sha1 String into 20-bit piece for exact piece of Data
shas_split(Shas, Index) ->
	Start = (20 * Index),
	<<_ShaStart:Start/binary, NeededSha:20/binary, _Rest/binary>> = Shas,
	NeededSha.

%%----------------------------------------------------------------------
%% Function:	move_to_folder/3
%% Purpose:		Moving the downloaded data from temporary folder into 
%%				destination folder 
%% Args:		Shas(String), PieceId(Integer), Data(Binary)
%% Returns:		true, if the validation succesful, otherwise false
%%----------------------------------------------------------------------

move_to_folder([], TempFolder, _) ->
	delete_files(["/"], TempFolder),
	{ok, done};
move_to_folder([H|T], TempFolder, DestFolder) ->
	{Name, FilePath}  = file_split:path_create(H#file.path, ""), 
	TempFilePath = TempFolder ++ FilePath ++ Name,
	DestFilePath = DestFolder ++ FilePath ++ Name,
	filelib:ensure_dir(DestFolder ++ FilePath),
	file:rename(TempFilePath, DestFilePath),
	move_to_folder(T, TempFolder, DestFolder).

%%----------------------------------------------------------------------
%% Function:	delete_files/2
%% Purpose:		Deleting the useless temporary files and folders 
%% Args:		File(List), Path(String)
%% Returns:		{ok, done}
%%----------------------------------------------------------------------

delete_files([], _) ->
	{ok, done};
delete_files([File|T],Path) ->
	case file:delete(Path ++ File) of
		ok ->
			delete_files(T, Path);
		{error, _Reason} ->
			{ok, Files} = file:list_dir(Path ++ File),
			delete_files(Files, Path ++ File ++ "/"),
			file:del_dir(Path ++ File),
			delete_files(T, Path)
	end.