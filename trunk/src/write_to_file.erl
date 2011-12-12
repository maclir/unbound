%% Author: Evelina Vorobyeva
%% Created: Nov 14, 2011

-module(write_to_file).
-export([write/4]).
-include("torrent_db_records.hrl").

write(PieceId, Data, Records, Done) ->
	case (check_sha(Records#torrent.info#info.pieces, PieceId, Data)) of
		true ->
			%%TODO get the TempFolder from settings db
			{ok, Dir} = file:get_cwd(),
			TempFolder = Dir ++ "/Unbound_Test/" ++ info_hash:to_hex(Records#torrent.info_sha) ++ "/" ,
			DestFolder = Dir ++ "/Unbound_Dest/",
			
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


%% Validating the piece's SHA1 
check_sha(Shas, PieceId, Data) ->
	hashcheck:compare(shas_split(Shas, PieceId), Data).

%% Split sha1 String into 20-bit piece for exact piece of Data
shas_split(Shas, Index) ->
	Start = (20 * Index),
	<<_ShaStart:Start/binary, NeededSha:20/binary, _Rest/binary>> = Shas,
	NeededSha.

%% Move the downloaded data from temporary folder into destination folder
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