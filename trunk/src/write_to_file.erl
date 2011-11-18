%% Author: Evelina Vorobyeva
%% Created: Nov 14, 2011

-module(write_to_file).
-export([check_sha/7, write_file/2, shas_split/2, read_name/2]).

%% Validating the piece's SHA1 

check_sha(Piece, Index, Offset, {_,_,[],NewName}, Shas, Piece_length, PiecesList) ->
	case hashcheck:compare(shas_split(Shas, Index), Piece) of
		true ->						 
			String = "Desktop",
			{JustName, Dir} = read_name(NewName, String),
			io:format("Dir: ~s~n", [Dir]),
			Place = (Index * Piece_length) + Offset,
			List = [{Piece, Place, JustName} | PiecesList],		
			write_file(List, Dir);
		_  ->
			error
	end.

%% Split sha1 String into 20-bit piece for exact piece of Data

shas_split(Shas, Index) ->
		Start = (20 * Index) + 1,
		string:substr(Shas, Start, 20).
		
%% Write data in the file on the exact place

write_file([{Piece, Place, Name}|T], Dir) ->
	{ok, D} = file:get_cwd(),
	file:set_cwd(D ++ "/" ++ Dir),
	io:format("~s~n", [D ]),
	{ok, I} = file:open(Name, [write, read]),
	io:format("Writing piece to the place ~w~n", [Place]),
	if Place >= 0 ->
		  	file:pwrite(I, Place, Piece),
	   		write_file(T, Dir),
		  	file:set_cwd(D),
	   		file:close(I);
			
	    Place < 0 ->
		  error
	end;


write_file([], _) ->
	io:format("Empty list").


%% Read name/path and create all the necessary folders

read_name([H|[]], String) ->
	io:format("~s~n", [H]),
	filelib:ensure_dir(String ++ "/"),
	{H, String};

	
read_name([H|T], String) ->
	read_name(T, (String ++ "/" ++ (binary_to_list(H)))).

	



	

	

		 
	