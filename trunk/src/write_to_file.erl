%% Author: Eff
%% Created: Nov 14, 2011

-module(write_to_file).
-export([check_sha/7, write_file/1]).


check_sha(Piece, Index, Offset, Name, Shas, Piece_length, PiecesList) ->
	case hashcheck:compare(Shas, Piece) of
		true ->
			Place = (Index * Piece_length) + Offset,
			List = [{Piece, Place, Name} | PiecesList],
			write_file(List);
		_  ->
			error
	end.


write_file([]) ->
	io:format("Empty list");

write_file([{Piece, Place, Name}|T]) ->
	{ok, I} = file:open(Name, [write, read, raw, binary]),
	io:format("Writing piece to the place ~w~n", [Place]),
	if Place >= 0 ->
		  file:pwrite(I, Place, Piece),
	   		write_file(T);
	  Place < 0 ->
		  error
	end.
		

		 
	