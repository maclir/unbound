-module(testing).

-export([start/0]).

start() ->
	{ok, File} = file:read_file("M3_TestCar.torrent"),
	{ok, Record} = parser:decode(File),
	torrent_db:init(),
	
	torrent_db:add(Record).