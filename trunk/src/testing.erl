-module(testing).

-export([start/0]).

start() ->
	{ok, File} = file:read_file("M3_TestCar.torrent"),
	{ok, Record} = parser:decode(File),
	torrent_db:init(),
	torrent_db:add(Record),
%% 	torrent_db:get_torrent_by_id(0).
	app_sup:start_link().