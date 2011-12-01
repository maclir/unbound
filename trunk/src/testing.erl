-module(testing).

-export([start/0]).

start() ->
%% 	{ok, File} = file:read_file("../torrents/Gentoo.torrent"),
	{ok, File} = file:read_file("../torrents/Ubuntu.torrent"),
%% 	{ok, File} = file:read_file("../torrents/M3_TestCar.torrent"),
%% 	{ok, File} = file:read_file("../torrents/LetterFiles.torrent"),
	{ok, Record} = parser:decode(File),
	torrent_db:init(),
	torrent_db:add(Record),
%% 	torrent_db:get_torrent_by_id(0).
	app_sup:start_link().
