-module(testing).

-export([start/0]).

start() ->
	{ok, File} = file:read_file("M3_TestCar.torrent"),
	spawn(com_central,start_link,[]),
	com_central:add_new_torrent_file(File).