-module(testing).

-export([start/0]).

start() ->
	{ok, File} = file:read_file("ubuntu.torrent"),
	{ok, Record} = parser:decode(File),
	torrent_db:init(),
	torrent_db:add(Record),
	app_sup:start_link().