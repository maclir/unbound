-module(testing).

-export([start/0]).

start() ->
	app_sup:start_link().
%%	com_central:add_new_torrent_url("http://46.239.111.192:8080/torrents/Pppm.torrent", ""),
%% 	com_central:add_new_torrent_url("http://46.239.111.192:8080/torrents/Gentoo.torrent", "").
%% 	com_central:add_new_torrent_url("http://46.239.111.192:8080/torrents/Ubuntu.torrent", "").
%% 	com_central:add_new_torrent_url("http://46.239.111.192:8080/torrents/oooo.torrent", "").
%% 	com_central:add_new_torrent_url("http://46.239.111.192:8080/torrents/M3_TestCar.torrent", "").
%% 	com_central:add_new_torrent_url("http://46.239.111.192:8080/torrents/LetterFiles.torrent", "").
%% 	com_central:add_new_torrent_url("http://46.239.111.192:8080/torrents/Pic3.torrent", "").
%% 	com_central:add_new_torrent_url("http://46.239.111.192:8080/torrents/New.torrent", "").
%%    web_tcp:start().
