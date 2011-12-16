%%% @author Peter Myllykoski <peter@UL30JT>
%%% @copyright (C) 2011, Peter Myllykoski
%%% @doc
%%%
%%% @end
%%% Created :  3 Nov 2011 by Peter Myllykoski <peter@UL30JT>

-module(url).
-export([encode/1]).

%%----------------------------------------------------------------------
%% Function:
%% Purpose:
%% Args:
%% Returns:
%%----------------------------------------------------------------------

encode(Binary) ->
    {ok, File} = file:open("HexSHA",[append]),
    encode(Binary,File),
    file:close(File),
    {ok, RFile} = file:read_file("HexSHA"),
    RFile.

encode(<<>>,_) ->
    ok;

encode(Binary,File) ->
    <<X:4,Y:4,T/binary>> = Binary,
    file:write(File,"%"),
    io:fwrite(File,"~.16X",[X,""]),
    io:fwrite(File,"~.16X",[Y,""]),
    encode(T,File).




