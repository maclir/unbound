-module(utils).
-export([kbytes_to_mbytes/1, bytes_to_kbytes/1, bytes_to_mbytes/1]).

%%----------------------------------------------------------------------
%% Function:	        kbytes_to_mbytes/1
%% Purpose:		Converts kilobytes to megabytes.
%% Args:		KBs (integer or float)
%% Returns:	        float
%%----------------------------------------------------------------------
kbytes_to_mbytes(KBs)->
    KBs / 1024.
%%----------------------------------------------------------------------
%% Function:     	bytes_to_kbytes/1
%% Purpose:		Converts bytes to kilobytes.
%% Args:		Bs (integer or float)
%% Returns:		float
%%----------------------------------------------------------------------
bytes_to_kbytes(Bs) ->
    Bs / 1024.
%%----------------------------------------------------------------------
%% Function:	        bytes_to_mbytes/1
%% Purpose:		Converts bytes to mbytes.
%% Args:		Bs (integer or float)
%% Returns:		float
%%----------------------------------------------------------------------
bytes_to_mbytes(Bs) ->
    kbytes_to_mbytes(Bs / 1024).
