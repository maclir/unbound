%%% @author  Nahid Vafaie

%%% Created :  10 Nov 2011 by Nahid Vafaie


-module(hashcheck).

-export([calculate_hash/1, compare/2,is_equal/2]).

%%----------------------------------------------------------------------
%% Function:	calculate_hash/1
%% Purpose:		calcualtes the sha1 for the binary file
%% Args:		File
%% Returns:		computation of sha for the given file.
%%----------------------------------------------------------------------

calculate_hash(File) ->
     crypto:sha(File).


%%----------------------------------------------------------------------
%% Function:	compare/2
%% Purpose:     first calcualtes the sha1 for the received data and then send it
%%              together with the sha1 stored in the file for compare.
%% Args:		Sha1, ReceivedData
%% Returns:     sends data to is_equal function for comparing.
%%----------------------------------------------------------------------

compare(Sha1 , ReceivedData) ->
    HashFile = calculate_hash(ReceivedData),
    is_equal(Sha1 , HashFile).


%%----------------------------------------------------------------------
%% Function:	is_equal/2
%% Purpose:		comparing the sha1 files
%% Args:		sha1,sha1
%% Returns:     checks if it is the same or not( true, false)
%%----------------------------------------------------------------------

is_equal(Sha1 , Sha1) ->
    true;
is_equal(_,_) ->
    false.










