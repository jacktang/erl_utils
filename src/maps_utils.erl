%%%-------------------------------------------------------------------
%%% @author Jack Tang <jack@taodinet.com>
%%% @copyright (C) 2014, Jack Tang
%%% @doc
%%%
%%% @end
%%% Created : 15 Dec 2014 by Jack Tang <jack@taodinet.com>
%%%-------------------------------------------------------------------
-module(maps_utils).

%% API
-export([append/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
append(Key, Val, Maps) -> 
    case maps:get(Key, Maps, undefined) of
        undefined ->
            maps:put(Key, Val, Maps);
        LsVal when is_list(LsVal) ->
            maps:put(Key, lists:append(LsVal, Val), Maps);
        Val ->
            io:format("Existed value is not list type"),
            Maps
    end.
%%%===================================================================
%%% Internal functions
%%%===================================================================
