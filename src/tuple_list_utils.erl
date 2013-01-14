%%%-------------------------------------------------------------------
%%% @author Jack Tang <jack@taodinet.com>
%%% @copyright (C) 2013, Jack Tang
%%% @doc
%%%
%%% @end
%%% Created : 14 Jan 2013 by Jack Tang <jack@taodinet.com>
%%%-------------------------------------------------------------------
-module(tuple_list_utils).

%% API
-export([atomize_key/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
atomize_key(Tuplelist) ->
    atomize_key(Tuplelist, []).
    
%%%===================================================================
%%% Internal functions
%%%===================================================================
atomize_key([], Return) ->
    Return;
atomize_key([{Key, Value}|Tail], Return) ->
    case type_utils:any_to_atom(Key) of
        R when is_atom(R) ->
            atomize_key(Tail, [{R, Value}|Return]);
        _ ->
            atomize_key(Tail, Return)
    end;
atomize_key([{Key, Operation, Value}|Tail], Return) ->
    case type_utils:any_to_atom(Key) of
        R when is_atom(R) ->
            atomize_key(Tail, [{R, Operation, Value} | Return]);
        _ ->
            atomize_key(Tail, Return)
    end.
