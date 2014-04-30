%%%-------------------------------------------------------------------
%%% @author Jack Tang <jack@taodinet.com>
%%% @copyright (C) 2012, Jack Tang
%%% @doc
%%%
%%% @end
%%% Created : 28 Dec 2012 by Jack Tang <jack@taodinet.com>
%%%-------------------------------------------------------------------
-module(lists_utils).

%% API
-export([index_of/2]).
-export([uniq/1]).
-export([droplast/1, last/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%% Erlang index is 1-based
index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
uniq(undefined) -> undefined;
uniq([]) -> [];
uniq(List) ->
    Set = sets:from_list(List),
    sets:to_list(Set).


%% This is the simple recursive implementation
%% reverse(tl(reverse(L))) is faster on average,
%% but creates more garbage.
droplast([_T])  -> [];
droplast([H|T]) -> [H|droplast(T)].

%% last(List) returns the last element in a list.
last([E|Es]) -> last(E, Es).

last(_, [E|Es]) -> last(E, Es);
last(E, []) -> E.

%%%===================================================================
%%% Internal functions
%%%===================================================================
