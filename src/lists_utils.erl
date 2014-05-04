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
-export([split/2, split/3]).

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


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
split(N, List) ->
    do_split(N, List, [], undefined).

split(N, List, Fun) ->
    do_split(N, List, [], Fun).

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_split(N, List, Acc, Fun) when length(List) < N ->
    R = case erlang:is_function(Fun, 1) of
            true  -> Fun(List);
            false -> List
        end,
    lists:append(Acc, [R]);
do_split(N, List, Acc, Fun) ->
    {List2, List3} = lists:split(N, List),
    R = case erlang:is_function(Fun, 1) of
            true  -> Fun(List2);
            false -> List2
        end,
    do_split(N, List3, lists:append(Acc, [R]), Fun).
