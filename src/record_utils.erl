%%%-------------------------------------------------------------------
%%% @author Jack Tang <jack@taodinet.com>
%%% @copyright (C) 2012, Jack Tang
%%% @doc
%%%
%%% @end
%%% Created : 28 Dec 2012 by Jack Tang <jack@taodinet.com>
%%%-------------------------------------------------------------------
-module(record_utils).

%% API
-export([record_to_proplist/2,
         record_to_proplist/3]).
-export([proplist_to_record/3]).

-include("record_utils.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%% -define(foo, {a, b})
%% record_to_proplist(Foo, record_info(fields, foo))
%% 
record_to_proplist(Rec, Fields) -> % Rec should be a record
    lists:zip(Fields, tl(tuple_to_list(Rec))).

record_to_proplist(Rec, Fields, RequiredFields)  -> % Rec should be a record
    Values = tl(tuple_to_list(Rec)),
    [Fs, Vs] = lists:foldl(
                 fun(Field, [FAcc0, VAcc0]) ->
                         case lists_utils:index_of(Field, Fields) of
                             not_found -> throw(badarg);
                             Index ->
                                 V = lists:nth(Index, Values),
                                 [ [Field|FAcc0], [V|VAcc0] ]
                         end
                 end, [[], []], RequiredFields),
    lists:zip(Fs, Vs).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
proplist_to_record(Record, Proplist, Fields) ->
    [Tag| Values] = tuple_to_list(Record),
    Defaults = lists:zip(Fields, Values),
    L = lists:map(fun ({K,V}) -> proplists:get_value(K, Proplist, V) end, Defaults),
    list_to_tuple([Tag|L]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
