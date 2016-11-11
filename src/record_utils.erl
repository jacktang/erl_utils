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
-export([proplists_to_record/3, proplists_to_record/4]).
-export([record_to_proplists/2, record_to_proplists/3]).

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
                 fun(F, [FAcc0, VAcc0]) ->
                         {Field, Target, Formatter} = formatter(F),
                         case lists_utils:index_of(Field, Fields) of
                             not_found -> 
                                 throw({invalid_field, Field});
                             Index ->
                                 V = lists:nth(Index, Values),
                                 NV = format(Formatter, V, Rec),
                                 [ [Target|FAcc0], [NV|VAcc0] ]
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
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
formatter(Field) when is_atom(Field) ->
    {Field, Field, fun identity/1};
formatter({Field, Target}) ->
    {Field, Target, fun identity/1};
formatter({Field, Target, Formatter}) when is_function(Formatter) ->
    {Field, Target, Formatter}.

identity(Value) ->
    Value.

proplists_to_record(Proplists, Fields, Default) ->
    proplists_to_record(Proplists, undefined, Fields, Default).
proplists_to_record(Proplists, Fun, Fields, Default) ->
    [Tag| Values] = tuple_to_list(Default),
    DefaultMap = maps:from_list(lists:zip(Fields, Values)),
    RecMap =
        lists:foldl(
          fun ({K,V}, Acc) ->
                  NK = to_atom(K),
                  key_value_converter(NK, V, Acc, Fun, Proplists)
          end, maps:new(), Proplists),
    L = 
        lists:map(
          fun(Field) ->
                  case maps:find(Field, RecMap) of
                      {ok, V} ->
                          V;
                      error ->
                          maps:get(Field, DefaultMap, undefined)
                  end
          end, Fields),

    list_to_tuple([Tag|L]).

record_to_proplists(Record, Fields) ->
    record_to_proplists(Record, undefined, Fields).

record_to_proplists(Record, Fun, Fields) ->
    Values = tl(tuple_to_list(Record)),
    Pairs = lists:zip(Fields, Values),
    lists:reverse(
      lists:foldl(
        fun({K, V}, Acc) ->
                key_value_converter(K, V, Acc, Fun, Record)
        end, [], Pairs)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
format(Formatter, Value, _Record) when is_function(Formatter, 1) ->
    Formatter(Value);
format(Formatter, Value, Record) when is_function(Formatter, 2) ->
    Formatter(Value, Record);
format(Formatter, Value, Record) ->
    throw({invalid_formatter, Formatter, Value, Record}).

key_value_converter(K, V, Acc, Fun, Record) when is_list(K) ->
    NK = list_to_atom(K),
    key_value_converter(NK, V, Acc, Fun, Record);
key_value_converter(K, V, Acc, Fun, Record) when is_binary(K) ->
    NK = binary_to_atom(K, utf8),
    key_value_converter(NK, V, Acc, Fun, Record);
key_value_converter(K, V, Acc, Fun, Record) when is_atom(K) ->
    case Fun of
        undefined ->
            append_value({K, V}, Acc);
        Fun when is_function(Fun, 1)  ->
            case Fun(K) of
                undefined ->
                    Acc;
                NK ->
                    append_value({NK, V}, Acc)
            end;
        Fun when is_function(Fun, 2) ->
            Pairs = Fun(K, V),
            append_values(Pairs, Acc);
        Fun when is_function(Fun, 3) ->
            Pairs =  Fun(K, V, Record),
            append_values(Pairs, Acc);
        ConveterList when is_list(ConveterList) ->
            case proplists:get_value(K, ConveterList) of
                undefined ->
                    Acc;
                ignore__ ->
                    Acc;
                true ->
                    append_value({K, V}, Acc);
                NK when is_atom(NK) ->
                    append_value({NK, V}, Acc);
                Formatter when is_function(Formatter) ->
                    NV = format(Formatter, V, Record),
                    append_value({K, NV}, Acc);
                {NK, Formatter} when is_atom(NK), is_function(Formatter) ->
                    NV = format(Formatter, V, Record),
                    append_value({NK, NV}, Acc)
            end;
        Other ->
            throw({invalid_converter, Other})
    end;
key_value_converter(K, _V, _Acc, _Fun, _Record) ->
    throw({invalid_key, K}).

to_atom(K) when is_atom(K) ->
    K;
to_atom(K) when is_list(K) ->
    list_to_atom(K);
to_atom(K) when is_binary(K) ->
    binary_to_atom(K, utf8).

append_values([KV|T], Acc) ->
    NAcc = append_value(KV, Acc),
    append_values(T, NAcc);
append_values([], Acc) ->
    Acc;
append_values(KV, Acc) ->
    append_value(KV, Acc).

append_value(Key, Acc) when is_atom(Key), is_list(Acc) ->
    [Key|Acc];
append_value({Key, Value}, Acc) when is_map(Acc) ->
    maps:put(Key, Value, Acc);
append_value({Key, Value}, Acc) when is_list(Acc) ->
    [{Key, Value}|Acc].










