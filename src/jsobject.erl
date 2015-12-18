%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2015, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 17 Dec 2015 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(jsobject).

%% API
-export([parse/1]).

-include_lib("parsec/include/parsec.hrl").

%%%===================================================================
%%% API
%%%===================================================================

parse(String) when is_binary(String) ->
    parse(binary_to_list(String));
parse(String) when is_list(String) ->
    parsec:parse(String, jsobject()).

jsobject() ->
    parsec:choice([array(), map(), quoted_string(), float(), integer()]).

integer() ->
    parsec:bind(?DIGITS,
                fun (N) -> 
                        parsec:return(list_to_integer(N))
                end).

float() ->
    Fraction=parsec:do([parsec:char($.),?DIGITS]),
    parsec:bind(parsec:capture(parsec:do([?DIGITS,
                                          Fraction
						      ])),
			     fun (N) -> 
				     parsec:return(list_to_float(N))
			     end).

array() ->
    parsec:bind(id(),
                fun(_Item) ->
                        parsec:bind(array_(),
                                    fun(Result) ->
                                            parsec:return(Result)
                                    end)
                end).
array_() ->
    parsec:between(escaped_char($[), escaped_char($]), 
                   parsec:sep_by(jsobject(), escaped_char($,))).
    
map() ->
    parsec:bind(parsec:between(escaped_char(${), escaped_char($}),
                               parsec:sep_by(key_value_pair(), escaped_char($,))),
                fun(Pairs) ->
                        parsec:return(lists:foldl(
                                        fun({Key, Value}, Acc) ->
                                                maps:put(Key, Value, Acc)
                                        end, maps:new(), Pairs))
                end).

key_value_pair() ->
    parsec:bind(object_key(),
                fun(Key) ->
                        parsec:bind(parsec:do([escaped_char($:),
                                               jsobject()]),
                                    fun(Value) ->
                                            parsec:return({Key, Value})
                                    end)
                end).

object_key() ->
    parsec:choice([parsec:many1(letters()), quoted_string()]).

quoted_string() ->
    parsec:choice([parsec:between(parsec:char($"), parsec:char($"),
                                  parsec:many(quoted_letters())),
                   parsec:between(parsec:char($'), parsec:char($'),
                                  parsec:many(quoted_letters()))]).

quoted_letters() ->
    parsec:choice([letters(), parsec:one_of("{}[].*: ")]).

letters() ->
    parsec:choice([?LETTER, parsec:one_of("-_"), ?DIGIT]).

escaped_char(Char) ->
    escaped_char(Char, " \t\r\n").

escaped_char(Char, Escape) ->
    parsec:between(parsec:many(parsec:one_of(Escape)),
                   parsec:many(parsec:one_of(Escape)),
                   parsec:char(Char)).
id() ->
    fun(X) ->
             X
    end.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
