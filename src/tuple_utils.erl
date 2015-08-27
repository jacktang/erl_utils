%%%-------------------------------------------------------------------
%%% @author Jack Tang <jack@taodi.local>
%%% @copyright (C) 2014, Jack Tang
%%% @doc
%%%
%%% @end
%%% Created : 26 Nov 2014 by Jack Tang <jack@taodi.local>
%%%-------------------------------------------------------------------
-module(tuple_utils).

%% API
-export([value/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
value(Field, Record, Fields) when is_tuple(Record) ->
    [_RecAtom | RecVals] = tuple_to_list(Record),
    RecWithFields = lists:zip(Fields, RecVals),
    proplists:get_value(Field, RecWithFields, undefined);
value(_Field, _Record, _Fields) ->
    undefined.

%%%===================================================================
%%% Internal functions
%%%===================================================================
