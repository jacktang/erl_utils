%%%-------------------------------------------------------------------
%%% @author Jack Tang <jack@taodinet.com>
%%% @copyright (C) 2013, Jack Tang
%%% @doc
%%%
%%% @end
%%% Created : 27 Feb 2013 by Jack Tang <jack@taodinet.com>
%%%-------------------------------------------------------------------
-module(json_utils).

%% API
-export([to_record/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
get_value(JSONStr, Key) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
to_record({obj, Values}, Fallback, Fields) ->  % rfc4627
    list_to_tuple([element(1, Fallback) | decode_record_fields(Values, Fallback, 2, Fields)]);

to_record({struct, Values}, Fallback, Fields) -> % mochijson2
    list_to_tuple([element(1, Fallback) | decode_record_fields(Values, Fallback, 2, Fields)]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
decode_record_fields(_Values, _Fallback, _Index, []) ->
    [];
decode_record_fields(Values, Fallback, Index, [Field | Rest]) ->
    [case lists:keysearch(atom_to_list(Field), 1, Values) of
	 {value, {_, Value}} ->
	     Value;
	 false ->
	     element(Index, Fallback)
     end | decode_record_fields(Values, Fallback, Index + 1, Rest)].
