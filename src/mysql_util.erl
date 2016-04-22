%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2016, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 22 Apr 2016 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(mysql_util).

%% API
-export([update_statement/3]).

%%%===================================================================
%%% API
%%%===================================================================

update_statement(Tab, Type, Attrs) ->
    SetBlock = generate_attr_block(Attrs),
    Head = 
        case Type of
            insert ->
                "insert into";
            insert_ignore ->
                "insert ignore into";
            replace ->
                "replace into";
            update ->
                "update"
        end,
    list_to_binary(
      lists:flatten([Head, " ", 
                     Tab, " set ", string:join(SetBlock, ",")])).

generate_attr_block(Attrs) ->
    lists:map(
      fun(Key) ->
              io_lib:format("~p = ?", [Key])
      end, Attrs).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
