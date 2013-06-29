%%%-------------------------------------------------------------------
%%% @author Jack Tang <jack@taodinet.com>
%%% @copyright (C) 2012, Jack Tang
%%% @doc
%%%
%%% @end
%%% Created : 28 Dec 2012 by Jack Tang <jack@taodinet.com>
%%%-------------------------------------------------------------------
-module(mnesia_utils).

%% API
-export([create_table/2, create_table/4]).
-export([add_index/2, show_index/1]).
-export([restore/2]).
-export([dump_to_textfile/1,
         dump_to_textfile/2,
         load_textfile/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
%-sepc create_table(atom(), atom()) -> integer().
create_table(Record, Fields) ->
    case mnesia:system_info(is_running) of
        yes ->
            mnesia:create_table(Record, [{disc_copies, [node()]},
                                         {attributes,  Fields %record_info(fields, Record)
                                         }]);
        _ ->
            io:format("Error: mnesia is not running~n", [])
    end.

%-spec create_table(atom(), atom(), []) -> ok.
create_table(Table, Record, IndexList, Fields) ->
    case mnesia:system_info(is_running) of
        yes ->
            mnesia:create_table(Table,  [{disc_copies, [node()]},
                                         {record_name, Record},
                                         {index, IndexList},
                                         {attributes, Fields %record_info(fields, Record)
                                         }]);
        _ ->
            io:format("Error: mnesia is not running~n", [])
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
add_index(Table, IndexList) when is_list(IndexList) ->
    lists:foreach(
      fun(Index) ->
              mnesia:add_table_index(Table, Index)
      end, IndexList).

show_index(Table) ->
    Attrs = mnesia:table_info(Table, attributes),
    IndexPos = mnesia:table_info(Table, index),
    lists:foreach(fun(Pos) ->
                          IndexField = lists:nth(Pos, Attrs),
                          io:format("~p, ", [IndexField])
                  end, IndexPos).



%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
restore(File, Table) when is_atom(Table) ->
    restore(File, [Table]);
restore(File, Tables) when is_list(Tables) ->
    AllTables  = mnesia:system_info(tables),
    
    lists:foreach(
      fun(Tab) ->
              case mnesia:restore(File, [{skip_tables, AllTables -- [Tab] }]) of
                  {atomic, _} ->
                      io:format("table ~p is restored~n", [Tab]);
                  Error ->
                      io:format("table can't be restored because ~p~n", [Error])
              end
      end, Tables).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
dump_to_textfile(File) ->
    dump_to_textfile(mnesia:system_info(is_running), file:open(File, write), all).
dump_to_textfile(File, Tabs) when is_list(Tabs)->
    dump_to_textfile(mnesia:system_info(is_running), file:open(File, write), Tabs).

load_textfile(File) ->
    mnesia:load_textfile(File).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
migrate_table(Record, Table, NewTable) ->
    TFun = fun(_, NData) ->
                   {ok, NData}
           end,
    migrate_table(Record, Table, NewTable, TFun).

migrate_table(Record, Table, NewTable, Transformer) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
dump_to_textfile(yes, {ok, F}, Tabs) ->
    Tabs1 = lists:delete(schema, mnesia:system_info(local_tables)),
    Tabs2 = lists:filter(
             fun(T) ->
                     case mnesia:table_info(T, storage_type) of
                         disc_copies -> true;
                         disc_only_copies -> true;
                         _ -> false
                     end
             end, Tabs1),
    DumpTabs = case Tabs of
                   all -> Tabs2;
                   _ when is_list(Tabs) ->
                       lists:filter(fun(T) ->
                                            lists:member(T, Tabs2)
                                    end, Tabs)
               end,
    io:format("dump tables ~p to file now~n", [DumpTabs]),
    Defs = lists:map(
             fun(T) -> {T, [{record_name, mnesia:table_info(T, record_name)},
                            {attributes, mnesia:table_info(T, attributes)}]} 
             end, DumpTabs),
    io:format(F, "~p.~n", [{tables, Defs}]),
    lists:foreach(fun(T) -> dump_tab(F, T) end, DumpTabs),
    file:close(F);
dump_to_textfile(_, {ok, F}, _Tabs) ->
    file:close(F),
    {error, mnesia_not_running};
dump_to_textfile(_, {error, Reason}, _Tabs) ->
    {error, Reason}.

dump_tab(F, T) ->
    W = mnesia:table_info(T, wild_pattern),
    {atomic,All} = mnesia:transaction(
		     fun() -> mnesia:match_object(T, W, read) end),
    lists:foreach(
      fun(Term) -> io:format(F,"~p.~n", [setelement(1, Term, T)]) end, All).
