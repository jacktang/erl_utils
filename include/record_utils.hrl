-define(tuple_to_list(RecTag),
        ((fun() -> tuple_to_list(#RecTag{}))())).

-define(record_to_proplist(Record, Rec),
        ((fun(#Record{} = Rec) ->
                  lists:zip(record_info(fields, Record), tl(tuple_to_list(Rec)))
          end)(Rec))).

-define(record_to_proplist(Record, Rec, RequiredFields),
        ((fun(#Record{} = Rec) ->
                  Fields = record_info(fields, Record),
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
                  lists:zip(Fs, Vs)
          end)(Rec))).

-define(proplist_to_record(Record, Proplist),
        ((fun(Proplist) ->
                Fields = record_info(fields, Record),
                [Tag| Values] = tuple_to_list(#Record{}),
                Defaults = lists:zip(Fields, Values),
                L = lists:map(fun ({K,V}) -> proplists:get_value(K, Proplist, V) end, Defaults),
                list_to_tuple([Tag|L])
        end
       )(Proplist))).

-define(PLTR(Record, Proplist),
        record_utils:proplists_to_record(Proplist, record_info(fields, Record), #Record{})
       ).

-define(PLTR(Record, Proplist, Fun),
        record_utils:proplists_to_record(Proplist, Fun, record_info(fields, Record), #Record{})
       ).


-define(RTPL(RName, Record),
        record_utils:record_to_proplists(Record, record_info(fields, RName))
       ).

-define(RTPL(RName, Record, Fun),
        record_utils:record_to_proplists(Record, Fun, record_info(fields, RName))
       ).
