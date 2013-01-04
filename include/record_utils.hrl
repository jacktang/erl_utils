-define(R2P(Record, Rec),
        ((fun(#Record{} = Rec) ->
                  lists:zip(record_info(fields, Record), tl(tuple_to_list(Rec)))
          end)(Rec))).

-define(R2P(Record, Rec, RequiredFields),
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
