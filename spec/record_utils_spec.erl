-module(record_utils_spec).

-include_lib("espec/include/espec.hrl").
-record(foo, {}).
-record(bar1, {a}).
-record(bar2, {a, b}).

spec() ->
    describe("Converts record to proplist",
             fun() ->
                     before_each(
                       fun() ->
                               ok
                       end),
                     
                     it("should return [] given one record without field",
                        fun() ->
                                Fields = record_info(fields, foo),
                                R1 = record_utils:record_to_proplist(#foo{}, Fields),
                                ?assertEqual([], R1)
                        end),
                     % --------------------------------------------------------------------
                     it("should return [{a, A}] given record instance of bar1",
                        fun() ->
                                Fields = record_info(fields, bar1),
                                R1 = record_utils:record_to_proplist(#bar1{a = "A"}, Fields),
                                ?assertEqual([{a, "A"}], R1)
                        end),

                     % -------------------------------------------------------------------
                     it("should ignore a field given only requre b field in record of bar2",
                       fun() ->
                               Fields = record_info(fields, bar2),
                               R = record_utils:record_to_proplist(#bar2{b = "B"}, Fields, [b]),
                               ?assertEqual([{b, "B"}], R)
                       end)
                     
             end).
