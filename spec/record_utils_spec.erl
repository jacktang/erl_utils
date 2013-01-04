-module(record_utils_spec).

-include_lib("espec/include/espec.hrl").
-include("include/record_utils.hrl").

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
                                R2 = ?R2P(foo, #foo{}),
                                ?assertEqual([], R1),
                                ?assertEqual([], R2)
                        end),
                     % --------------------------------------------------------------------
                     it("should return [{a, A}] given record instance of bar1",
                        fun() ->
                                Fields = record_info(fields, bar1),
                                R1 = record_utils:record_to_proplist(#bar1{a = "A"}, Fields),
                                R2 = ?R2P(bar1, #bar1{a = "A"}),
                                ?assertEqual([{a, "A"}], R1),
                                ?assertEqual([{a, "A"}], R2)
                        end),

                     % -------------------------------------------------------------------
                     it("should ignore a field given only requre b field in record of bar2",
                       fun() ->
                               Fields = record_info(fields, bar2),
                               R1 = record_utils:record_to_proplist(#bar2{b = "B"}, Fields, [b]),
                               R2 = ?R2P(bar2, #bar2{b = "B"}, [b]),
                               ?assertEqual([{b, "B"}], R1),
                               ?assertEqual([{b, "B"}], R2)
                       end)

                     
                     
             end).
