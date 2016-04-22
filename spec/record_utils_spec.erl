-module(record_utils_spec).

-include_lib("espec/include/espec.hrl").
-include("include/record_utils.hrl").

-record(foo, {}).
-record(bar1, {a}).
-record(bar2, {a, b}).
-record(bar3, {a, b, c = "C"}).


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
                                R2 = ?record_to_proplist(foo, #foo{}),
                                ?assertEqual([], R1),
                                ?assertEqual([], R2)
                        end),
                     % --------------------------------------------------------------------
                     it("should return [{a, A}] given record instance of bar1",
                        fun() ->
                                Fields = record_info(fields, bar1),
                                R1 = record_utils:record_to_proplist(#bar1{a = "A"}, Fields),
                                R2 = ?record_to_proplist(bar1, #bar1{a = "A"}),
                                ?assertEqual([{a, "A"}], R1),
                                ?assertEqual([{a, "A"}], R2)
                        end),

                     % -------------------------------------------------------------------
                     it("should ignore a field given only requre b field in record of bar2",
                       fun() ->
                               Fields = record_info(fields, bar2),
                               R1 = record_utils:record_to_proplist(#bar2{b = "B"}, Fields, [b]),
                               R2 = ?record_to_proplist(bar2, #bar2{b = "B"}, [b]),
                               ?assertEqual([{b, "B"}], R1),
                               ?assertEqual([{b, "B"}], R2)
                       end)

                     
                     
             end),
    describe("Converts proplist to record",
            fun() ->
                    it("should return record foo",
                       fun() ->
                               Fields = record_info(fields, foo),
                               R1 = record_utils:proplist_to_record(#foo{}, [], Fields),
                               R2 = ?proplist_to_record(foo, []),
                               ?assertEqual(#foo{}, R1),
                               ?assertEqual(#foo{}, R2)
                       end),
                    it("should not change default value unless override it",
                      fun() ->
                              Fields = record_info(fields, bar3),
                              R1 = record_utils:proplist_to_record(#bar3{}, [], Fields),
                              ?assertEqual(#bar3{}, R1),
                              R2 = ?proplist_to_record(bar3, [{a, "A1"}, {b, "B1"}]),
                              ?assertEqual(#bar3{a="A1", b="B1"}, R2)
                      end)
                   end).
                        
