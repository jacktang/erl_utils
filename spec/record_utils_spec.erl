-module(record_utils_spec).

-include_lib("espec/include/espec.hrl").
-record(foo, {}).
%-record(bar, {a, b}).

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
                                io:format("--> ~p~n", [Fields]),
                                R1 = record_utils:record_to_proplist(#foo{}, Fields),
                                io:format("--> ~p~n", [R1]),
                                ?assertEqual([], R1)
                        end)
             end).
