-module(lists_utils_spec).

-include_lib("espec/include/espec.hrl").

spec() ->
    describe("Index of specified item",
            fun() ->
                    it("should return not_found when the item is not in the list",
                       fun() ->
                               R = lists_utils:index_of(x, [a, b, c]),
                               ?assertEqual(not_found, R)
                       end),

                    it("should return 1 when the list have only one exactly one item",
                       fun() ->
                               R = lists_utils:index_of(a, [a]),
                               ?assertEqual(1, R)
                       end)
            end).
