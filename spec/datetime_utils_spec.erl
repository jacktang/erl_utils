-module(datetime_utils_spec).

-include_lib("espec/include/espec.hrl").


spec() ->
    describe("Utilities for datetime",
            fun() ->
                    it("should be equal in converting epoch to now and now to epoch",
                       fun() ->
                               Now = erlang:now(),
                               E = datetime_utils:now_to_seconds(Now),
                               N = datetime_utils:epoch_to_now(E),
                               {NMega, NSec, _} = Now,
                               ?assertEqual({NMega, NSec, 0}, N)
                               %LT = datetime_utils:epoch_to_localtime(E),
                               %io:format("==> ~p~n", [LT])
                       end),

                    it("should be equal in converting date to string and string to date",
                      fun() ->
                              DI1 = {2012, 10, 5},
                              DI2 = {{2012, 10, 5}, {2, 3, 50}},
                              S1 = datetime_utils:datetime_as_string(DI1),
                              S2 = datetime_utils:datetime_as_string(DI2),
                              D2 = datetime_utils:parse_date(S1, 'yyyy-MM-dd'),
                              ?assertEqual(DI1, D2),
                              ?assertEqual("2012-10-05 02:03:50", S2)
                      end),

                    it("should be equal in converting local_time to epoch and epoch to local_time",
                      fun() ->
                              LocalTime = erlang:localtime(),
                              Epoch = datetime_utils:localtime_to_epoch(LocalTime),
                              DateTime2 = datetime_utils:epoch_to_localtime(Epoch),
                              ?assertEqual(LocalTime, DateTime2)
                      end)

            end).
