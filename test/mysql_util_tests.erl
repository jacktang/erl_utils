-module(mysql_util_tests).

-include_lib("eunit/include/eunit.hrl").

query_statement_test_() ->
    [?_assertEqual(<<"select * from settlements">>,
                   mysql_util:query_statement(settlements, [])),
     ?_assertEqual(<<"select * from settlements where investor_id = ? and trading_day = ?">>,
                   mysql_util:query_statement(settlements, 
                                              [{investor_id, 1}, {trading_day,2}])),
     ?_assertEqual(<<"select count(*), sum(win_by_exit) from settlements where investor_id = ? group by investor_id, trading_day">>,
                   mysql_util:query_statement(settlements,
                                              [investor_id],
                                              #{columns => ["count(*)", "sum(win_by_exit)"],
                                                group_by => [investor_id, trading_day]}))],
    ?_assertEqual(<<"select count(*) as count, instrument_code as instrument_id, sum(win_by_exit) as win_by_exit from settlements">>,
                    mysql_util:query_statement(settlements, 
                                               [],
                                               #{columns => [{{count, '*'}, count},
                                                             {instrument_code, instrument_id},
                                                             {{sum}, win_by_exit}]})).
