%%%-------------------------------------------------------------------
%%% @author Jack Tang <jack@taodinet.com>
%%% @copyright (C) 2013, Jack Tang
%%% @doc
%%%    now = {Mega, Sec, _}
%%%    datetime = {{Y, M, D}, {H, M, S}}
%%%
%%%   epoch <-> now
%%%   epoch <-> datetime(localtime)
%%%   epoch <-> datetime(utc)
%%%
%%% @end
%%% Created :  9 Jan 2013 by Jack Tang <jack@taodinet.com>
%%%-------------------------------------------------------------------
-module(datetime_utils).

%% API
-export([epoch/0,
         date_to_epoch/1,
         datetime_to_epoch/1,
         localtime_to_epoch/1,
         now_to_seconds/1]).
-export([epoch_to_now/1,
         epoch_to_date/1,
         epoch_to_datetime/1,
         epoch_to_localtime/1
        ]).
-export([localtime_as_string/0,
         utc_as_string/0,
         datetime_as_string/1]).
-export([parse_date/2]).
-export([beginning_of_day/1, midday/1, end_of_day/1]).
-export([is_older_by/3, is_sooner_by/3]).
-export([subtract/2, add/2]).
-export([parse_datetime/1]).

-include_lib("parsec/include/parsec.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
epoch() ->
    now_to_seconds(os:timestamp()).
localtime_to_epoch({_D, _T} = DateTime) ->
    [UTCTime] = calendar:local_time_to_universal_time_dst(DateTime),
    gregorian_seconds_to_epoch(calendar:datetime_to_gregorian_seconds(UTCTime)).

now_to_seconds({Mega, Sec, _}) ->
    (Mega * 1000000) + Sec.

epoch_to_now(Epoch) ->
    {Epoch div 1000000, Epoch rem 1000000, 0}.

epoch_to_datetime(Epoch) ->
    calendar:now_to_datetime({Epoch div 1000000, Epoch rem 1000000, 0}).
epoch_to_date(Epoch) ->
    {Date, _Time} = epoch_to_datetime(Epoch),
    Date.

epoch_to_localtime(Epoch) ->
    Now = epoch_to_now(Epoch),
    calendar:now_to_local_time(Now).

date_to_epoch(Date) ->
    datetime_to_epoch({Date, {0,0,0} }).
datetime_to_epoch({Date, Time}) ->
    gregorian_seconds_to_epoch(calendar:datetime_to_gregorian_seconds({Date, Time})).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
localtime_as_string() ->
    datetime_as_string(erlang:localtime()).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
utc_as_string() ->
    datetime_as_string(calendar:now_to_universal_time(os:timestamp())).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
datetime_as_string({_Y, _M, _D} = Date) ->
    datetime_as_string({Date, {0, 0, 0}});
datetime_as_string({{Y, M, D} = _Date, undefined}) ->
    lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B",
                                [Y, M, D]));
datetime_as_string({{Y, M, D} = _Date, {HH, MM, SS} = _Time}) ->
    lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
                                [Y, M, D, HH, MM, SS])).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
parse_date(DateInput, 'yyyy-mm-dd' = _Formatter) ->
    DateStr = type_utils:any_to_list(DateInput),
    ParseFun = fun(Start, Length) ->
                    {I, _} = string:to_integer(string:substr(DateStr, Start, Length)),
                    I
            end,
    Date = (catch {ParseFun(1, 4), ParseFun(6, 2), ParseFun(9, 2)}),
    case catch calendar:valid_date(Date) of
        {'EXIT', Reason} -> throw(Reason);
        true  -> Date;
        false -> throw(invalid_date)
    end;
parse_date(DateInput, 'yyyymmdd' = _Formatter) ->
    DateStr = type_utils:any_to_list(DateInput),
    ParseFun = fun(Start, Length) ->
                       {I, _} = string:to_integer(string:substr(DateStr, Start, Length)),
                       I
               end,
    Date = (catch {ParseFun(1, 4), ParseFun(5, 2), ParseFun(7, 2)}),
    case catch calendar:valid_date(Date) of
        {'EXIT', Reason} -> throw(Reason);
        true -> Date;
        false -> throw(invalid_date)
    end;
parse_date(_Date, Formatter) ->
    throw({unsupported_formatter, Formatter}).


%%--------------------------------------------------------------------
%% @doc
%% @spec beginning_of_day(Input) -> Datetime
%% @end
%%--------------------------------------------------------------------
beginning_of_day({date, Date}) ->
    {Date, {0, 0, 0}};
beginning_of_day({datetime, {Date, _Time}}) ->
    {Date, {0, 0, 0}};
beginning_of_day({Date, Formatter}) ->
    {parse_date(Date, Formatter), {0, 0, 0}}.

%%--------------------------------------------------------------------
%% @doc
%% @spec midday(Input) -> Datetime
%% @end
%%--------------------------------------------------------------------
midday({date, Date}) ->
    {Date, {12, 0, 0}};
midday({datetime, {Date, _T}}) ->
    {Date, {12, 0, 0}};
midday({Date, Formatter}) ->
    {parse_date(Date, Formatter), {12, 0, 0}}.

%%--------------------------------------------------------------------
%% @doc
%% @spec end_of_day(Input) -> Datetime
%% @end
%%--------------------------------------------------------------------
end_of_day({date, Date}) ->
    {Date, {23, 59, 59}};
end_of_day({datetime, {Date, _Time}}) ->
    {Date, {23, 59, 59}};
end_of_day({Date, Formatter}) ->
    {parse_date(Date, Formatter), {23, 59, 59}}.

%%--------------------------------------------------------------------
%% @doc
%% @spec is_older_by
%% @end
%%--------------------------------------------------------------------
is_older_by(T1, T2, {days, N}) ->
    N1 = day_difference(T1, T2),
    case N1 of
        N2 when (-N > N2) ->
            true;
        _ ->
            false
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec is_older_by
%% @end
%%--------------------------------------------------------------------
is_sooner_by(T1, T2, {days, N}) ->
    case day_difference(T1, T2) of
        N1 when N > N1 ->
            true;
        _ ->
            false
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec subtract
%% @end
%%--------------------------------------------------------------------
subtract(Date, {days, N}) ->
    add(Date, {days, -N});

subtract(Epoch, {months, Months}) when is_integer(Epoch) ->
    date_to_epoch(subtract(epoch_to_date(Epoch), {months, Months}));

subtract({Y, M, D}, {months, Month}) when Month >= M ->
    update_last_day({Y - (Month - M) div 12 -1, 12 - (Month - M) rem 12 , D});
subtract({Y, M, D}, {months, Month}) when is_integer(M) ->
    update_last_day({Y, M - Month, D}).

%%--------------------------------------------------------------------
%% @doc
%% @spec add
%% @end
%%--------------------------------------------------------------------
add(Epoch, Diff) when is_integer(Epoch) ->
    date_to_epoch(add(epoch_to_date(Epoch), Diff));

add({_Y, _M, _D} = Date, {days, N}) ->
    New = calendar:date_to_gregorian_days(Date) + N,
    calendar:gregorian_days_to_date(New);

add({Y, M, D}, {months, Month}) ->
    update_last_day({Y + (M + Month - 1) div 12, (M + Month - 1) rem 12 + 1, D}).


%%%===================================================================
%%% Internal functions
%%%===================================================================
epoch_gregorian_seconds() ->
    calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}).

gregorian_seconds_to_epoch(Secs) ->
    EpochSecs = epoch_gregorian_seconds(),
    Secs - EpochSecs.

day_difference({D1, _}, D2) ->
    day_difference(D1, D2);
day_difference(D1, {D2, _}) ->
    day_difference(D1, D2);
day_difference(D1, D2) ->
    Days1 = calendar:date_to_gregorian_days(D1),
    Days2 = calendar:date_to_gregorian_days(D2),
    Days1 - Days2.

update_last_day({Y, M, D}) ->
    LastDayOfMonth = calendar:last_day_of_the_month(Y, M),
    case D > LastDayOfMonth of
        true ->
            {Y, M, LastDayOfMonth};
        false ->
            {Y, M, D}
    end.

parse_datetime(Datetime) when is_binary(Datetime) ->
    parse_datetime(binary_to_list(Datetime));
parse_datetime(Datetime) ->
    case parsec:parse(Datetime, p_datetime()) of
        {ok, V, _} ->
            {ok, V};
        {error, pzero} ->
            {error, parse_failed}
    end.

p_datetime() ->
    parsec:choice([
                   parsec:bind(
                     parsec:choice([p_date_with_sep(), p_date_without_sep()]),
                     fun(Date) ->
                             parsec:choice([parsec:bind(
                                              p_time_all(),
                                              fun(Time) ->
                                                      parsec:return(Time#{date => Date})
                                                          
                                              end),
                                            parsec:return(#{date => Date})])
                     end),
                   p_time_all()]).

p_time_all() ->
    parsec:bind(
      parsec:do([parsec:many1(parsec:char($ )), p_time_with_dot()]),
      fun({Time, Dot}) ->
              DateTime = #{time => Time},
              NDateTime = 
                  case Dot of
                      undefined ->
                          DateTime;
                      {Digits, Dec} ->
                          DateTime#{after_dot => Digits, dec => Dec}
                  end,
              parsec:return(NDateTime)
      end).
                                    
p_integer() ->
    parsec:bind(?DIGITS,
                fun (N) -> 
                        parsec:return(list_to_integer(N))
                end).

p_digit() ->
    parsec:bind(?DIGIT,
                fun (N) -> 
                        parsec:return(list_to_integer([N]))
                end).      

p_int_with_dec() ->
    parsec:bind(parsec:many(p_digit()),
                fun(Digits) ->
                        Len = length(Digits),
                        Num = 
                            lists:foldl(
                              fun(N, Acc) ->
                                      N + Acc * 10
                              end, 0, Digits),
                        parsec:return({Num, Len})
                end).

p_date_with_sep() ->
    parsec:bind(
      parsec:sep_by(p_integer(), parsec:char($-)),
      fun([Y, M, D]) ->
              parsec:return({Y, M, D});
         (_Other) ->
              parsec:pzero()
      end).

p_date_without_sep() ->
    parsec:bind(parsec:many(p_digit()),
                fun([Y1, Y2, Y3, Y4, M1, M2, D1, D2]) ->
                        Date = {Y1 * 1000 + Y2 * 100 + Y3 * 10 + Y4,
                                M1 * 10 + M2,
                                D1 * 10 + D2},
                        parsec:return(Date);
                   ([Y1, Y2, M1, M2]) ->
                        parsec:return({2000 + Y1 * 10 + Y2, M1 * 10 + M2});
                   (_Other) ->
                        parsec:pzero()
                end).

p_time() ->
    parsec:bind(
      parsec:sep_by(p_integer(), parsec:char($:)),
      fun([HH, MM, SS]) ->
              parsec:return({HH, MM, SS});
         (_Other) ->
              parsec:pzero()
      end).

p_time_with_dot() ->
    parsec:bind(
      p_time(),
      fun(Time) ->
              parsec:bind(parsec:many(parsec:do([parsec:char($.), p_int_with_dec()])),
                          fun([]) ->
                                  parsec:return({Time, undefined});
                             ([AfterDot]) ->
                                  parsec:return({Time, AfterDot})
                          end)
      end).



















