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
-export([epoch/0, now_to_seconds/1]).
-export([epoch_to_now/1,
         localtime_to_epoch/1,
         epoch_to_localtime/1]).
-export([localtime_as_string/0,
         utc_as_string/0,
         datetime_as_string/1]).
-export([parse_date/2]).
-export([beginning_of_day/1, end_of_day/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
epoch() ->
    now_to_seconds(now()).
localtime_to_epoch({_D, _T} = DateTime) ->
    [UTCTime] = calendar:local_time_to_universal_time_dst(DateTime),
    gregorian_seconds_to_epoch(calendar:datetime_to_gregorian_seconds(UTCTime)).

now_to_seconds({Mega, Sec, _}) ->
    (Mega * 1000000) + Sec.   
  
epoch_to_now(Epoch) ->
    {Epoch div 1000000, Epoch rem 1000000, 0}.

epoch_to_localtime(Epoch) ->
    Now = epoch_to_now(Epoch),
    calendar:now_to_local_time(Now).

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
    datetime_as_string(calendar:now_to_universal_time(now())).

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
parse_date(DateInput, 'yyyy-MM-dd' = _Formatter) ->
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
parse_date(DateInput, 'yyyyMMdd' = _Formatter) ->
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
%% @spec beginning_of_day(Input) -> Datetime
%% @end
%%--------------------------------------------------------------------
end_of_day({date, Date}) ->
    {Date, {23, 59, 59}};
end_of_day({datetime, {Date, _Time}}) ->
    {Date, {23, 59, 59}};
end_of_day({Date, Formatter}) ->
    {parse_date(Date, Formatter), {23, 59, 59}}.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================
epoch_gregorian_seconds() ->
    calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}).

gregorian_seconds_to_epoch(Secs) ->
    EpochSecs = epoch_gregorian_seconds(),
    Secs - EpochSecs.
