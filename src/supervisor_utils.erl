%%%-------------------------------------------------------------------
%%% @author Jack Tang <jack@taodinet.com>
%%% @copyright (C) 2014, Jack Tang
%%% @doc
%%%
%%% @end
%%% Created :  8 May 2014 by Jack Tang <jack@taodinet.com>
%%%-------------------------------------------------------------------
-module(supervisor_utils).

%% API
-export([start_child/2, start_child/3,
         start_no_offending_child/2, start_no_offending_child/3]).
-export([grep_children/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_child(Sup, Args) ->
    case supervisor:start_child(Sup, Args) of
        {ok, Handler} ->
            {ok, Handler};
        {ok, Handler, _Info} ->
            {ok, Handler};
        {error, {already_started, Handler}} ->
            {ok, Handler};
        {error, Reason} ->
            {error, Reason}
    end.

start_child(PName, Sup, Args) ->
    case whereis(PName) of
        undefined ->
            start_child(Sup, Args);
        PId when is_pid(PId) ->
            {ok, PId}
    end.

start_no_offending_child(Sup, Args) ->
    case supervisor:start_child(Sup, [Args]) of
        {ok, Handler} ->
            case supervisor:which_children(Handler) of
                [{_, PId, _, _}] ->
                    {ok, PId};
                Other ->
                    {error, {invalid_supervisor_children, Other}}
            end;
        {error,{shutdown,{failed_to_start_child,undefined,
                          {already_started, Handler}}}} ->
            {ok, Handler};
        {error, Reason} ->
            {error, Reason}
    end.

start_no_offending_child(PName, Sup, Args) ->
    case whereis(PName) of
        undefined ->
            start_no_offending_child(Sup, Args);
        PId when is_pid(PId) ->
            {ok, PId}
    end.

grep_children(RegExp) ->
    ok.

grep_children(RegExp, Fun) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
