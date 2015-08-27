%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2015, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 27 Aug 2015 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(supervisor_utils).

%% API
-export([start_child/2, start_child/3,
         start_no_offending_child/2, start_no_offending_child/3]).

%%%===================================================================
%%% API
%%%===================================================================
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
            start_protected_child(Sup, Args);
        PId when is_pid(PId) ->
            {ok, PId}
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
