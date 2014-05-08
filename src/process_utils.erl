%%%-------------------------------------------------------------------
%%% @author Jack Tang <jack@taodinet.com>
%%% @copyright (C) 2013, Jack Tang
%%% @doc
%%%
%%% @end
%%% Created :  2 Jul 2013 by Jack Tang <jack@taodinet.com>
%%%-------------------------------------------------------------------
-module(process_utils).

%% API
-export([generate_name/2]).
-export([to_pid/1]).
-export([state/1,
         state/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% process_utils:take_name(?MODULE, Data)
%% 
%% @spec
%% @end
%%--------------------------------------------------------------------
generate_name(Namespace, Data) ->
    list_to_atom(type_utils:any_to_list(Namespace) ++ "_" ++
                 integer_to_list(erlang:phash2(Data))).

%%--------------------------------------------------------------------
%% @doc
%% process_utils:to_pid("<0.4.1>")
%% 
%% @spec
%% @end
%%--------------------------------------------------------------------
to_pid(PidStr) when is_list(PidStr) ->
    erlang:list_to_pid(PidStr);

to_pid(PName) when is_atom(PName) ->
  whereis(PName).

%%--------------------------------------------------------------------
%% @doc
%% Pid = process_utils:to_pid("<0.4.1>"),
%% process_utils:register_name(Pid).
%% 
%% @spec
%% @end
%%--------------------------------------------------------------------
registered_name(Pid) ->
    erlang:process_info(Pid, registered_name).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
state(Pid) when is_pid(Pid) ->
    element(2, lists:nth(1, element(2, lists:nth(3, lists:nth(5, element(4, sys:get_status(Pid))))))).

state(Pid, N) when is_pid(Pid) ->
    State = state(Pid),
    element(N+1, State).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
messages(Pid) when is_pid(Pid) ->
   erlang:process_info(Pid, messages).

%%%===================================================================
%%% Internal functions
%%%===================================================================
