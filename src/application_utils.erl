%%%-------------------------------------------------------------------
%%% @author Jack Tang <jack@taodinet.com>
%%% @copyright (C) 2012, Jack Tang
%%% @doc
%%%
%%% @end
%%% Created : 28 Dec 2012 by Jack Tang <jack@taodinet.com>
%%%-------------------------------------------------------------------
-module(application_utils).

%% API
-export([start_supervisor/3]).
-export([one4one_supervisor/1,
         one4one_supervisor/2,
         one4all_supervisor/1]).
-export([get_env/2]).
-export([supervisor_spec/3]).
-export([child_spec/1,
         child_spec/2,
         child_spec/3,
         child_spec/4,
         no_offending_child_spec/2,
        ]).
-export([dynamic_child_spec/1,
         dynamic_child_spec/2,
         dynamic_child_spec/3]).

-export([memory/1]).

-define(MAXR, 1000).
-define(MAXT, 3600).


%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
get_env(Par, Def) ->
    case application:get_env(Par) of
        undefined -> {ok, Def};
        {ok, Val} -> {ok, Val}
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_supervisor(SupModule, Module, Args) ->
    supervisor:start_link({local, Module}, SupModule, [Module | Args]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
one4one_supervisor(Specs) when is_list(Specs)->
    gen_supervisor(one_for_one, Specs);
one4one_supervisor(Spec) ->
    gen_supervisor(one_for_one, [Spec]).

one4one_supervisor(simple, Specs) when is_list(Specs) ->
    gen_supervisor(simple_one_for_one, Specs);
one4one_supervisor(simple, Spec) ->
    gen_supervisor(simple_one_for_one, [Spec]).

one4all_supervisor(Specs) when is_list(Specs) ->
    gen_supervisor(one_for_all, Specs);
one4all_supervisor(Spec) ->
    gen_supervisor(one_for_all, [Spec]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
supervisor_spec(SupModule, Module, Args) ->
    {Restart, Shutdown, Type} = {transient, infinity, supervisor},
    {Module,
     {?MODULE, start_supervisor, [SupModule, Module, Args]},
     Restart, Shutdown, Type ,[SupModule]}.

child_spec(Module) ->
    child_spec(Module, []).
child_spec(Module, Args) ->
    child_spec(Module, Module, Args, transient).
child_spec(Module, Args, RestartPolicy) ->
    child_spec(Module, Module, Args, RestartPolicy).  % Restart = transient, temporary

dynamic_child_spec(Module) ->
    dynamic_child_spec(Module, []).
dynamic_child_spec(Module, Args) ->
    child_spec(undefined, Module, Args, transient).
dynamic_child_spec(Module, Args, RestartPolicy) ->
    child_spec(undefined, Module, Args, RestartPolicy).

child_spec(Name, Module, Args, RestartPolicy) ->
    {Restart, Shutdown, Type} = {RestartPolicy, 2000, worker},
    {Name,
     {Module, start_link, Args},
     Restart, Shutdown, Type, [Module]}.

no_offending_child_spec(Module, Args) ->
    {Restart, Shutdown, Type} = {transient, 2000, supervisor},
    {undefined,
     {offending_child_supervisor, start_link, [Module, Args]},
     Restart, Shutdown, Type, [Module]}.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
memory(App) ->
    case  application_controller:get_master(App) of
        Master when is_pid(Master) ->
            {Root, Name} = application_master:get_child(Master),
            {Memory, Children} = memory_tree(Root),
            {Name, Memory div 1000000, Children};
        _ ->
            undefined
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================
gen_supervisor(RestartStrategy, Specs) when is_list(Specs)->
    SupFlags = {RestartStrategy, ?MAXR, ?MAXT},
    {ok, {SupFlags, Specs}}.


memory_tree(Sup) ->
    Infos = supervisor:which_children(Sup),
    {M, E} =
        lists:foldl(
          fun({Name, PId, Type, _}, {Total, Memories}) ->
                  case Type of
                      worker ->
                          {memory, Memory} = process_info(PId, memory),
                          NTotal = Total + Memory,
                          NMemories =
                              case Name of
                                  undefined -> Memories;
                                  Name      -> [{Name, Memory}|Memories]
                              end,
                          {NTotal, NMemories};
                      supervisor ->
                          {Memory, Each} = memory_tree(PId),
                          NTotal = Total + Memory,
                          {NTotal, [{Name, {Memory, Each}}|Memories]}
                  end
          end, {0, []}, Infos),
    NE = lists:map(
           fun({N, Mem}) ->
                   case Mem of
                       Mem when is_integer(Mem) ->
                           {N, Mem div 1000000, undefined};
                       {T, RestEach} ->
                           {N, T div 1000000, RestEach}
                   end
           end, E),
    NNE = lists:reverse(lists:keysort(2, NE)),
    {M, NNE}.
