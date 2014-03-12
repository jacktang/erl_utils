%%%-------------------------------------------------------------------
%%% @author Jack Tang <jack@taodi.local>
%%% @copyright (C) 2014, Jack Tang
%%% @doc
%%%
%%% @end
%%% Created : 12 Mar 2014 by Jack Tang <jack@taodi.local>
%%%-------------------------------------------------------------------
-module(node_utils).

%% API
-export([]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Check the specific node exist or not, if the node is alive, return the pid,
%% otherwise start the node through [M, F, A] 
%% @spec
%% @end
%%--------------------------------------------------------------------
touch_node(NodeName, [M, F, A]) ->
    case whereis(NodeName) of
        undefined -> apply(M, F, A);
        Node ->   Node
    end;

touch_node(NodeName, [F, A]) ->
    case whereis(NodeName) of
        undefined -> apply(F, A);
        Node ->  Node
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
