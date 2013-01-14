%%%-------------------------------------------------------------------
%%% @author Jack Tang <jack@taodinet.com>
%%% @copyright (C) 2013, Jack Tang
%%% @doc
%%%
%%% @end
%%% Created : 14 Jan 2013 by Jack Tang <jack@taodinet.com>
%%%-------------------------------------------------------------------
-module(type_utils).

%% API
-export([any_to_list/1,
         any_to_atom/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Converts to list
%%
%% @end
%%--------------------------------------------------------------------
any_to_list(undefined) ->
    "";
any_to_list(List) when is_list(List) ->
    List;
any_to_list(Binary) when is_binary(Binary) ->
    binary_to_list(Binary);
any_to_list(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
any_to_list(_) ->
    throw(badarg).


any_to_atom(Atom) when is_atom(Atom) ->
    Atom;
any_to_atom(List) when is_list(List) ->
    list_to_atom(List);
any_to_atom(Binary) when is_binary(Binary) ->
    binary_to_list(Binary);
any_to_atom(_) ->
    throw(badarg).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------


%%%===================================================================
%%% Internal functions
%%%===================================================================
