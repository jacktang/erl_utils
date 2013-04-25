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
         any_to_atom/1,
         any_to_binary/1]).

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
any_to_list(Bin) when is_binary(Bin) ->
    case unicode:characters_to_binary(Bin,utf8,utf8) of
        Bin -> unicode:characters_to_list(Bin);
        _ -> binary_to_list(Bin)
    end;
any_to_list(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
any_to_list(_) ->
    throw(badarg).

%%--------------------------------------------------------------------
%% @doc
%% Converts to list
%%
%% @end
%%--------------------------------------------------------------------
any_to_atom(Atom) when is_atom(Atom) ->
    Atom;
any_to_atom(List) when is_list(List) ->
    list_to_atom(List);
any_to_atom(Bin) when is_binary(Bin) ->
    list_to_atom(any_to_list(Bin));
any_to_atom(_) ->
    throw(badarg).

%%--------------------------------------------------------------------
%% @doc
%% Converts to list
%%
%% @end
%%--------------------------------------------------------------------
any_to_binary(Bin) when is_binary(Bin)->
    Bin;
any_to_binary(List) when is_list(List) ->
    case unicode:characters_to_binary(List) of
        {error,_,_} -> list_to_binary(List);
        B -> case unicode:characters_to_list(B,utf8) of
                 List -> B;
                 _ -> list_to_binary(List)
             end
    end;
any_to_binary(Atom) when is_atom(Atom) ->
    erlang:atom_to_binary(Atom, utf8);
any_to_binary(_) ->
    throw(badarg).
    

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------


%%%===================================================================
%%% Internal functions
%%%===================================================================
