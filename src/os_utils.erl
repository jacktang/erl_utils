%%%-------------------------------------------------------------------
%%% @author Jack Tang <jack@taodinet.com>
%%% @copyright (C) 2014, Jack Tang
%%% @doc
%%%
%%% @end
%%% Created : 23 Oct 2014 by Jack Tang <jack@taodinet.com>
%%%-------------------------------------------------------------------
-module(os_utils).

%% API
-export([mac_addr/0, mac_addr/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieve the first MAC addresses for machine
%% @spec
%% @end
%%--------------------------------------------------------------------
-spec(mac_addr() -> string()).
mac_addr() ->
    case get_mac_addresses() of
        {error, Reason} -> {error, Reason};
        [MacAddr | _]   -> to_hex_str(MacAddr)
    end.

%%% Retrieve MAC address for specific interface
-spec(mac_addr(string()) -> string()).
mac_addr(Interface) ->
    case get_mac_address(Interface) of
        {error, Reason} -> {error, Reason};
        MacAddr         -> to_hex_str(MacAddr)
    end.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================
get_mac_addresses() ->
    case inet:getifaddrs() of
        {ok, IfAddrs} ->
            Ret = lists:foldl(
                    fun({_, IfAttrs}, AccIn) ->
                            case tuple_list_utils:keyfind(hwaddr, IfAttrs) of
                                {_, undefined}     ->
                                    AccIn;
                                {_, [0,0,0,0,0,0]} ->
                                    AccIn; 
                                {_, MacAddrDecimal} ->
                                    [MacAddrDecimal | AccIn]
                            end
                    end, [], IfAddrs),
            case Ret of
                []    -> {error, no_macaddr_available};
                Value -> lists:usort(Value)
            end;
        {error, Reason} -> {error, Reason}
    end.

get_mac_address(Interface) ->
    case inet:getifaddrs() of
        {ok, IfAddrs} ->
            case tuple_list_utils:keyfind(Interface, IfAddrs) of
                {_, undefined} -> {error, no_iterface};
                {_, IfAttrs} ->
                    case tuple_list_utils:keyfind(hwaddr, IfAttrs) of
                        {_, undefined}     -> {error, no_macaddr_available};
                        {_, [0,0,0,0,0,0]} -> {error, no_macaddr_available}; 
                        {_, MacAddrDecimal} -> MacAddrDecimal
                    end
            end;
        {error, Reason} -> {error, Reason}
    end.
    
                             
to_hex_str(MacAddrDecimal) ->
    HexList = [string:right(integer_to_list(I, 16), 2, $0) || I <- MacAddrDecimal ],
    string:join(HexList, ":").
