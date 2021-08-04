-module(bn_peer).

-include("bn_jsonrpc.hrl").
-behavior(bn_jsonrpc_handler).

-export([handle_rpc/2]).

%%
%% jsonrpc_handler
%%
handle_rpc(<<"peer_book_self">>, []) ->
    peer_book_response(self);
handle_rpc(_, _) ->
    ?jsonrpc_error(method_not_found).

%%
%% Internal
%%

peer_book_response(Target) ->
    TID = blockchain_swarm:tid(),
    Peerbook = libp2p_swarm:peerbook(TID),

    %% Always return a list here, even when there's just
    %% a single entry
    case Target of
        all ->
            [ format_peer(P) || P <- libp2p_peerbook:values(Peerbook) ];
        self ->
            {ok, Peer} = libp2p_peerbook:get(Peerbook, blockchain_swarm:pubkey_bin()),
            [ lists:foldl(fun(M, Acc) -> maps:merge(Acc, M) end,
                        format_peer(Peer),
                        [format_listen_addrs(TID, libp2p_peer:listen_addrs(Peer)),
                         format_peer_sessions(TID)]
                       ) ];
        Addrs when is_list(Addrs) ->
            [begin
                 {ok, P} = libp2p_peerbook:get(Peerbook, A),
                 lists:foldl(fun(M, Acc) -> maps:merge(Acc, M) end,
                             format_peer(P),
                             [format_listen_addrs(TID, libp2p_peer:listen_addrs(P)),
                              format_peer_connections(P)])
             end || A <- Addrs ];
        Addr ->
            {ok, P} = libp2p_peerbook:get(Peerbook, Addr),
            [ lists:foldl(fun(M, Acc) -> maps:merge(Acc, M) end,
                          format_peer(P),
                          [format_listen_addrs(TID, libp2p_peer:listen_addrs(P)),
                          format_peer_connections(P)]) ]
    end.

format_peer(Peer) ->
    ListenAddrs = libp2p_peer:listen_addrs(Peer),
    ConnectedTo = libp2p_peer:connected_peers(Peer),
    NatType = libp2p_peer:nat_type(Peer),
    Timestamp = libp2p_peer:timestamp(Peer),
    Bin = libp2p_peer:pubkey_bin(Peer),
    M = #{
        <<"address">> => libp2p_crypto:pubkey_bin_to_p2p(Bin),
        <<"name">> => ?BIN_TO_ANIMAL(Bin),
        <<"listen_addr_count">> => length(ListenAddrs),
        <<"connection_count">> => length(ConnectedTo),
        <<"nat">> => NatType,
        <<"last_updated">> => (erlang:system_time(millisecond) - Timestamp) / 1000
    },
    maps:map(fun(_K, V) -> ?TO_VALUE(V) end, M).

format_peer_connections(Peer) ->
    #{
        <<"connections">> => [
            ?TO_VALUE(libp2p_crypto:pubkey_bin_to_p2p(P))
         || P <- libp2p_peer:connected_peers(Peer)
        ]
    }.

format_listen_addrs(TID, Addrs) ->
    libp2p_transport:sort_addrs(TID, Addrs),
    #{<<"listen_addresses">> => [?TO_VALUE(A) || A <- Addrs]}.

format_peer_sessions(Swarm) ->
    SessionInfos = libp2p_swarm:sessions(Swarm),
    Rs = lists:filtermap(
        fun({A, S}) ->
            case multiaddr:protocols(A) of
                [{"p2p", B58}] ->
                    {true, {A, libp2p_session:addr_info(libp2p_swarm:tid(Swarm), S), B58}};
                _ ->
                    false
            end
        end,
        SessionInfos
    ),

    FormatEntry = fun({MA, {SockAddr, PeerAddr}, B58}) ->
        M = #{
            <<"local">> => SockAddr,
            <<"remote">> => PeerAddr,
            <<"p2p">> => MA,
            <<"name">> => ?B58_TO_ANIMAL(B58)
        },
        maps:map(fun(_K, V) -> ?TO_VALUE(V) end, M)
    end,
    #{ <<"sessions">> => [FormatEntry(E) || E <- Rs] }.