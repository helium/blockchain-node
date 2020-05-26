-define (jsonrpc_b58_to_bin(B), bn_jsonrpc_handler:jsonrpc_b58_to_bin((B))).
-define (jsonrpc_b64_to_bin(B), bn_jsonrpc_handler:jsonrpc_b64_to_bin((B))).
-define(jsonrpc_error(E), bn_jsonrpc_handler:jsonrpc_error((E))).

-define (BIN_TO_B58(B), list_to_binary(libp2p_crypto:bin_to_b58((B)))).
-define (B58_TO_BIN(B), libp2p_crypto:b58_to_bin(binary_to_list((B)))).

-define (BIN_TO_B64(B), base64url:encode((B))).
-define (B64_TO_BIN(B), base64url:decode((B))).
