-define(METRICS_HISTOGRAM_BUCKETS, [50, 100, 250, 500, 1000, 2000, 5000, 10000, 30000, 60000]).

-define(METRICS_BLOCK_ABSORB, "blockchain_block_absorb_duration").
-define(METRICS_BLOCK_UNVAL_ABSORB, "blockchain_block_unval_absorb_duration").
-define(METRICS_BLOCK_HEIGHT, "blockchain_block_height").
-define(METRICS_BLOCK_UNVAL_HEIGHT, "blockchain_block_unval_height").
-define(METRICS_TXN_ABSORB_DURATION, "blockchain_txn_absorb_duration").
-define(METRICS_TXN_BLOCK_SPAN, "blockchain_txn_mgr_block_span").
-define(METRICS_TXN_QUEUE, "blockchain_txn_mgr_queue").
-define(METRICS_TXN_SUBMIT_COUNT, "blockchain_txn_mgr_submitted_count").
-define(METRICS_TXN_REJECT_COUNT, "blockchain_txn_mgr_rejected_count").
-define(METRICS_TXN_ACCEPT_COUNT, "blockchain_txn_mgr_accepted_count").
-define(METRICS_TXN_UPDATE_COUNT, "blockchain_txn_mgr_updated_count").
-define(METRICS_TXN_PROCESS_DURATION, "blockchain_txn_mgr_process_duration").
-define(METRICS_TXN_CACHE_SIZE, "blockchain_txn_mgr_cache_size").
-define(METRICS_TXN_BLOCK_TIME, "blockchain_txn_mgr_block_time").
-define(METRICS_TXN_BLOCK_AGE, "blockchain_txn_mgr_block_age").
-define(METRICS_GRPC_SESSIONS, "grpc_session_count").
-define(METRICS_GRPC_LATENCY, "grpcbox_session_latency").

-define(METRICS, #{
    block_metrics => {
        [ [blockchain, block, absorb],
          [blockchain, block, height],
          [blockchain, block, unvalidated_absorb],
          [blockchain, block, unvalidated_height] ],
        [ {?METRICS_BLOCK_ABSORB, prometheus_histogram, [stage], "Block absorb duration"},
          {?METRICS_BLOCK_HEIGHT, prometheus_gauge, [time], "Most recent block height"},
          {?METRICS_BLOCK_UNVAL_ABSORB, prometheus_histogram, [stage], "Block unvalidated absorb duration"},
          {?METRICS_BLOCK_UNVAL_HEIGHT, prometheus_gauge, [time], "Most recent unvalidated block height"} ]
    },
    txn_metrics => {
        [ [blockchain, txn, absorb],
          [blockchain, txn_mgr, submit],
          [blockchain, txn_mgr, reject],
          [blockchain, txn_mgr, accept],
          [blockchain, txn_mgr, update],
          [blockchain, txn_mgr, process],
          [blockchain, txn_mgr, add_block] ],
        [ {?METRICS_TXN_ABSORB_DURATION, prometheus_histogram, [stage], "Txn absorb duration"},
          {?METRICS_TXN_BLOCK_SPAN, prometheus_gauge, [], "Block span on transactions"},
          {?METRICS_TXN_QUEUE, prometheus_gauge, [], "Txn manager submission queue length"},
          {?METRICS_TXN_SUBMIT_COUNT, prometheus_counter, [type], "Count of submitted transactions"},
          {?METRICS_TXN_REJECT_COUNT, prometheus_counter, [type], "Count of rejected transactions"},
          {?METRICS_TXN_ACCEPT_COUNT, prometheus_counter, [type], "Count of accepted transactions"},
          {?METRICS_TXN_UPDATE_COUNT, prometheus_counter, [type], "Count of updated transaction"},
          {?METRICS_TXN_PROCESS_DURATION, prometheus_histogram, [stage], "Transaction manager cache process duration"},
          {?METRICS_TXN_CACHE_SIZE, prometheus_gauge, [height], "Transaction manager buffer size"},
          {?METRICS_TXN_BLOCK_TIME, prometheus_gauge, [height], "Block time observed from the transaction manager"},
          {?METRICS_TXN_BLOCK_AGE, prometheus_gauge, [height], "Block age observed from the transaction manager"} ]
    },
    grpc_metrics => {
        [ [grpcbox, server, rpc_begin],
          [grpcbox, server, rpc_end] ],
        [ {?METRICS_GRPC_SESSIONS, prometheus_gauge, [method], "GRPC session count"},
          {?METRICS_GRPC_LATENCY, prometheus_histogram, [method, status], "GRPC session latency"} ]
    }
}).
