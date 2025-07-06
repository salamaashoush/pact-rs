;; performance-monitoring.pact
;; Tools and utilities for performance monitoring and gas analysis

(module performance-monitoring GOVERNANCE
  @doc "Performance monitoring and gas analysis tools"
  
  (defcap GOVERNANCE ()
    (enforce-keyset 'admin))
  
  ;; ==========================================================================
  ;; PERFORMANCE MEASUREMENT SCHEMAS
  ;; ==========================================================================
  
  (defschema performance-metric
    @doc "Performance measurement record"
    function-name:string
    input-size:integer
    execution-time:decimal
    estimated-gas:decimal
    memory-usage:integer
    timestamp:time
    test-parameters:object)
  
  (deftable performance-metrics:{performance-metric})
  
  (defschema benchmark-result
    @doc "Benchmark comparison result"
    test-name:string
    implementations:[object]
    winner:string
    improvement-factor:decimal
    timestamp:time)
  
  (deftable benchmark-results:{benchmark-result})
  
  (defschema gas-profile
    @doc "Gas consumption profile"
    operation-type:string
    min-gas:decimal
    max-gas:decimal
    avg-gas:decimal
    total-executions:integer
    total-gas:decimal
    last-update:time)
  
  (deftable gas-profiles:{gas-profile})
  
  ;; ==========================================================================
  ;; GAS MEASUREMENT UTILITIES
  ;; ==========================================================================
  
  (defun profile-operation:object (operation-name:string operation-func input-data:object iterations:integer)
    @doc "Profile operation performance across multiple iterations"
    (let ((measurements (map (lambda (i)
                               (measure-single-execution operation-name operation-func input-data))
                             (enumerate 1 iterations))))
      
      ;; Calculate statistics
      (let ((gas-measurements (map (at 'estimated-gas) measurements))
            (time-measurements (map (at 'execution-time) measurements)))
        
        { "operation": operation-name,
          "iterations": iterations,
          "gas-stats": {
            "min": (fold (lambda (a b) (if (< a b) a b)) 999999999.0 gas-measurements),
            "max": (fold (lambda (a b) (if (> a b) a b)) 0.0 gas-measurements),
            "avg": (/ (fold (+) 0.0 gas-measurements) iterations),
            "total": (fold (+) 0.0 gas-measurements)
          },
          "time-stats": {
            "min": (fold (lambda (a b) (if (< a b) a b)) 999999999.0 time-measurements),
            "max": (fold (lambda (a b) (if (> a b) a b)) 0.0 time-measurements),
            "avg": (/ (fold (+) 0.0 time-measurements) iterations)
          },
          "measurements": measurements })))
  
  (defun measure-single-execution:object (operation-name:string operation-func input-data:object)
    @doc "Measure single operation execution"
    (let ((start-time (at 'block-time (chain-data)))
          (start-block (chain-data 'block-height)))
      
      ;; Execute operation (in real implementation, this would call the actual function)
      (let ((result (execute-operation operation-func input-data)))
        (let ((end-time (at 'block-time (chain-data)))
              (end-block (chain-data 'block-height)))
          
          { "operation": operation-name,
            "execution-time": (diff-time end-time start-time),
            "estimated-gas": (estimate-gas-from-time (diff-time end-time start-time)),
            "result": result,
            "block-span": (- end-block start-block),
            "timestamp": start-time }))))
  
  (defun execute-operation:object (operation-func input-data:object)
    @doc "Execute operation based on function identifier"
    ;; Simplified operation execution
    ;; In practice, this would use dynamic function calling
    (cond
      ((= operation-func "list-sum")
       { "result": (fold (+) 0.0 (at 'numbers input-data)) })
      ((= operation-func "list-filter")
       { "result": (filter (> (at 'threshold input-data)) (at 'numbers input-data)) })
      ((= operation-func "object-lookup")
       { "result": (at (at 'key input-data) (at 'object input-data)) })
      ((= operation-func "database-read")
       { "result": "simulated-db-read" })
      ((= operation-func "database-write")
       { "result": "simulated-db-write" })
      { "result": "unknown-operation" }))
  
  (defun estimate-gas-from-time:decimal (execution-time:decimal)
    @doc "Estimate gas consumption from execution time"
    ;; Simplified estimation: 1 second ≈ 1000 gas units
    ;; Real implementation would use actual gas metering
    (* execution-time 1000.0))
  
  ;; ==========================================================================
  ;; BENCHMARK COMPARISON UTILITIES
  ;; ==========================================================================
  
  (defun compare-implementations:object (test-name:string implementations:[object] test-data:object)
    @doc "Compare performance of different implementations"
    (let ((results (map (lambda (impl)
                          (bind impl { "name" := name, "function" := func }
                            (let ((profile (profile-operation name func test-data 10)))
                              { "name": name,
                                "avg-gas": (at 'avg (at 'gas-stats profile)),
                                "avg-time": (at 'avg (at 'time-stats profile)),
                                "total-gas": (at 'total (at 'gas-stats profile)) })))
                        implementations)))
      
      ;; Find best performer (lowest average gas)
      (let ((best (fold (lambda (current-best impl)
                          (if (< (at 'avg-gas impl) (at 'avg-gas current-best))
                              impl
                              current-best))
                        (at 0 results)
                        results)))
        
        ;; Calculate improvement factors
        (let ((best-gas (at 'avg-gas best))
              (results-with-improvement 
                (map (lambda (result)
                       (+ result { "improvement-factor": (/ (at 'avg-gas result) best-gas) }))
                     results)))
          
          ;; Store benchmark result
          (let ((benchmark-id (hash [test-name (at 'block-time (chain-data))])))
            (insert benchmark-results benchmark-id {
              "test-name": test-name,
              "implementations": results-with-improvement,
              "winner": (at 'name best),
              "improvement-factor": (/ (at 'avg-gas (at 1 results)) best-gas),
              "timestamp": (at 'block-time (chain-data))
            })
            
            { "benchmark-id": benchmark-id,
              "winner": (at 'name best),
              "results": results-with-improvement })))))
  
  ;; ==========================================================================
  ;; GAS PROFILING UTILITIES
  ;; ==========================================================================
  
  (defun update-gas-profile:string (operation-type:string gas-consumed:decimal)
    @doc "Update gas consumption profile for operation type"
    (with-default-read gas-profiles operation-type
      { "min-gas": 999999999.0, "max-gas": 0.0, "avg-gas": 0.0, 
        "total-executions": 0, "total-gas": 0.0 }
      { "min-gas" := min-gas, "max-gas" := max-gas, "total-executions" := count, "total-gas" := total }
      
      (let ((new-count (+ count 1))
            (new-total (+ total gas-consumed))
            (new-min (if (< gas-consumed min-gas) gas-consumed min-gas))
            (new-max (if (> gas-consumed max-gas) gas-consumed max-gas))
            (new-avg (/ new-total new-count)))
        
        (write gas-profiles operation-type {
          "operation-type": operation-type,
          "min-gas": new-min,
          "max-gas": new-max,
          "avg-gas": new-avg,
          "total-executions": new-count,
          "total-gas": new-total,
          "last-update": (at 'block-time (chain-data))
        }))))
  
  (defun get-gas-profile:object (operation-type:string)
    @doc "Get gas consumption profile for operation type"
    (read gas-profiles operation-type))
  
  (defun list-all-profiles:[object] ()
    @doc "List all gas consumption profiles"
    (select gas-profiles (constantly true)))
  
  ;; ==========================================================================
  ;; PERFORMANCE TEST SUITES
  ;; ==========================================================================
  
  (defun run-database-performance-tests:[object] ()
    @doc "Run comprehensive database performance tests"
    (let ((test-scenarios [
      { "name": "single-read",
        "description": "Single database read operation",
        "operation": "database-read",
        "data": { "table": "accounts", "key": "test-account" } },
      { "name": "batch-read",
        "description": "Multiple database reads",
        "operation": "database-batch-read", 
        "data": { "table": "accounts", "keys": ["acc1", "acc2", "acc3"] } },
      { "name": "single-write",
        "description": "Single database write operation",
        "operation": "database-write",
        "data": { "table": "accounts", "key": "test-account", "data": {} } },
      { "name": "batch-write",
        "description": "Multiple database writes",
        "operation": "database-batch-write",
        "data": { "table": "accounts", "operations": [{}, {}, {}] } }
    ]))
      
      (map (lambda (scenario)
             (bind scenario { "name" := name, "operation" := op, "data" := test-data }
               (profile-operation name op test-data 5)))
           test-scenarios)))
  
  (defun run-algorithm-performance-tests:[object] ()
    @doc "Run algorithm complexity performance tests"
    (let ((input-sizes [10, 100, 1000, 10000])
          (algorithms ["linear-search", "binary-search", "hash-lookup"]))
      
      (map (lambda (size)
             (let ((test-data { "size": size, "data": (enumerate 1 size) }))
               { "input-size": size,
                 "algorithm-results": (map (lambda (algo)
                                            (profile-operation algo algo test-data 3))
                                          algorithms) }))
           input-sizes)))
  
  (defun run-data-structure-tests:[object] ()
    @doc "Run data structure performance tests"
    (let ((test-cases [
      { "name": "list-operations",
        "tests": ["list-append", "list-filter", "list-map", "list-fold"] },
      { "name": "object-operations", 
        "tests": ["object-lookup", "object-merge", "object-keys"] },
      { "name": "string-operations",
        "tests": ["string-concat", "string-format", "string-length"] }
    ]))
      
      (map (lambda (test-case)
             (bind test-case { "name" := category, "tests" := operations }
               { "category": category,
                 "operation-results": (map (lambda (op)
                                            (profile-operation op op {} 5))
                                          operations) }))
           test-cases)))
  
  ;; ==========================================================================
  ;; OPTIMIZATION RECOMMENDATION ENGINE
  ;; ==========================================================================
  
  (defun analyze-performance-bottlenecks:[object] (profile-data:[object])
    @doc "Analyze performance data and suggest optimizations"
    (let ((bottlenecks (filter (lambda (profile)
                                 (> (at 'avg (at 'gas-stats profile)) 10000.0))
                               profile-data)))
      
      (map (lambda (bottleneck)
             (let ((operation (at 'operation bottleneck))
                   (avg-gas (at 'avg (at 'gas-stats bottleneck))))
               { "operation": operation,
                 "avg-gas": avg-gas,
                 "severity": (cond
                               ((> avg-gas 100000.0) "critical")
                               ((> avg-gas 50000.0) "high")
                               ((> avg-gas 10000.0) "medium")
                               "low"),
                 "recommendations": (generate-recommendations operation avg-gas) }))
           bottlenecks)))
  
  (defun generate-recommendations:[string] (operation:string avg-gas:decimal)
    @doc "Generate optimization recommendations based on operation and gas usage"
    (cond
      ((contains "database" operation)
       (cond
         ((> avg-gas 50000.0)
          ["Consider batching database operations",
           "Use with-read instead of separate read/update",
           "Minimize select operations",
           "Implement caching for frequently accessed data"])
         ((> avg-gas 25000.0)
          ["Batch multiple operations where possible",
           "Consider using with-default-read for optional data"])
         ["Optimize data structure to reduce read size"]))
      
      ((contains "list" operation)
       (if (> avg-gas 10000.0)
           ["Use more efficient algorithms (O(n) vs O(n²))",
            "Consider using objects for lookups instead of lists",
            "Minimize list traversals"]
           ["Consider list size optimization"]))
      
      ((contains "object" operation)
       (if (> avg-gas 5000.0)
           ["Minimize object field access",
            "Use destructuring instead of repeated at calls",
            "Consider object size optimization"]
           ["Object operations are reasonably efficient"]))
      
      ["General optimization: review algorithm complexity",
       "Consider caching for repeated computations"]))
  
  ;; ==========================================================================
  ;; REPORTING UTILITIES
  ;; ==========================================================================
  
  (defun generate-performance-report:object (test-results:[object])
    @doc "Generate comprehensive performance report"
    (let ((total-tests (length test-results))
          (total-gas (fold (+) 0.0 (map (lambda (result)
                                          (at 'total (at 'gas-stats result)))
                                        test-results)))
          (avg-gas (/ total-gas total-tests)))
      
      { "summary": {
          "total-tests": total-tests,
          "total-gas-consumed": total-gas,
          "average-gas-per-test": avg-gas,
          "generated-at": (at 'block-time (chain-data))
        },
        "bottlenecks": (analyze-performance-bottlenecks test-results),
        "detailed-results": test-results,
        "recommendations": (generate-global-recommendations test-results) }))
  
  (defun generate-global-recommendations:[string] (test-results:[object])
    @doc "Generate global optimization recommendations"
    (let ((high-gas-operations (filter (lambda (result)
                                         (> (at 'avg (at 'gas-stats result)) 25000.0))
                                       test-results)))
      (if (> (length high-gas-operations) 0)
          ["Focus on database operation optimization",
           "Implement batch processing for high-volume operations",
           "Consider implementing result caching",
           "Review algorithm choices for computational efficiency"]
          ["Performance appears optimal",
           "Continue monitoring for regression",
           "Consider stress testing with larger datasets"])))
  
  ;; ==========================================================================
  ;; AUTOMATED PERFORMANCE MONITORING
  ;; ==========================================================================
  
  (defschema monitoring-config
    enabled:bool
    sample-rate:decimal
    alert-threshold:decimal
    operations-to-monitor:[string])
  
  (deftable monitoring-configs:{monitoring-config})
  
  (defun setup-performance-monitoring:string (sample-rate:decimal alert-threshold:decimal operations:[string])
    @doc "Setup automated performance monitoring"
    (write monitoring-configs "default" {
      "enabled": true,
      "sample-rate": sample-rate,
      "alert-threshold": alert-threshold,
      "operations-to-monitor": operations
    })
    "Performance monitoring configured")
  
  (defun check-performance-alerts:[object] ()
    @doc "Check for performance alerts based on thresholds"
    (with-read monitoring-configs "default" { 
      "enabled" := enabled,
      "alert-threshold" := threshold,
      "operations-to-monitor" := monitored-ops 
    }
      (if enabled
          (let ((alerts (map (lambda (op)
                               (let ((profile (get-gas-profile op)))
                                 (if (> (at 'avg-gas profile) threshold)
                                     { "operation": op,
                                       "alert-type": "high-gas-usage",
                                       "current-avg": (at 'avg-gas profile),
                                       "threshold": threshold,
                                       "severity": (if (> (at 'avg-gas profile) (* threshold 2.0)) "critical" "warning") }
                                     {})))
                             monitored-ops)))
            (filter (lambda (alert) (!= alert {})) alerts))
          [])))
  
  ;; Query functions
  (defun get-benchmark-history:[object] (test-name:string)
    @doc "Get benchmark history for specific test"
    (select benchmark-results (where 'test-name (= test-name))))
  
  (defun get-recent-metrics:[object] (hour-count:integer)
    @doc "Get performance metrics from last N hours"
    (let ((cutoff-time (add-time (at 'block-time (chain-data)) (hours (- 0 hour-count)))))
      (select performance-metrics (where 'timestamp (> cutoff-time)))))
)

;; Initialize tables
(create-table performance-metrics)
(create-table benchmark-results)
(create-table gas-profiles)
(create-table monitoring-configs)