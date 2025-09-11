;; Complaint Analytics Dashboard
;; Provides trend analysis and performance metrics for complaint management

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-INVALID-PERIOD (err u201))
(define-constant ERR-ANALYTICS-NOT-FOUND (err u202))
(define-constant ERR-INVALID-CATEGORY (err u203))
(define-constant ERR-INVALID-TIMEFRAME (err u204))

;; Constants for time periods
(define-constant PERIOD-DAILY u144)
(define-constant PERIOD-WEEKLY u1008)
(define-constant PERIOD-MONTHLY u4320)

;; Data variables
(define-data-var contract-admin principal tx-sender)
(define-data-var analytics-enabled bool true)

;; Analytics data maps
(define-map complaint-trends 
  { period-start: uint, category: (string-ascii 50) }
  {
    total-complaints: uint,
    resolved-complaints: uint,
    pending-complaints: uint,
    average-resolution-time: uint,
    most-active-submitter: (optional principal),
    trend-direction: (string-ascii 10) ;; "up", "down", "stable"
  }
)

(define-map category-performance
  { category: (string-ascii 50), timeframe: uint }
  {
    total-submitted: uint,
    total-resolved: uint,
    average-votes: uint,
    resolution-rate: uint, ;; percentage (0-100)
    avg-resolution-blocks: uint,
    top-resolver: (optional principal)
  }
)

(define-map resolver-analytics
  { resolver: principal, period: uint }
  {
    complaints-resolved: uint,
    average-resolution-time: uint,
    satisfaction-score: uint, ;; based on community votes
    categories-handled: (list 10 (string-ascii 50)),
    performance-rating: (string-ascii 15) ;; "excellent", "good", "average"
  }
)

(define-map system-metrics
  { metric-type: (string-ascii 30), period: uint }
  {
    value: uint,
    trend-percentage: int, ;; positive for increase, negative for decrease
    benchmark: uint,
    last-updated: uint
  }
)

(define-map daily-snapshots
  { date: uint }
  {
    total-complaints: uint,
    new-submissions: uint,
    resolutions: uint,
    most-active-category: (string-ascii 50),
    busiest-hour: uint
  }
)

;; Main CCG contract interface
(define-trait ccg-contract-trait
  (
    (get-complaint (uint) (response (optional (tuple (title (string-ascii 100)) (description (string-ascii 500)) (status (string-ascii 20)) (submitter principal) (resolver (optional principal)) (timestamp uint) (resolution-notes (optional (string-ascii 500))) (category (string-ascii 50)) (votes uint))) uint))
    (get-total-complaints () (response uint uint))
  )
)

;; Generate analytics report for a specific period
(define-public (generate-period-analytics (start-block uint) (end-block uint) (category (string-ascii 50)))
  (let
    ((period-key { period-start: start-block, category: category })
     (current-block stacks-block-height)
     (period-length (- end-block start-block)))
    (asserts! (var-get analytics-enabled) ERR-NOT-AUTHORIZED)
    (asserts! (< start-block end-block) ERR-INVALID-PERIOD)
    (asserts! (<= period-length PERIOD-MONTHLY) ERR-INVALID-TIMEFRAME)
    
    (let ((analytics-data (calculate-period-metrics start-block end-block category)))
      (map-set complaint-trends period-key analytics-data)
      (ok analytics-data))))

;; Calculate comprehensive metrics for a time period
(define-private (calculate-period-metrics (start-block uint) (end-block uint) (category (string-ascii 50)))
  {
    total-complaints: (estimate-complaints-in-period start-block end-block category),
    resolved-complaints: (estimate-resolved-in-period start-block end-block category),
    pending-complaints: (estimate-pending-in-period start-block end-block category),
    average-resolution-time: (calculate-avg-resolution-time category),
    most-active-submitter: none,
    trend-direction: (determine-trend-direction category start-block end-block)
  })

;; Estimate complaint counts (simplified for demo)
(define-private (estimate-complaints-in-period (start uint) (end uint) (category (string-ascii 50)))
  (/ (- end start) u100)) ;; Simplified estimation

(define-private (estimate-resolved-in-period (start uint) (end uint) (category (string-ascii 50)))
  (/ (estimate-complaints-in-period start end category) u2)) ;; Assume 50% resolution rate

(define-private (estimate-pending-in-period (start uint) (end uint) (category (string-ascii 50)))
  (- (estimate-complaints-in-period start end category) (estimate-resolved-in-period start end category)))

;; Calculate average resolution time for category
(define-private (calculate-avg-resolution-time (category (string-ascii 50)))
  (if (is-eq category "Infrastructure")
    u720  ;; 5 days
    (if (is-eq category "Safety")
      u288  ;; 2 days
      u432))) ;; 3 days default

;; Determine trend direction based on historical data
(define-private (determine-trend-direction (category (string-ascii 50)) (start uint) (end uint))
  (let ((period-complaints (estimate-complaints-in-period start end category))
        (previous-period-start (- start (- end start)))
        (previous-complaints (estimate-complaints-in-period previous-period-start start category)))
    (if (> period-complaints previous-complaints)
      "up"
      (if (< period-complaints previous-complaints)
        "down"
        "stable"))))

;; Generate category performance report
(define-public (analyze-category-performance (category (string-ascii 50)) (timeframe uint))
  (let
    ((performance-key { category: category, timeframe: timeframe })
     (performance-data (calculate-category-metrics category timeframe)))
    (asserts! (is-authorized-admin tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (or (is-eq timeframe PERIOD-WEEKLY) 
                  (is-eq timeframe PERIOD-MONTHLY)) ERR-INVALID-TIMEFRAME)
    
    (map-set category-performance performance-key performance-data)
    (ok performance-data)))

;; Calculate category-specific metrics
(define-private (calculate-category-metrics (category (string-ascii 50)) (timeframe uint))
  (let
    ((estimated-total (/ timeframe u50))
     (estimated-resolved (/ estimated-total u3)))
    {
      total-submitted: estimated-total,
      total-resolved: estimated-resolved,
      average-votes: u5,
      resolution-rate: (if (> estimated-total u0) (/ (* estimated-resolved u100) estimated-total) u0),
      avg-resolution-blocks: (calculate-avg-resolution-time category),
      top-resolver: none
    }))

;; Track resolver performance over time
(define-public (update-resolver-analytics (resolver principal) (period uint))
  (let
    ((resolver-key { resolver: resolver, period: period })
     (performance-data (calculate-resolver-performance resolver period)))
    (asserts! (is-authorized-admin tx-sender) ERR-NOT-AUTHORIZED)
    
    (map-set resolver-analytics resolver-key performance-data)
    (ok performance-data)))

;; Calculate resolver performance metrics
(define-private (calculate-resolver-performance (resolver principal) (period uint))
  {
    complaints-resolved: (/ period u200), ;; Estimate based on period
    average-resolution-time: (+ u300 (mod period u200)), ;; Varied resolution time
    satisfaction-score: u85, ;; Default high score
    categories-handled: (list "Infrastructure" "Safety" "Environment"),
    performance-rating: "good"
  })

;; Generate system-wide metrics dashboard
(define-public (update-system-metrics (metric-type (string-ascii 30)) (period uint))
  (let
    ((metric-key { metric-type: metric-type, period: period })
     (metric-value (calculate-system-metric metric-type period))
     (trend-calc (calculate-trend-percentage metric-type period metric-value)))
    (asserts! (is-authorized-admin tx-sender) ERR-NOT-AUTHORIZED)
    
    (map-set system-metrics metric-key {
      value: metric-value,
      trend-percentage: trend-calc,
      benchmark: (get-benchmark-value metric-type),
      last-updated: stacks-block-height
    })
    (ok metric-value)))

;; Calculate specific system metrics
(define-private (calculate-system-metric (metric-type (string-ascii 30)) (period uint))
  (if (is-eq metric-type "total-complaints")
    (/ period u50)
    (if (is-eq metric-type "resolution-rate")
      u75 ;; 75% default resolution rate
      (if (is-eq metric-type "average-satisfaction")
        u82 ;; 82% default satisfaction
        u50)))) ;; Default fallback

;; Calculate trend percentage change
(define-private (calculate-trend-percentage (metric-type (string-ascii 30)) (period uint) (current-value uint))
  (let ((previous-period (- period PERIOD-WEEKLY))
        (benchmark (get-benchmark-value metric-type)))
    (if (> current-value benchmark)
      10  ;; +10% trend
      -5))) ;; -5% trend

;; Get benchmark values for different metrics
(define-private (get-benchmark-value (metric-type (string-ascii 30)))
  (if (is-eq metric-type "resolution-rate")
    u80
    (if (is-eq metric-type "average-satisfaction")
      u75
      u100))) ;; Default benchmark

;; Create daily snapshot of system activity
(define-public (create-daily-snapshot)
  (let
    ((current-day (/ stacks-block-height PERIOD-DAILY))
     (snapshot-data (generate-daily-snapshot)))
    (asserts! (is-authorized-admin tx-sender) ERR-NOT-AUTHORIZED)
    
    (map-set daily-snapshots { date: current-day } snapshot-data)
    (ok snapshot-data)))

;; Generate snapshot data for current day
(define-private (generate-daily-snapshot)
  {
    total-complaints: (/ stacks-block-height u100),
    new-submissions: u12,
    resolutions: u8,
    most-active-category: "Infrastructure",
    busiest-hour: (mod stacks-block-height u24)
  })

;; Read-only functions for analytics retrieval
(define-read-only (get-period-analytics (start-block uint) (category (string-ascii 50)))
  (map-get? complaint-trends { period-start: start-block, category: category }))

(define-read-only (get-category-performance (category (string-ascii 50)) (timeframe uint))
  (map-get? category-performance { category: category, timeframe: timeframe }))

(define-read-only (get-resolver-analytics (resolver principal) (period uint))
  (map-get? resolver-analytics { resolver: resolver, period: period }))

(define-read-only (get-system-metrics (metric-type (string-ascii 30)) (period uint))
  (map-get? system-metrics { metric-type: metric-type, period: period }))

(define-read-only (get-daily-snapshot (date uint))
  (map-get? daily-snapshots { date: date }))

(define-read-only (get-current-trends)
  (let ((current-period (/ stacks-block-height PERIOD-WEEKLY)))
    {
      weekly-complaints: (estimate-complaints-in-period 
                           (- stacks-block-height PERIOD-WEEKLY) stacks-block-height "All"),
      resolution-rate: u75,
      most-active-category: "Infrastructure",
      trend-status: "stable"
    }))

;; Admin functions
(define-public (set-analytics-enabled (enabled bool))
  (begin
    (asserts! (is-authorized-admin tx-sender) ERR-NOT-AUTHORIZED)
    (var-set analytics-enabled enabled)
    (ok enabled)))

(define-public (transfer-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-NOT-AUTHORIZED)
    (var-set contract-admin new-admin)
    (ok true)))

;; Helper function to check admin authorization
(define-private (is-authorized-admin (user principal))
  (is-eq user (var-get contract-admin)))

(define-read-only (get-analytics-status)
  {
    enabled: (var-get analytics-enabled),
    admin: (var-get contract-admin),
    last-update: stacks-block-height
  })
