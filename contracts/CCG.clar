(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-STATUS (err u101))
(define-constant ERR-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-RESOLVED (err u103))
(define-constant ERR-EMPTY-TITLE (err u104))
(define-constant ERR-EMPTY-DESCRIPTION (err u105))
(define-constant ERR-TOO-MANY-COMPLAINTS (err u106))

(define-data-var complaint-counter uint u0)
(define-data-var contract-owner principal tx-sender)

(define-map complaints
    { id: uint }
    {
        title: (string-ascii 100),
        description: (string-ascii 500),
        status: (string-ascii 20),
        submitter: principal,
        resolver: (optional principal),
        timestamp: uint,
        resolution-notes: (optional (string-ascii 500)),
        category: (string-ascii 50),
        votes: uint,
    }
)

(define-map citizen-complaints
    principal
    (list 50 uint)
)

(define-map complaint-votes
    {
        complaint-id: uint,
        voter: principal,
    }
    { voted: bool }
)

(define-map complaint-comments
    {
        complaint-id: uint,
        comment-id: uint,
    }
    {
        author: principal,
        content: (string-ascii 200),
        timestamp: uint,
    }
)

(define-map complaint-comment-counters
    { complaint-id: uint }
    { counter: uint }
)

(define-public (submit-complaint
        (title (string-ascii 100))
        (description (string-ascii 500))
        (category (string-ascii 50))
    )
    (let (
            (complaint-id (+ (var-get complaint-counter) u1))
            (caller tx-sender)
            (user-complaints (default-to (list) (map-get? citizen-complaints caller)))
        )
        (asserts! (> (len title) u0) ERR-EMPTY-TITLE)
        (asserts! (> (len description) u0) ERR-EMPTY-DESCRIPTION)
        (asserts! (< (len user-complaints) u50) ERR-TOO-MANY-COMPLAINTS)
        (map-set complaints { id: complaint-id } {
            title: title,
            description: description,
            status: "pending",
            submitter: caller,
            resolver: none,
            timestamp: stacks-block-height,
            resolution-notes: none,
            category: category,
            votes: u0,
        })
        (map-set citizen-complaints caller
            (unwrap-panic (as-max-len? (append user-complaints complaint-id) u50))
        )
        (var-set complaint-counter complaint-id)
        (ok complaint-id)
    )
)

(define-public (resolve-complaint
        (complaint-id uint)
        (resolution-notes (string-ascii 500))
    )
    (let (
            (complaint (unwrap! (map-get? complaints { id: complaint-id }) ERR-NOT-FOUND))
            (caller tx-sender)
        )
        (asserts! (is-authorized caller) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status complaint) "pending") ERR-ALREADY-RESOLVED)
        (map-set complaints { id: complaint-id }
            (merge complaint {
                status: "resolved",
                resolver: (some caller),
                resolution-notes: (some resolution-notes),
            })
        )
        (ok true)
    )
)

(define-public (update-complaint-status
        (complaint-id uint)
        (new-status (string-ascii 20))
    )
    (let (
            (complaint (unwrap! (map-get? complaints { id: complaint-id }) ERR-NOT-FOUND))
            (caller tx-sender)
        )
        (asserts! (is-authorized caller) ERR-NOT-AUTHORIZED)
        (asserts! (is-valid-status new-status) ERR-INVALID-STATUS)
        (map-set complaints { id: complaint-id }
            (merge complaint { status: new-status })
        )
        (ok true)
    )
)

(define-public (vote-for-complaint (complaint-id uint))
    (let (
            (complaint (unwrap! (map-get? complaints { id: complaint-id }) ERR-NOT-FOUND))
            (caller tx-sender)
            (vote-key {
                complaint-id: complaint-id,
                voter: caller,
            })
            (has-voted (default-to { voted: false } (map-get? complaint-votes vote-key)))
        )
        (asserts! (not (get voted has-voted)) (err u107))
        (map-set complaint-votes vote-key { voted: true })
        (map-set complaints { id: complaint-id }
            (merge complaint { votes: (+ (get votes complaint) u1) })
        )
        (ok true)
    )
)

(define-public (add-comment
        (complaint-id uint)
        (content (string-ascii 200))
    )
    (let (
            (complaint (unwrap! (map-get? complaints { id: complaint-id }) ERR-NOT-FOUND))
            (caller tx-sender)
            (counter-key { complaint-id: complaint-id })
            (comment-counter (default-to { counter: u0 }
                (map-get? complaint-comment-counters counter-key)
            ))
            (new-comment-id (+ (get counter comment-counter) u1))
        )
        (asserts! (> (len content) u0) (err u108))
        (map-set complaint-comments {
            complaint-id: complaint-id,
            comment-id: new-comment-id,
        } {
            author: caller,
            content: content,
            timestamp: stacks-block-height,
        })
        (map-set complaint-comment-counters counter-key { counter: new-comment-id })
        (ok new-comment-id)
    )
)

(define-public (transfer-ownership (new-owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (var-set contract-owner new-owner)
        (ok true)
    )
)

(define-read-only (get-complaint (complaint-id uint))
    (ok (unwrap! (map-get? complaints { id: complaint-id }) ERR-NOT-FOUND))
)

(define-read-only (get-user-complaints (user principal))
    (ok (default-to (list) (map-get? citizen-complaints user)))
)

(define-read-only (get-total-complaints)
    (ok (var-get complaint-counter))
)

(define-read-only (get-comment
        (complaint-id uint)
        (comment-id uint)
    )
    (ok (unwrap!
        (map-get? complaint-comments {
            complaint-id: complaint-id,
            comment-id: comment-id,
        })
        ERR-NOT-FOUND
    ))
)

(define-read-only (get-comment-count (complaint-id uint))
    (ok (get counter
        (default-to { counter: u0 }
            (map-get? complaint-comment-counters { complaint-id: complaint-id })
        )))
)

(define-read-only (has-voted
        (complaint-id uint)
        (voter principal)
    )
    (ok (get voted
        (default-to { voted: false }
            (map-get? complaint-votes {
                complaint-id: complaint-id,
                voter: voter,
            })
        )))
)

(define-read-only (get-contract-owner)
    (ok (var-get contract-owner))
)

(define-private (is-authorized (user principal))
    (is-eq user (var-get contract-owner))
)

(define-private (is-valid-status (status (string-ascii 20)))
    (or
        (is-eq status "pending")
        (is-eq status "in-progress")
        (is-eq status "resolved")
        (is-eq status "rejected")
    )
)

(define-map valid-categories
    (string-ascii 50)
    { active: bool }
)

(define-public (add-category (category (string-ascii 50)))
    (begin
        (asserts! (is-authorized tx-sender) ERR-NOT-AUTHORIZED)
        (map-set valid-categories category { active: true })
        (ok true)
    )
)

(define-public (remove-category (category (string-ascii 50)))
    (begin
        (asserts! (is-authorized tx-sender) ERR-NOT-AUTHORIZED)
        (map-set valid-categories category { active: false })
        (ok true)
    )
)

(define-read-only (is-valid-category (category (string-ascii 50)))
    (get active
        (default-to { active: false } (map-get? valid-categories category))
    )
)

(define-constant PRIORITY-HIGH "high")
(define-constant PRIORITY-MEDIUM "medium")
(define-constant PRIORITY-LOW "low")

(define-map complaints-with-priority
    { id: uint }
    {
        title: (string-ascii 100),
        description: (string-ascii 500),
        status: (string-ascii 20),
        submitter: principal,
        resolver: (optional principal),
        timestamp: uint,
        resolution-notes: (optional (string-ascii 500)),
        category: (string-ascii 50),
        votes: uint,
        priority: (string-ascii 10),
    }
)

(define-public (submit-complaint-with-priority
        (title (string-ascii 100))
        (description (string-ascii 500))
        (category (string-ascii 50))
        (priority (string-ascii 10))
    )
    (let (
            (complaint-id (+ (var-get complaint-counter) u1))
            (caller tx-sender)
            (user-complaints (default-to (list) (map-get? citizen-complaints caller)))
        )
        (asserts! (> (len title) u0) ERR-EMPTY-TITLE)
        (asserts! (> (len description) u0) ERR-EMPTY-DESCRIPTION)
        (asserts! (< (len user-complaints) u50) ERR-TOO-MANY-COMPLAINTS)
        (asserts! (is-valid-priority priority) (err u110))
        (map-set complaints-with-priority { id: complaint-id } {
            title: title,
            description: description,
            status: "pending",
            submitter: caller,
            resolver: none,
            timestamp: stacks-block-height,
            resolution-notes: none,
            category: category,
            votes: u0,
            priority: priority,
        })
        (map-set citizen-complaints caller
            (unwrap-panic (as-max-len? (append user-complaints complaint-id) u50))
        )
        (var-set complaint-counter complaint-id)
        (ok complaint-id)
    )
)

(define-private (is-valid-priority (priority (string-ascii 10)))
    (or
        (is-eq priority PRIORITY-HIGH)
        (is-eq priority PRIORITY-MEDIUM)
        (is-eq priority PRIORITY-LOW)
    )
)

(define-map authorized-resolvers
    principal
    { active: bool }
)

(define-public (add-resolver (resolver principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (map-set authorized-resolvers resolver { active: true })
        (ok true)
    )
)

(define-public (remove-resolver (resolver principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (map-set authorized-resolvers resolver { active: false })
        (ok true)
    )
)

(define-constant ERR-ALREADY-ESCALATED (err u109))
(define-constant ERR-CANNOT-ESCALATE (err u110))
(define-constant ERR-INVALID-ESCALATION-LEVEL (err u111))
(define-constant ERR-ALREADY-RATED (err u112))
(define-constant ERR-INVALID-RATING (err u113))
(define-constant ERR-COMPLAINT-NOT-RESOLVED (err u114))
(define-constant ERR-NOT-COMPLAINT-SUBMITTER (err u115))

(define-constant ESCALATION-VOTE-THRESHOLD u10)
(define-constant ESCALATION-TIME-THRESHOLD u144)
(define-constant ESCALATION-LEVEL-1 "level-1")
(define-constant ESCALATION-LEVEL-2 "level-2")
(define-constant ESCALATION-LEVEL-3 "urgent")

(define-map complaint-escalations
    { complaint-id: uint }
    {
        escalation-level: (string-ascii 10),
        escalated-at: uint,
        escalated-by: principal,
        escalation-reason: (string-ascii 20),
        auto-escalated: bool,
    }
)

(define-map escalation-history
    {
        complaint-id: uint,
        escalation-id: uint,
    }
    {
        from-level: (string-ascii 10),
        to-level: (string-ascii 10),
        timestamp: uint,
        reason: (string-ascii 50),
    }
)

(define-map escalation-counters
    { complaint-id: uint }
    { counter: uint }
)

(define-public (escalate-complaint
        (complaint-id uint)
        (reason (string-ascii 20))
    )
    (let (
            (complaint (unwrap! (map-get? complaints { id: complaint-id }) ERR-NOT-FOUND))
            (caller tx-sender)
            (current-escalation (map-get? complaint-escalations { complaint-id: complaint-id }))
            (votes (get votes complaint))
            (complaint-age (- stacks-block-height (get timestamp complaint)))
        )
        (asserts! (is-none current-escalation) ERR-ALREADY-ESCALATED)
        (asserts! (is-eq (get status complaint) "pending") ERR-INVALID-STATUS)
        (asserts!
            (or
                (>= votes ESCALATION-VOTE-THRESHOLD)
                (>= complaint-age ESCALATION-TIME-THRESHOLD)
                (is-authorized caller)
            )
            ERR-CANNOT-ESCALATE
        )
        (let (
                (escalation-level (get-escalation-level votes complaint-age))
                (is-auto (not (is-authorized caller)))
            )
            (map-set complaint-escalations { complaint-id: complaint-id } {
                escalation-level: escalation-level,
                escalated-at: stacks-block-height,
                escalated-by: caller,
                escalation-reason: reason,
                auto-escalated: is-auto,
            })
            ;; (try! (record-escalation-history complaint-id "none" escalation-level reason))
            (ok escalation-level)
        )
    )
)

(define-public (update-escalation-level
        (complaint-id uint)
        (new-level (string-ascii 10))
    )
    (let (
            (complaint (unwrap! (map-get? complaints { id: complaint-id }) ERR-NOT-FOUND))
            (escalation (unwrap!
                (map-get? complaint-escalations { complaint-id: complaint-id })
                ERR-NOT-FOUND
            ))
            (caller tx-sender)
        )
        (asserts! (is-authorized caller) ERR-NOT-AUTHORIZED)
        (asserts! (is-valid-escalation-level new-level)
            ERR-INVALID-ESCALATION-LEVEL
        )
        (let ((old-level (get escalation-level escalation)))
            (map-set complaint-escalations { complaint-id: complaint-id }
                (merge escalation {
                    escalation-level: new-level,
                    escalated-at: stacks-block-height,
                    escalated-by: caller,
                    auto-escalated: false,
                })
            )
            ;; (try! (record-escalation-history complaint-id old-level new-level "manual-update"))
            (ok true)
        )
    )
)

(define-public (check-and-auto-escalate (complaint-id uint))
    (let (
            (complaint (unwrap! (map-get? complaints { id: complaint-id }) ERR-NOT-FOUND))
            (current-escalation (map-get? complaint-escalations { complaint-id: complaint-id }))
            (votes (get votes complaint))
            (complaint-age (- stacks-block-height (get timestamp complaint)))
        )
        (asserts! (is-eq (get status complaint) "pending") ERR-INVALID-STATUS)
        (if (is-none current-escalation)
            (if (or
                    (>= votes ESCALATION-VOTE-THRESHOLD)
                    (>= complaint-age ESCALATION-TIME-THRESHOLD)
                )
                (let ((escalation-level (get-escalation-level votes complaint-age)))
                    (map-set complaint-escalations { complaint-id: complaint-id } {
                        escalation-level: escalation-level,
                        escalated-at: stacks-block-height,
                        escalated-by: tx-sender,
                        escalation-reason: "auto-escalation",
                        auto-escalated: true,
                    })
                    (unwrap!
                        (record-escalation-history complaint-id "none"
                            escalation-level "auto-escalation"
                        )
                        (err u102)
                    )
                    (ok true)
                )
                (ok false)
            )
            (ok false)
        )
    )
)

(define-read-only (get-complaint-escalation (complaint-id uint))
    (ok (map-get? complaint-escalations { complaint-id: complaint-id }))
)

(define-read-only (get-escalation-history
        (complaint-id uint)
        (escalation-id uint)
    )
    (ok (map-get? escalation-history {
        complaint-id: complaint-id,
        escalation-id: escalation-id,
    }))
)

(define-read-only (get-escalation-count (complaint-id uint))
    (ok (get counter
        (default-to { counter: u0 }
            (map-get? escalation-counters { complaint-id: complaint-id })
        )))
)

(define-read-only (is-escalated (complaint-id uint))
    (ok (is-some (map-get? complaint-escalations { complaint-id: complaint-id })))
)

(define-read-only (get-escalation-thresholds)
    (ok {
        vote-threshold: ESCALATION-VOTE-THRESHOLD,
        time-threshold: ESCALATION-TIME-THRESHOLD,
    })
)

(define-private (get-escalation-level
        (votes uint)
        (age uint)
    )
    (if (>= votes (* ESCALATION-VOTE-THRESHOLD u3))
        ESCALATION-LEVEL-3
        (if (>= age (* ESCALATION-TIME-THRESHOLD u2))
            ESCALATION-LEVEL-3
            (if (>= votes (* ESCALATION-VOTE-THRESHOLD u2))
                ESCALATION-LEVEL-2
                ESCALATION-LEVEL-1
            )
        )
    )
)

(define-private (is-valid-escalation-level (level (string-ascii 10)))
    (or
        (is-eq level ESCALATION-LEVEL-1)
        (is-eq level ESCALATION-LEVEL-2)
        (is-eq level ESCALATION-LEVEL-3)
    )
)

(define-private (record-escalation-history
        (complaint-id uint)
        (from-level (string-ascii 10))
        (to-level (string-ascii 10))
        (reason (string-ascii 50))
    )
    (let (
            (counter-key { complaint-id: complaint-id })
            (current-counter (default-to { counter: u0 }
                (map-get? escalation-counters counter-key)
            ))
            (new-escalation-id (+ (get counter current-counter) u1))
        )
        (map-set escalation-history {
            complaint-id: complaint-id,
            escalation-id: new-escalation-id,
        } {
            from-level: from-level,
            to-level: to-level,
            timestamp: stacks-block-height,
            reason: reason,
        })
        (map-set escalation-counters counter-key { counter: new-escalation-id })
        (ok new-escalation-id)
    )
)

(define-map complaint-ratings
    { complaint-id: uint }
    {
        rating: uint,
        feedback: (string-ascii 300),
        submitter: principal,
        timestamp: uint,
    }
)

(define-map resolver-statistics
    principal
    {
        total-resolutions: uint,
        total-rating-points: uint,
        total-ratings: uint,
        average-rating: uint,
    }
)

(define-public (rate-complaint
        (complaint-id uint)
        (rating uint)
        (feedback (string-ascii 300))
    )
    (let (
            (complaint (unwrap! (map-get? complaints { id: complaint-id }) ERR-NOT-FOUND))
            (caller tx-sender)
            (existing-rating (map-get? complaint-ratings { complaint-id: complaint-id }))
            (resolver-opt (get resolver complaint))
        )
        (asserts! (is-eq (get submitter complaint) caller)
            ERR-NOT-COMPLAINT-SUBMITTER
        )
        (asserts! (is-eq (get status complaint) "resolved")
            ERR-COMPLAINT-NOT-RESOLVED
        )
        (asserts! (is-none existing-rating) ERR-ALREADY-RATED)
        (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-RATING)
        (map-set complaint-ratings { complaint-id: complaint-id } {
            rating: rating,
            feedback: feedback,
            submitter: caller,
            timestamp: stacks-block-height,
        })
        (match resolver-opt
            resolver-principal (begin
                (let (
                        (current-stats (default-to {
                            total-resolutions: u0,
                            total-rating-points: u0,
                            total-ratings: u0,
                            average-rating: u0,
                        }
                            (map-get? resolver-statistics resolver-principal)
                        ))
                        (new-total-ratings (+ (get total-ratings current-stats) u1))
                        (new-total-points (+ (get total-rating-points current-stats) rating))
                        (new-average (/ new-total-points new-total-ratings))
                    )
                    (map-set resolver-statistics resolver-principal {
                        total-resolutions: (+ (get total-resolutions current-stats) u1),
                        total-rating-points: new-total-points,
                        total-ratings: new-total-ratings,
                        average-rating: new-average,
                    })
                )
                (ok true)
            )
            (ok true)
        )
    )
)

(define-public (update-rating
        (complaint-id uint)
        (new-rating uint)
        (new-feedback (string-ascii 300))
    )
    (let (
            (complaint (unwrap! (map-get? complaints { id: complaint-id }) ERR-NOT-FOUND))
            (caller tx-sender)
            (existing-rating (unwrap! (map-get? complaint-ratings { complaint-id: complaint-id })
                ERR-NOT-FOUND
            ))
            (resolver-opt (get resolver complaint))
        )
        (asserts! (is-eq (get submitter existing-rating) caller)
            ERR-NOT-COMPLAINT-SUBMITTER
        )
        (asserts! (and (>= new-rating u1) (<= new-rating u5)) ERR-INVALID-RATING)
        (let ((old-rating (get rating existing-rating)))
            (map-set complaint-ratings { complaint-id: complaint-id } {
                rating: new-rating,
                feedback: new-feedback,
                submitter: caller,
                timestamp: stacks-block-height,
            })
            (match resolver-opt
                resolver-principal (begin
                    (let (
                            (current-stats (default-to {
                                total-resolutions: u0,
                                total-rating-points: u0,
                                total-ratings: u0,
                                average-rating: u0,
                            }
                                (map-get? resolver-statistics resolver-principal)
                            ))
                            (adjusted-points (+
                                (- (get total-rating-points current-stats)
                                    old-rating
                                )
                                new-rating
                            ))
                            (new-average (/ adjusted-points (get total-ratings current-stats)))
                        )
                        (map-set resolver-statistics resolver-principal
                            (merge current-stats {
                                total-rating-points: adjusted-points,
                                average-rating: new-average,
                            })
                        )
                    )
                    (ok true)
                )
                (ok true)
            )
        )
    )
)

(define-read-only (get-complaint-rating (complaint-id uint))
    (ok (map-get? complaint-ratings { complaint-id: complaint-id }))
)

(define-read-only (get-resolver-statistics (resolver principal))
    (ok (map-get? resolver-statistics resolver))
)

(define-read-only (get-resolver-average-rating (resolver principal))
    (ok (get average-rating
        (default-to {
            total-resolutions: u0,
            total-rating-points: u0,
            total-ratings: u0,
            average-rating: u0,
        }
            (map-get? resolver-statistics resolver)
        )))
)

(define-read-only (has-rating (complaint-id uint))
    (ok (is-some (map-get? complaint-ratings { complaint-id: complaint-id })))
)

(define-read-only (can-rate-complaint
        (complaint-id uint)
        (user principal)
    )
    (match (map-get? complaints { id: complaint-id })
        complaint (ok (and
            (is-eq (get submitter complaint) user)
            (is-eq (get status complaint) "resolved")
            (is-none (map-get? complaint-ratings { complaint-id: complaint-id }))
        ))
        (ok false)
    )
)


