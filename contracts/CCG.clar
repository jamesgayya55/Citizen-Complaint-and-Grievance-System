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
(define-constant ERR-DEPARTMENT-NOT-FOUND (err u116))
(define-constant ERR-ALREADY-ASSIGNED (err u117))
(define-constant ERR-NOT-ASSIGNED (err u118))
(define-constant ERR-INVALID-DEPARTMENT (err u119))
(define-constant ERR-DEPARTMENT-INACTIVE (err u120))

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

;; =================
;; ASSIGNMENT SYSTEM
;; =================

;; Department registry for organizational structure
(define-map departments
    (string-ascii 50)
    {
        name: (string-ascii 100),
        head: principal,
        active: bool,
        created-at: uint,
    }
)

;; Complaint assignments to departments and individuals
(define-map complaint-assignments
    { complaint-id: uint }
    {
        assigned-to: principal,
        department: (string-ascii 50),
        assigned-by: principal,
        assigned-at: uint,
        notes: (optional (string-ascii 300)),
        status: (string-ascii 20), ;; "assigned", "accepted", "working", "completed"
    }
)

;; Assignment history for tracking changes
(define-map assignment-history
    {
        complaint-id: uint,
        assignment-id: uint,
    }
    {
        previous-assignee: (optional principal),
        new-assignee: principal,
        department: (string-ascii 50),
        changed-by: principal,
        changed-at: uint,
        reason: (string-ascii 200),
    }
)

;; Assignment counters for history tracking
(define-map assignment-counters
    { complaint-id: uint }
    { counter: uint }
)

;; Department workload tracking
(define-map department-workload
    (string-ascii 50)
    {
        total-assigned: uint,
        active-complaints: uint,
        completed-complaints: uint,
        avg-resolution-time: uint,
    }
)

;; Individual assignee workload tracking
(define-map assignee-workload
    principal
    {
        total-assigned: uint,
        active-complaints: uint,
        completed-complaints: uint,
        current-department: (optional (string-ascii 50)),
    }
)

;; Register a new department
(define-public (register-department
        (dept-code (string-ascii 50))
        (name (string-ascii 100))
        (head principal)
    )
    (begin
        (asserts! (is-authorized tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (> (len dept-code) u0) ERR-INVALID-DEPARTMENT)
        (asserts! (> (len name) u0) ERR-INVALID-DEPARTMENT)

        (map-set departments dept-code {
            name: name,
            head: head,
            active: true,
            created-at: stacks-block-height,
        })

        ;; Initialize workload tracking
        (map-set department-workload dept-code {
            total-assigned: u0,
            active-complaints: u0,
            completed-complaints: u0,
            avg-resolution-time: u0,
        })

        (ok true)
    )
)

;; Assign complaint to department and individual
(define-public (assign-complaint
        (complaint-id uint)
        (assignee principal)
        (department (string-ascii 50))
        (notes (optional (string-ascii 300)))
    )
    (let (
            (complaint (unwrap! (map-get? complaints { id: complaint-id }) ERR-NOT-FOUND))
            (dept-info (unwrap! (map-get? departments department) ERR-DEPARTMENT-NOT-FOUND))
            (existing-assignment (map-get? complaint-assignments { complaint-id: complaint-id }))
            (caller tx-sender)
        )
        ;; Validation checks
        (asserts! (is-authorized caller) ERR-NOT-AUTHORIZED)
        (asserts! (get active dept-info) ERR-DEPARTMENT-INACTIVE)
        (asserts! (is-none existing-assignment) ERR-ALREADY-ASSIGNED)
        (asserts! (not (is-eq (get status complaint) "resolved"))
            ERR-ALREADY-RESOLVED
        )

        ;; Create assignment
        (map-set complaint-assignments { complaint-id: complaint-id } {
            assigned-to: assignee,
            department: department,
            assigned-by: caller,
            assigned-at: stacks-block-height,
            notes: notes,
            status: "assigned",
        })

        ;; Update workload counters
        (let (
                (dept-workload-data (default-to {
                    total-assigned: u0,
                    active-complaints: u0,
                    completed-complaints: u0,
                    avg-resolution-time: u0,
                }
                    (map-get? department-workload department)
                ))
                (assignee-workload-data (default-to {
                    total-assigned: u0,
                    active-complaints: u0,
                    completed-complaints: u0,
                    current-department: none,
                }
                    (map-get? assignee-workload assignee)
                ))
            )
            ;; Update department workload
            (map-set department-workload department
                (merge dept-workload-data {
                    total-assigned: (+ (get total-assigned dept-workload-data) u1),
                    active-complaints: (+ (get active-complaints dept-workload-data) u1),
                })
            )

            ;; Update assignee workload  
            (map-set assignee-workload assignee
                (merge assignee-workload-data {
                    total-assigned: (+ (get total-assigned assignee-workload-data) u1),
                    active-complaints: (+ (get active-complaints assignee-workload-data) u1),
                    current-department: (some department),
                })
            )
        )

        ;; Record in assignment history
        (unwrap-panic (record-assignment-change complaint-id none assignee department
            "initial-assignment"
        ))

        (ok true)
    )
)

;; Reassign complaint to different assignee/department
(define-public (reassign-complaint
        (complaint-id uint)
        (new-assignee principal)
        (new-department (string-ascii 50))
        (reason (string-ascii 200))
    )
    (let (
            (complaint (unwrap! (map-get? complaints { id: complaint-id }) ERR-NOT-FOUND))
            (assignment (unwrap!
                (map-get? complaint-assignments { complaint-id: complaint-id })
                ERR-NOT-ASSIGNED
            ))
            (dept-info (unwrap! (map-get? departments new-department)
                ERR-DEPARTMENT-NOT-FOUND
            ))
            (caller tx-sender)
        )
        ;; Validation
        (asserts! (is-authorized caller) ERR-NOT-AUTHORIZED)
        (asserts! (get active dept-info) ERR-DEPARTMENT-INACTIVE)
        (asserts! (not (is-eq (get status complaint) "resolved"))
            ERR-ALREADY-RESOLVED
        )

        (let (
                (old-assignee (get assigned-to assignment))
                (old-department (get department assignment))
            )
            ;; Update assignment
            (map-set complaint-assignments { complaint-id: complaint-id }
                (merge assignment {
                    assigned-to: new-assignee,
                    department: new-department,
                    assigned-by: caller,
                    assigned-at: stacks-block-height,
                    status: "assigned",
                })
            )

            ;; Update old department workload (decrease)
            (let ((old-dept-workload (default-to {
                    total-assigned: u0,
                    active-complaints: u0,
                    completed-complaints: u0,
                    avg-resolution-time: u0,
                }
                    (map-get? department-workload old-department)
                )))
                (map-set department-workload old-department
                    (merge old-dept-workload { active-complaints: (if (> (get active-complaints old-dept-workload) u0)
                        (- (get active-complaints old-dept-workload) u1)
                        u0
                    ) }
                    ))
            )

            ;; Update new department workload (increase)
            (let ((new-dept-workload (default-to {
                    total-assigned: u0,
                    active-complaints: u0,
                    completed-complaints: u0,
                    avg-resolution-time: u0,
                }
                    (map-get? department-workload new-department)
                )))
                (map-set department-workload new-department
                    (merge new-dept-workload {
                        total-assigned: (+ (get total-assigned new-dept-workload) u1),
                        active-complaints: (+ (get active-complaints new-dept-workload) u1),
                    })
                )
            )

            ;; Update assignee workloads
            (let (
                    (old-assignee-workload-data (default-to {
                        total-assigned: u0,
                        active-complaints: u0,
                        completed-complaints: u0,
                        current-department: none,
                    }
                        (map-get? assignee-workload old-assignee)
                    ))
                    (new-assignee-workload-data (default-to {
                        total-assigned: u0,
                        active-complaints: u0,
                        completed-complaints: u0,
                        current-department: none,
                    }
                        (map-get? assignee-workload new-assignee)
                    ))
                )
                ;; Decrease old assignee workload
                (map-set assignee-workload old-assignee
                    (merge old-assignee-workload-data { active-complaints: (if (> (get active-complaints old-assignee-workload-data) u0)
                        (- (get active-complaints old-assignee-workload-data) u1)
                        u0
                    ) }
                    ))

                ;; Increase new assignee workload
                (map-set assignee-workload new-assignee
                    (merge new-assignee-workload-data {
                        total-assigned: (+ (get total-assigned new-assignee-workload-data) u1),
                        active-complaints: (+ (get active-complaints new-assignee-workload-data) u1),
                        current-department: (some new-department),
                    })
                )
            )

            ;; Record reassignment in history
            (unwrap-panic (record-assignment-change complaint-id (some old-assignee)
                new-assignee new-department reason
            ))

            (ok true)
        )
    )
)

;; Update assignment status (for assignees to update their progress)
(define-public (update-assignment-status
        (complaint-id uint)
        (new-status (string-ascii 20))
    )
    (let (
            (assignment (unwrap!
                (map-get? complaint-assignments { complaint-id: complaint-id })
                ERR-NOT-ASSIGNED
            ))
            (caller tx-sender)
        )
        ;; Allow assignee or authorized personnel to update status
        (asserts!
            (or
                (is-eq (get assigned-to assignment) caller)
                (is-authorized caller)
            )
            ERR-NOT-AUTHORIZED
        )

        (asserts! (is-valid-assignment-status new-status) ERR-INVALID-STATUS)

        (map-set complaint-assignments { complaint-id: complaint-id }
            (merge assignment { status: new-status })
        )

        (ok true)
    )
)

;; Deactivate a department
(define-public (deactivate-department (dept-code (string-ascii 50)))
    (let ((dept-info (unwrap! (map-get? departments dept-code) ERR-DEPARTMENT-NOT-FOUND)))
        (asserts! (is-authorized tx-sender) ERR-NOT-AUTHORIZED)

        (map-set departments dept-code (merge dept-info { active: false }))
        (ok true)
    )
)

;; Read-only functions for querying assignments

(define-read-only (get-complaint-assignment (complaint-id uint))
    (ok (map-get? complaint-assignments { complaint-id: complaint-id }))
)

(define-read-only (get-department-info (dept-code (string-ascii 50)))
    (ok (map-get? departments dept-code))
)

(define-read-only (get-department-workload (dept-code (string-ascii 50)))
    (ok (map-get? department-workload dept-code))
)

(define-read-only (get-assignee-workload (assignee principal))
    (ok (map-get? assignee-workload assignee))
)

(define-read-only (get-assignment-history
        (complaint-id uint)
        (assignment-id uint)
    )
    (ok (map-get? assignment-history {
        complaint-id: complaint-id,
        assignment-id: assignment-id,
    }))
)

(define-read-only (get-assignment-count (complaint-id uint))
    (ok (get counter
        (default-to { counter: u0 }
            (map-get? assignment-counters { complaint-id: complaint-id })
        )))
)

(define-read-only (is-complaint-assigned (complaint-id uint))
    (ok (is-some (map-get? complaint-assignments { complaint-id: complaint-id })))
)

;; Private helper functions

(define-private (is-valid-assignment-status (status (string-ascii 20)))
    (or
        (is-eq status "assigned")
        (is-eq status "accepted")
        (is-eq status "working")
        (is-eq status "completed")
    )
)

(define-private (record-assignment-change
        (complaint-id uint)
        (previous-assignee (optional principal))
        (new-assignee principal)
        (department (string-ascii 50))
        (reason (string-ascii 200))
    )
    (let (
            (counter-key { complaint-id: complaint-id })
            (current-counter (default-to { counter: u0 }
                (map-get? assignment-counters counter-key)
            ))
            (new-assignment-id (+ (get counter current-counter) u1))
        )
        ;; Record the change
        (map-set assignment-history {
            complaint-id: complaint-id,
            assignment-id: new-assignment-id,
        } {
            previous-assignee: previous-assignee,
            new-assignee: new-assignee,
            department: department,
            changed-by: tx-sender,
            changed-at: stacks-block-height,
            reason: reason,
        })

        ;; Update counter
        (map-set assignment-counters counter-key { counter: new-assignment-id })

        (ok new-assignment-id)
    )
)


