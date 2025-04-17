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
        votes: uint
    }
)

(define-map citizen-complaints
    principal
    (list 50 uint)
)

(define-map complaint-votes
    { complaint-id: uint, voter: principal }
    { voted: bool }
)

(define-map complaint-comments
    { complaint-id: uint, comment-id: uint }
    {
        author: principal,
        content: (string-ascii 200),
        timestamp: uint
    }
)

(define-map complaint-comment-counters
    { complaint-id: uint }
    { counter: uint }
)

(define-public (submit-complaint (title (string-ascii 100)) (description (string-ascii 500)) (category (string-ascii 50)))
    (let
        (
            (complaint-id (+ (var-get complaint-counter) u1))
            (caller tx-sender)
            (user-complaints (default-to (list) (map-get? citizen-complaints caller)))
        )
        (asserts! (> (len title) u0) ERR-EMPTY-TITLE)
        (asserts! (> (len description) u0) ERR-EMPTY-DESCRIPTION)
        (asserts! (< (len user-complaints) u50) ERR-TOO-MANY-COMPLAINTS)
        
        (map-set complaints
            { id: complaint-id }
            { 
                title: title,
                description: description,
                status: "pending",
                submitter: caller,
                resolver: none,
                timestamp: stacks-block-height,
                resolution-notes: none,
                category: category,
                votes: u0
            }
        )
        
        (map-set citizen-complaints
            caller
            (unwrap-panic (as-max-len? (append user-complaints complaint-id) u50))
        )
        
        (var-set complaint-counter complaint-id)
        (ok complaint-id)
    )
)

(define-public (resolve-complaint (complaint-id uint) (resolution-notes (string-ascii 500)))
    (let
        (
            (complaint (unwrap! (map-get? complaints {id: complaint-id}) ERR-NOT-FOUND))
            (caller tx-sender)
        )
        (asserts! (is-authorized caller) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status complaint) "pending") ERR-ALREADY-RESOLVED)
        
        (map-set complaints
            { id: complaint-id }
            (merge complaint {
                status: "resolved",
                resolver: (some caller),
                resolution-notes: (some resolution-notes)
            })
        )
        (ok true)
    )
)

(define-public (update-complaint-status (complaint-id uint) (new-status (string-ascii 20)))
    (let
        (
            (complaint (unwrap! (map-get? complaints {id: complaint-id}) ERR-NOT-FOUND))
            (caller tx-sender)
        )
        (asserts! (is-authorized caller) ERR-NOT-AUTHORIZED)
        (asserts! (is-valid-status new-status) ERR-INVALID-STATUS)
        
        (map-set complaints
            { id: complaint-id }
            (merge complaint {
                status: new-status
            })
        )
        (ok true)
    )
)

(define-public (vote-for-complaint (complaint-id uint))
    (let
        (
            (complaint (unwrap! (map-get? complaints {id: complaint-id}) ERR-NOT-FOUND))
            (caller tx-sender)
            (vote-key { complaint-id: complaint-id, voter: caller })
            (has-voted (default-to { voted: false } (map-get? complaint-votes vote-key)))
        )
        (asserts! (not (get voted has-voted)) (err u107))
        
        (map-set complaint-votes
            vote-key
            { voted: true }
        )
        
        (map-set complaints
            { id: complaint-id }
            (merge complaint {
                votes: (+ (get votes complaint) u1)
            })
        )
        (ok true)
    )
)

(define-public (add-comment (complaint-id uint) (content (string-ascii 200)))
    (let
        (
            (complaint (unwrap! (map-get? complaints {id: complaint-id}) ERR-NOT-FOUND))
            (caller tx-sender)
            (counter-key { complaint-id: complaint-id })
            (comment-counter (default-to { counter: u0 } (map-get? complaint-comment-counters counter-key)))
            (new-comment-id (+ (get counter comment-counter) u1))
        )
        (asserts! (> (len content) u0) (err u108))
        
        (map-set complaint-comments
            { complaint-id: complaint-id, comment-id: new-comment-id }
            {
                author: caller,
                content: content,
                timestamp: stacks-block-height
            }
        )
        
        (map-set complaint-comment-counters
            counter-key
            { counter: new-comment-id }
        )
        
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
    (ok (unwrap! (map-get? complaints {id: complaint-id}) ERR-NOT-FOUND))
)

(define-read-only (get-user-complaints (user principal))
    (ok (default-to (list) (map-get? citizen-complaints user)))
)

(define-read-only (get-total-complaints)
    (ok (var-get complaint-counter))
)

(define-read-only (get-comment (complaint-id uint) (comment-id uint))
    (ok (unwrap! (map-get? complaint-comments { complaint-id: complaint-id, comment-id: comment-id }) ERR-NOT-FOUND))
)

(define-read-only (get-comment-count (complaint-id uint))
    (ok (get counter (default-to { counter: u0 } (map-get? complaint-comment-counters { complaint-id: complaint-id }))))
)

(define-read-only (has-voted (complaint-id uint) (voter principal))
    (ok (get voted (default-to { voted: false } (map-get? complaint-votes { complaint-id: complaint-id, voter: voter }))))
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
