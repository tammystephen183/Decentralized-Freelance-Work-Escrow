(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_PROJECT_NOT_FOUND (err u101))
(define-constant ERR_INVALID_STATUS (err u102))
(define-constant ERR_INSUFFICIENT_FUNDS (err u103))
(define-constant ERR_ALREADY_EXISTS (err u104))
(define-constant ERR_DEADLINE_PASSED (err u105))
(define-constant ERR_DEADLINE_NOT_PASSED (err u106))
(define-constant ERR_INVALID_AMOUNT (err u107))

(define-constant STATUS_CREATED u0)
(define-constant STATUS_FUNDED u1)
(define-constant STATUS_IN_PROGRESS u2)
(define-constant STATUS_SUBMITTED u3)
(define-constant STATUS_COMPLETED u4)
(define-constant STATUS_DISPUTED u5)
(define-constant STATUS_CANCELLED u6)

(define-data-var project-counter uint u0)

(define-map projects
  uint
  {
    client: principal,
    freelancer: principal,
    amount: uint,
    deadline: uint,
    status: uint,
    title: (string-ascii 100),
    description: (string-ascii 500),
    created-at: uint,
    submitted-at: (optional uint),
    completed-at: (optional uint)
  }
)

(define-map project-funds
  uint
  uint
)

(define-map user-projects
  principal
  (list 50 uint)
)

(define-map dispute-votes
  uint
  {
    client-vote: (optional bool),
    freelancer-vote: (optional bool),
    admin-decision: (optional bool)
  }
)

(define-public (create-project 
  (freelancer principal)
  (amount uint)
  (deadline uint)
  (title (string-ascii 100))
  (description (string-ascii 500)))
  (let
    (
      (project-id (+ (var-get project-counter) u1))
      (current-block stacks-block-height)
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> deadline current-block) ERR_DEADLINE_PASSED)
    (asserts! (not (is-eq tx-sender freelancer)) ERR_NOT_AUTHORIZED)
    
    (map-set projects project-id
      {
        client: tx-sender,
        freelancer: freelancer,
        amount: amount,
        deadline: deadline,
        status: STATUS_CREATED,
        title: title,
        description: description,
        created-at: current-block,
        submitted-at: none,
        completed-at: none
      }
    )
    
    (var-set project-counter project-id)
    (update-user-projects tx-sender project-id)
    (update-user-projects freelancer project-id)
    (ok project-id)
  )
)
(define-public (fund-project (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (amount (get amount project))
    )
    (asserts! (is-eq tx-sender (get client project)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status project) STATUS_CREATED) ERR_INVALID_STATUS)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set projects project-id
      (merge project { status: STATUS_FUNDED })
    )
    
    (map-set project-funds project-id amount)
    (ok true)
  )
)

(define-public (start-work (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get freelancer project)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status project) STATUS_FUNDED) ERR_INVALID_STATUS)
    (asserts! (< stacks-block-height (get deadline project)) ERR_DEADLINE_PASSED)
    
    (map-set projects project-id
      (merge project { status: STATUS_IN_PROGRESS })
    )
    (ok true)
  )
)

(define-public (submit-work (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get freelancer project)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status project) STATUS_IN_PROGRESS) ERR_INVALID_STATUS)
    
    (map-set projects project-id
      (merge project { 
        status: STATUS_SUBMITTED,
        submitted-at: (some current-block)
      })
    )
    (ok true)
  )
)

(define-public (approve-work (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (amount (unwrap! (map-get? project-funds project-id) ERR_INSUFFICIENT_FUNDS))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get client project)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status project) STATUS_SUBMITTED) ERR_INVALID_STATUS)
    
    (try! (as-contract (stx-transfer? amount tx-sender (get freelancer project))))
    
    (map-set projects project-id
      (merge project { 
        status: STATUS_COMPLETED,
        completed-at: (some current-block)
      })
    )
    
    (map-delete project-funds project-id)
    (ok true)
  )
)
(define-public (dispute-work (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
    )
    (asserts! (or 
      (is-eq tx-sender (get client project))
      (is-eq tx-sender (get freelancer project))
    ) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status project) STATUS_SUBMITTED) ERR_INVALID_STATUS)
    
    (map-set projects project-id
      (merge project { status: STATUS_DISPUTED })
    )
    
    (map-set dispute-votes project-id
      {
        client-vote: none,
        freelancer-vote: none,
        admin-decision: none
      }
    )
    (ok true)
  )
)

(define-public (vote-dispute (project-id uint) (vote-for-freelancer bool))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (current-votes (unwrap! (map-get? dispute-votes project-id) ERR_PROJECT_NOT_FOUND))
    )
    (asserts! (or 
      (is-eq tx-sender (get client project))
      (is-eq tx-sender (get freelancer project))
    ) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status project) STATUS_DISPUTED) ERR_INVALID_STATUS)
    
    (if (is-eq tx-sender (get client project))
      (map-set dispute-votes project-id
        (merge current-votes { client-vote: (some vote-for-freelancer) })
      )
      (map-set dispute-votes project-id
        (merge current-votes { freelancer-vote: (some vote-for-freelancer) })
      )
    )
    (ok true)
  )
)

(define-public (resolve-dispute (project-id uint) (favor-freelancer bool))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (amount (unwrap! (map-get? project-funds project-id) ERR_INSUFFICIENT_FUNDS))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status project) STATUS_DISPUTED) ERR_INVALID_STATUS)
    
    (if favor-freelancer
      (try! (as-contract (stx-transfer? amount tx-sender (get freelancer project))))
      (try! (as-contract (stx-transfer? amount tx-sender (get client project))))
    )
    
    (map-set projects project-id
      (merge project { 
        status: STATUS_COMPLETED,
        completed-at: (some current-block)
      })
    )
    
    (map-delete project-funds project-id)
    (ok true)
  )
)
(define-public (cancel-project (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (amount-opt (map-get? project-funds project-id))
    )
    (asserts! (is-eq tx-sender (get client project)) ERR_NOT_AUTHORIZED)
    (asserts! (or 
      (is-eq (get status project) STATUS_CREATED)
      (and 
        (is-eq (get status project) STATUS_FUNDED)
        (> stacks-block-height (get deadline project))
      )
    ) ERR_INVALID_STATUS)
    
    (match amount-opt
      amount (try! (as-contract (stx-transfer? amount tx-sender (get client project))))
      true
    )
    
    (map-set projects project-id
      (merge project { status: STATUS_CANCELLED })
    )
    
    (map-delete project-funds project-id)
    (ok true)
  )
)

(define-public (emergency-withdraw (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (amount (unwrap! (map-get? project-funds project-id) ERR_INSUFFICIENT_FUNDS))
      (deadline-passed (> stacks-block-height (+ (get deadline project) u1440)))
    )
    (asserts! (is-eq tx-sender (get freelancer project)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status project) STATUS_SUBMITTED) ERR_INVALID_STATUS)
    (asserts! deadline-passed ERR_DEADLINE_NOT_PASSED)
    
    (try! (as-contract (stx-transfer? amount tx-sender (get freelancer project))))
    
    (map-set projects project-id
      (merge project { 
        status: STATUS_COMPLETED,
        completed-at: (some stacks-block-height)
      })
    )
    
    (map-delete project-funds project-id)
    (ok true)
  )
)
(define-private (update-user-projects (user principal) (project-id uint))
  (let
    (
      (current-projects (default-to (list) (map-get? user-projects user)))
    )
    (map-set user-projects user (unwrap-panic (as-max-len? (append current-projects project-id) u50)))
  )
)

(define-read-only (get-project (project-id uint))
  (map-get? projects project-id)
)

(define-read-only (get-project-funds (project-id uint))
  (map-get? project-funds project-id)
)

(define-read-only (get-user-projects (user principal))
  (default-to (list) (map-get? user-projects user))
)

(define-read-only (get-dispute-votes (project-id uint))
  (map-get? dispute-votes project-id)
)

(define-read-only (get-project-counter)
  (var-get project-counter)
)

(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)
