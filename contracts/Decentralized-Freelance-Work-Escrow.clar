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

(define-constant MILESTONE_STATUS_CREATED u0)
(define-constant MILESTONE_STATUS_FUNDED u1)
(define-constant MILESTONE_STATUS_SUBMITTED u2)
(define-constant MILESTONE_STATUS_APPROVED u3)

(define-constant ERR_TEMPLATE_NOT_FOUND (err u108))
(define-constant ERR_TEMPLATE_EXISTS (err u109))

(define-data-var template-counter uint u0)

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

(define-map user-reputation
  principal
  {
    projects-completed: uint,
    projects-disputed: uint,
    total-projects: uint,
    on-time-deliveries: uint,
    last-activity: uint
  }
)

(define-private (update-reputation (user principal) (completed bool) (disputed bool) (on-time bool))
  (let
    (
      (current-rep (default-to 
        { projects-completed: u0, projects-disputed: u0, total-projects: u0, on-time-deliveries: u0, last-activity: u0 }
        (map-get? user-reputation user)))
      (new-completed (if completed (+ (get projects-completed current-rep) u1) (get projects-completed current-rep)))
      (new-disputed (if disputed (+ (get projects-disputed current-rep) u1) (get projects-disputed current-rep)))
      (new-total (+ (get total-projects current-rep) u1))
      (new-on-time (if on-time (+ (get on-time-deliveries current-rep) u1) (get on-time-deliveries current-rep)))
    )
    (map-set user-reputation user
      {
        projects-completed: new-completed,
        projects-disputed: new-disputed,
        total-projects: new-total,
        on-time-deliveries: new-on-time,
        last-activity: stacks-block-height
      }
    )
  )
)

(define-public (track-project-completion (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (is-on-time (match (get submitted-at project)
        submitted-block (<= submitted-block (get deadline project))
        false))
    )
    (asserts! (is-eq (get status project) STATUS_COMPLETED) ERR_INVALID_STATUS)
    
    (update-reputation (get freelancer project) true false is-on-time)
    (update-reputation (get client project) true false true)
    (ok true)
  )
)

(define-public (track-project-dispute (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
    )
    (asserts! (is-eq (get status project) STATUS_DISPUTED) ERR_INVALID_STATUS)
    
    (update-reputation (get freelancer project) false true false)
    (update-reputation (get client project) false true false)
    (ok true)
  )
)

(define-read-only (get-user-reputation (user principal))
  (map-get? user-reputation user)
)

(define-read-only (calculate-reputation-score (user principal))
  (match (map-get? user-reputation user)
    rep-data (let
      (
        (total (get total-projects rep-data))
        (completed (get projects-completed rep-data))
        (disputed (get projects-disputed rep-data))
        (on-time (get on-time-deliveries rep-data))
      )
      (if (is-eq total u0)
        (ok u0)
        (ok (/ (* (+ (* completed u60) (* on-time u30) (* (- total disputed) u10)) u100) total))
      )
    )
    (ok u0)
  )
)

(define-map project-milestones
  { project-id: uint, milestone-id: uint }
  {
    title: (string-ascii 100),
    amount: uint,
    deadline: uint,
    status: uint,
    funded-at: (optional uint),
    submitted-at: (optional uint),
    approved-at: (optional uint)
  }
)

(define-map milestone-counter uint uint)


(define-public (create-milestone 
  (project-id uint)
  (title (string-ascii 100))
  (amount uint)
  (deadline uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (current-count (default-to u0 (map-get? milestone-counter project-id)))
      (milestone-id (+ current-count u1))
    )
    (asserts! (is-eq tx-sender (get client project)) ERR_NOT_AUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> deadline stacks-block-height) ERR_DEADLINE_PASSED)
    
    (map-set project-milestones 
      { project-id: project-id, milestone-id: milestone-id }
      {
        title: title,
        amount: amount,
        deadline: deadline,
        status: MILESTONE_STATUS_CREATED,
        funded-at: none,
        submitted-at: none,
        approved-at: none
      }
    )
    
    (map-set milestone-counter project-id milestone-id)
    (ok milestone-id)
  )
)

(define-public (fund-milestone (project-id uint) (milestone-id uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (milestone (unwrap! (map-get? project-milestones { project-id: project-id, milestone-id: milestone-id }) ERR_PROJECT_NOT_FOUND))
      (amount (get amount milestone))
    )
    (asserts! (is-eq tx-sender (get client project)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status milestone) MILESTONE_STATUS_CREATED) ERR_INVALID_STATUS)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set project-milestones 
      { project-id: project-id, milestone-id: milestone-id }
      (merge milestone { 
        status: MILESTONE_STATUS_FUNDED,
        funded-at: (some stacks-block-height)
      })
    )
    (ok true)
  )
)

(define-public (submit-milestone (project-id uint) (milestone-id uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (milestone (unwrap! (map-get? project-milestones { project-id: project-id, milestone-id: milestone-id }) ERR_PROJECT_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get freelancer project)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status milestone) MILESTONE_STATUS_FUNDED) ERR_INVALID_STATUS)
    (asserts! (< stacks-block-height (get deadline milestone)) ERR_DEADLINE_PASSED)
    
    (map-set project-milestones 
      { project-id: project-id, milestone-id: milestone-id }
      (merge milestone { 
        status: MILESTONE_STATUS_SUBMITTED,
        submitted-at: (some stacks-block-height)
      })
    )
    (ok true)
  )
)

(define-public (approve-milestone (project-id uint) (milestone-id uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (milestone (unwrap! (map-get? project-milestones { project-id: project-id, milestone-id: milestone-id }) ERR_PROJECT_NOT_FOUND))
      (amount (get amount milestone))
    )
    (asserts! (is-eq tx-sender (get client project)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status milestone) MILESTONE_STATUS_SUBMITTED) ERR_INVALID_STATUS)
    
    (try! (as-contract (stx-transfer? amount tx-sender (get freelancer project))))
    
    (map-set project-milestones 
      { project-id: project-id, milestone-id: milestone-id }
      (merge milestone { 
        status: MILESTONE_STATUS_APPROVED,
        approved-at: (some stacks-block-height)
      })
    )
    (ok true)
  )
)

(define-read-only (get-milestone (project-id uint) (milestone-id uint))
  (map-get? project-milestones { project-id: project-id, milestone-id: milestone-id })
)

(define-read-only (get-milestone-count (project-id uint))
  (default-to u0 (map-get? milestone-counter project-id))
)


(define-map project-templates
  uint
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    category: (string-ascii 50),
    suggested-amount: uint,
    suggested-deadline-blocks: uint,
    usage-count: uint,
    created-at: uint,
    active: bool
  }
)

(define-map user-templates
  principal
  (list 20 uint)
)

(define-public (create-template
  (title (string-ascii 100))
  (description (string-ascii 500))
  (category (string-ascii 50))
  (suggested-amount uint)
  (suggested-deadline-blocks uint))
  (let
    (
      (template-id (+ (var-get template-counter) u1))
    )
    (asserts! (> suggested-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> suggested-deadline-blocks u0) ERR_INVALID_AMOUNT)
    
    (map-set project-templates template-id
      {
        creator: tx-sender,
        title: title,
        description: description,
        category: category,
        suggested-amount: suggested-amount,
        suggested-deadline-blocks: suggested-deadline-blocks,
        usage-count: u0,
        created-at: stacks-block-height,
        active: true
      }
    )
    
    (var-set template-counter template-id)
    (update-user-templates tx-sender template-id)
    (ok template-id)
  )
)

(define-public (create-project-from-template
  (template-id uint)
  (freelancer principal))
  (let
    (
      (template (unwrap! (map-get? project-templates template-id) ERR_TEMPLATE_NOT_FOUND))
      (deadline (+ stacks-block-height (get suggested-deadline-blocks template)))
      (project-id (+ (var-get project-counter) u1))
    )
    (asserts! (get active template) ERR_TEMPLATE_NOT_FOUND)
    (asserts! (not (is-eq tx-sender freelancer)) ERR_NOT_AUTHORIZED)
    
    (map-set projects project-id
      {
        client: tx-sender,
        freelancer: freelancer,
        amount: (get suggested-amount template),
        deadline: deadline,
        status: STATUS_CREATED,
        title: (get title template),
        description: (get description template),
        created-at: stacks-block-height,
        submitted-at: none,
        completed-at: none
      }
    )
    
    (map-set project-templates template-id
      (merge template { usage-count: (+ (get usage-count template) u1) })
    )
    
    (var-set project-counter project-id)
    (update-user-projects tx-sender project-id)
    (update-user-projects freelancer project-id)
    (ok project-id)
  )
)

(define-private (update-user-templates (user principal) (template-id uint))
  (let
    (
      (current-templates (default-to (list) (map-get? user-templates user)))
    )
    (map-set user-templates user (unwrap-panic (as-max-len? (append current-templates template-id) u20)))
  )
)

(define-read-only (get-template (template-id uint))
  (map-get? project-templates template-id)
)

(define-read-only (get-user-templates (user principal))
  (default-to (list) (map-get? user-templates user))
)

(define-read-only (get-template-counter)
  (var-get template-counter)
)