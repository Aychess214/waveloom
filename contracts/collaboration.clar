;; collaboration.clar
;; A smart contract that enables collaborative creation and shared ownership of audio assets
;; among multiple creators on the WaveLoom platform.
;;
;; This contract provides functionality for:
;; - Creating collaborative projects with multiple contributors
;; - Managing ownership rights and contribution tracking
;; - Handling proposal and approval workflows
;; - Setting and distributing royalties among collaborators
;; - Voting on creative decisions

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROJECT-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-COLLABORATOR (err u102))
(define-constant ERR-COLLABORATOR-NOT-FOUND (err u103))
(define-constant ERR-INVALID-CONTRIBUTION-VALUE (err u104))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u105))
(define-constant ERR-ALREADY-VOTED (err u106))
(define-constant ERR-INSUFFICIENT-VOTES (err u107))
(define-constant ERR-PROPOSAL-EXPIRED (err u108))
(define-constant ERR-ROYALTY-SUM-INVALID (err u109))
(define-constant ERR-PROJECT-ALREADY-EXISTS (err u110))
(define-constant ERR-PROJECT-LOCKED (err u111))

;; Data structures

;; Collaborative project data
(define-map projects
  { project-id: uint }
  {
    name: (string-ascii 100),
    description: (string-utf8 500),
    created-at: uint,
    owner: principal,
    locked: bool,
    total-contributors: uint
  }
)

;; Tracks the collaborators for each project
(define-map collaborators
  { project-id: uint, collaborator: principal }
  {
    role: (string-ascii 50),
    contribution-percent: uint,
    joined-at: uint,
    status: (string-ascii 20) ;; "active", "pending", "removed"
  }
)

;; Maps for collaborator proposals
(define-map collaboration-proposals
  { proposal-id: uint }
  {
    project-id: uint,
    proposer: principal,
    candidate: principal,
    role: (string-ascii 50),
    contribution-percent: uint,
    description: (string-utf8 500),
    created-at: uint,
    expires-at: uint,
    status: (string-ascii 20) ;; "pending", "approved", "rejected", "expired"
  }
)

;; Tracks votes on proposals
(define-map proposal-votes
  { proposal-id: uint, voter: principal }
  {
    vote: bool,
    voted-at: uint
  }
)

;; Tracks royalty distribution for each project
(define-map royalty-splits
  { project-id: uint }
  {
    splits: (list 20 { collaborator: principal, percentage: uint }),
    last-updated: uint
  }
)

;; Counters for IDs
(define-data-var next-project-id uint u1)
(define-data-var next-proposal-id uint u1)

;; Private functions

;; Get the current block height as a proxy for time
(define-private (get-block-height)
  block-height
)

;; Check if a principal is a collaborator on a project
(define-private (is-collaborator (project-id uint) (user principal))
  (match (map-get? collaborators { project-id: project-id, collaborator: user })
    collab (and (is-eq (get status collab) "active") true)
    false
  )
)

;; Check if a principal is the owner of a project
(define-private (is-owner (project-id uint) (user principal))
  (match (map-get? projects { project-id: project-id })
    project (is-eq (get owner project) user)
    false
  )
)

;; Calculate the total votes for a proposal
(define-private (count-votes (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? collaboration-proposals { proposal-id: proposal-id }) u0))
    (project-id (get project-id proposal))
    (project (unwrap! (map-get? projects { project-id: project-id }) u0))
    (total-contributors (get total-contributors project))
  )
    (fold count-votes-reducer
      (project-keys-owned project-id)
      { yes-votes: u0, no-votes: u0, total-possible: total-contributors })
  )
)

;; Helper for count-votes to accumulate votes
(define-private (count-votes-reducer (voter-data { project-id: uint, collaborator: principal }) (result { yes-votes: uint, no-votes: uint, total-possible: uint }))
  (match (map-get? proposal-votes { proposal-id: proposal-id, voter: (get collaborator voter-data) })
    vote-info 
      (if (get vote vote-info)
        (merge result { yes-votes: (+ (get yes-votes result) u1) })
        (merge result { no-votes: (+ (get no-votes result) u1) }))
    result
  )
)

;; Get a list of all collaborator keys for a project (for iterating)
(define-private (project-keys-owned (project-id uint))
  (map-get? collaborators-by-project { project-id: project-id })
)

;; Map to store lists of collaborators by project for iteration
(define-map collaborators-by-project
  { project-id: uint }
  (list 20 { project-id: uint, collaborator: principal })
)

;; Add a collaborator to the project's collaborator list
(define-private (add-collaborator-to-list (project-id uint) (user principal))
  (let (
    (current-list (default-to (list) (map-get? collaborators-by-project { project-id: project-id })))
  )
    (map-set collaborators-by-project
      { project-id: project-id }
      (append current-list { project-id: project-id, collaborator: user })
    )
  )
)

;; Validate royalty percentages add up to 100%
(define-private (validate-royalty-splits (splits (list 20 { collaborator: principal, percentage: uint })))
  (is-eq (fold + (map get-percentage splits) u0) u100)
)

;; Helper to extract percentage from split structure
(define-private (get-percentage (split { collaborator: principal, percentage: uint }))
  (get percentage split)
)

;; Read-only functions

;; Get project details
(define-read-only (get-project (project-id uint))
  (map-get? projects { project-id: project-id })
)

;; Get collaborator details
(define-read-only (get-collaborator (project-id uint) (user principal))
  (map-get? collaborators { project-id: project-id, collaborator: user })
)

;; Get proposal details
(define-read-only (get-proposal (proposal-id uint))
  (map-get? collaboration-proposals { proposal-id: proposal-id })
)

;; Get royalty distribution for a project
(define-read-only (get-royalty-splits (project-id uint))
  (map-get? royalty-splits { project-id: project-id })
)

;; Check if a proposal has enough votes to pass
(define-read-only (can-proposal-pass (proposal-id uint))
  (let (
    (vote-counts (count-votes proposal-id))
    (yes-votes (get yes-votes vote-counts))
    (total-contributors (get total-possible vote-counts))
    (threshold (/ (+ total-contributors u1) u2)) ;; Simple majority
  )
    (> yes-votes threshold)
  )
)

;; Public functions

;; Create a new collaborative project
(define-public (create-project (name (string-ascii 100)) (description (string-utf8 500)))
  (let (
    (project-id (var-get next-project-id))
    (caller tx-sender)
  )
    ;; Create the project
    (map-set projects
      { project-id: project-id }
      {
        name: name,
        description: description,
        created-at: (get-block-height),
        owner: caller,
        locked: false,
        total-contributors: u1
      }
    )
    
    ;; Add the creator as the first collaborator
    (map-set collaborators
      { project-id: project-id, collaborator: caller }
      {
        role: "creator",
        contribution-percent: u100,
        joined-at: (get-block-height),
        status: "active"
      }
    )
    
    ;; Initialize the collaborator list
    (add-collaborator-to-list project-id caller)
    
    ;; Set initial royalty split (100% to creator)
    (map-set royalty-splits
      { project-id: project-id }
      {
        splits: (list { collaborator: caller, percentage: u100 }),
        last-updated: (get-block-height)
      }
    )
    
    ;; Increment the project ID counter
    (var-set next-project-id (+ project-id u1))
    
    (ok project-id)
  )
)

;; Propose adding a new collaborator
(define-public (propose-collaborator 
  (project-id uint) 
  (candidate principal) 
  (role (string-ascii 50)) 
  (contribution-percent uint) 
  (description (string-utf8 500))
  (expires-in uint)
)
  (let (
    (caller tx-sender)
    (proposal-id (var-get next-proposal-id))
    (current-time (get-block-height))
    (expiration (+ current-time expires-in))
  )
    ;; Check if project exists
    (asserts! (is-some (map-get? projects { project-id: project-id })) ERR-PROJECT-NOT-FOUND)
    
    ;; Check if caller is a collaborator
    (asserts! (is-collaborator project-id caller) ERR-NOT-AUTHORIZED)
    
    ;; Check if the project is locked
    (asserts! (not (get locked (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))) ERR-PROJECT-LOCKED)
    
    ;; Check if candidate is already a collaborator
    (asserts! (not (is-collaborator project-id candidate)) ERR-ALREADY-COLLABORATOR)
    
    ;; Validate contribution percentage
    (asserts! (and (>= contribution-percent u1) (<= contribution-percent u100)) ERR-INVALID-CONTRIBUTION-VALUE)
    
    ;; Create the proposal
    (map-set collaboration-proposals
      { proposal-id: proposal-id }
      {
        project-id: project-id,
        proposer: caller,
        candidate: candidate,
        role: role,
        contribution-percent: contribution-percent,
        description: description,
        created-at: current-time,
        expires-at: expiration,
        status: "pending"
      }
    )
    
    ;; Auto-vote yes for the proposer
    (map-set proposal-votes
      { proposal-id: proposal-id, voter: caller }
      {
        vote: true,
        voted-at: current-time
      }
    )
    
    ;; Increment the proposal ID counter
    (var-set next-proposal-id (+ proposal-id u1))
    
    (ok proposal-id)
  )
)

;; Vote on a collaboration proposal
(define-public (vote-on-proposal (proposal-id uint) (approve bool))
  (let (
    (caller tx-sender)
    (proposal (unwrap! (map-get? collaboration-proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
    (project-id (get project-id proposal))
    (current-time (get-block-height))
  )
    ;; Check if caller is a collaborator on the project
    (asserts! (is-collaborator project-id caller) ERR-NOT-AUTHORIZED)
    
    ;; Check if proposal is still pending
    (asserts! (is-eq (get status proposal) "pending") ERR-PROPOSAL-NOT-FOUND)
    
    ;; Check if proposal has not expired
    (asserts! (<= current-time (get expires-at proposal)) ERR-PROPOSAL-EXPIRED)
    
    ;; Check if caller has not already voted
    (asserts! (is-none (map-get? proposal-votes { proposal-id: proposal-id, voter: caller })) ERR-ALREADY-VOTED)
    
    ;; Record the vote
    (map-set proposal-votes
      { proposal-id: proposal-id, voter: caller }
      {
        vote: approve,
        voted-at: current-time
      }
    )
    
    (ok true)
  )
)

;; Finalize a proposal (add collaborator if approved)
(define-public (finalize-proposal (proposal-id uint))
  (let (
    (caller tx-sender)
    (proposal (unwrap! (map-get? collaboration-proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
    (project-id (get project-id proposal))
    (candidate (get candidate proposal))
    (current-time (get-block-height))
    (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
  )
    ;; Check if caller is a collaborator or the proposal has expired
    (asserts! (or 
      (is-collaborator project-id caller) 
      (>= current-time (get expires-at proposal))
    ) ERR-NOT-AUTHORIZED)
    
    ;; Check if proposal is still pending
    (asserts! (is-eq (get status proposal) "pending") ERR-PROPOSAL-NOT-FOUND)
    
    ;; Check if the project is not locked
    (asserts! (not (get locked project)) ERR-PROJECT-LOCKED)
    
    (if (and 
          (< current-time (get expires-at proposal))
          (can-proposal-pass proposal-id)
        )
      ;; If proposal passed, add the collaborator
      (begin
        ;; Update the proposal status
        (map-set collaboration-proposals
          { proposal-id: proposal-id }
          (merge proposal { status: "approved" })
        )
        
        ;; Add collaborator
        (map-set collaborators
          { project-id: project-id, collaborator: candidate }
          {
            role: (get role proposal),
            contribution-percent: (get contribution-percent proposal),
            joined-at: current-time,
            status: "active"
          }
        )
        
        ;; Add to project's collaborator list
        (add-collaborator-to-list project-id candidate)
        
        ;; Update total contributors
        (map-set projects
          { project-id: project-id }
          (merge project { total-contributors: (+ (get total-contributors project) u1) })
        )
        
        ;; Recalculate royalty splits (simplified for now - equal distribution)
        (ok true)
      )
      ;; If proposal failed or expired
      (begin
        (map-set collaboration-proposals
          { proposal-id: proposal-id }
          (merge proposal { 
            status: (if (>= current-time (get expires-at proposal)) 
                      "expired" 
                      "rejected") 
          })
        )
        (ok false)
      )
    )
  )
)

;; Update royalty splits for a project
(define-public (update-royalty-splits (project-id uint) (new-splits (list 20 { collaborator: principal, percentage: uint })))
  (let (
    (caller tx-sender)
    (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
  )
    ;; Check if caller is the project owner
    (asserts! (is-eq (get owner project) caller) ERR-NOT-AUTHORIZED)
    
    ;; Check if the project is not locked
    (asserts! (not (get locked project)) ERR-PROJECT-LOCKED)
    
    ;; Validate that percentages add up to 100%
    (asserts! (validate-royalty-splits new-splits) ERR-ROYALTY-SUM-INVALID)
    
    ;; Validate all collaborators exist
    (asserts! (fold check-all-collaborators new-splits { project-id: project-id, valid: true }) ERR-COLLABORATOR-NOT-FOUND)
    
    ;; Update the royalty splits
    (map-set royalty-splits
      { project-id: project-id }
      {
        splits: new-splits,
        last-updated: (get-block-height)
      }
    )
    
    (ok true)
  )
)

;; Helper to check all collaborators exist
(define-private (check-all-collaborators 
  (split { collaborator: principal, percentage: uint }) 
  (state { project-id: uint, valid: bool })
)
  (let (
    (project-id (get project-id state))
    (valid (get valid state))
  )
    (if valid
      { 
        project-id: project-id, 
        valid: (is-some (map-get? collaborators { project-id: project-id, collaborator: (get collaborator split) })) 
      }
      state
    )
  )
)

;; Lock a project to prevent further modifications
(define-public (lock-project (project-id uint))
  (let (
    (caller tx-sender)
    (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
  )
    ;; Check if caller is the project owner
    (asserts! (is-eq (get owner project) caller) ERR-NOT-AUTHORIZED)
    
    ;; Update the project to locked status
    (map-set projects
      { project-id: project-id }
      (merge project { locked: true })
    )
    
    (ok true)
  )
)

;; Remove a collaborator (only project owner can do this)
(define-public (remove-collaborator (project-id uint) (collaborator-to-remove principal))
  (let (
    (caller tx-sender)
    (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
  )
    ;; Check if caller is the project owner
    (asserts! (is-eq (get owner project) caller) ERR-NOT-AUTHORIZED)
    
    ;; Check if the project is not locked
    (asserts! (not (get locked project)) ERR-PROJECT-LOCKED)
    
    ;; Check if target is a collaborator
    (asserts! (is-collaborator project-id collaborator-to-remove) ERR-COLLABORATOR-NOT-FOUND)
    
    ;; Cannot remove the project owner
    (asserts! (not (is-eq collaborator-to-remove (get owner project))) ERR-NOT-AUTHORIZED)
    
    ;; Update collaborator status to "removed"
    (map-set collaborators
      { project-id: project-id, collaborator: collaborator-to-remove }
      (merge (unwrap! (map-get? collaborators { project-id: project-id, collaborator: collaborator-to-remove }) ERR-COLLABORATOR-NOT-FOUND)
        { status: "removed" })
    )
    
    ;; Update total contributors
    (map-set projects
      { project-id: project-id }
      (merge project { total-contributors: (- (get total-contributors project) u1) })
    )
    
    (ok true)
  )
)