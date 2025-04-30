;; royalty-distributor.clar

;; ===============================================
;; ROYALTY DISTRIBUTOR CONTRACT
;; ===============================================
;; This contract manages royalty payments for audio assets in the WaveLoom ecosystem.
;; It handles the distribution of funds from primary and secondary sales to creators
;; and collaborators based on predetermined royalty splits.
;; 
;; The contract ensures transparent and automatic royalty payments, creating
;; a sustainable ecosystem where creators benefit from their work over time.
;; ===============================================

;; ===============================================
;; Error Constants
;; ===============================================
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_ROYALTY_PERCENTAGE (err u101))
(define-constant ERR_INVALID_COLLABORATOR (err u102))
(define-constant ERR_ASSET_ALREADY_EXISTS (err u103))
(define-constant ERR_ASSET_NOT_FOUND (err u104))
(define-constant ERR_PERCENTAGE_SUM_NOT_100 (err u105))
(define-constant ERR_COLLABORATOR_ALREADY_EXISTS (err u106))
(define-constant ERR_COLLABORATOR_NOT_FOUND (err u107))
(define-constant ERR_PAYMENT_FAILED (err u108))
(define-constant ERR_ZERO_AMOUNT (err u109))
(define-constant ERR_MAX_COLLABORATORS_REACHED (err u110))

;; ===============================================
;; Constants
;; ===============================================
(define-constant MAX_COLLABORATORS u10)
(define-constant PERCENTAGE_PRECISION u100) ;; Percentages are stored with 2 decimal places (e.g., 12.34% = 1234)

;; ===============================================
;; Data Maps and Variables
;; ===============================================

;; Maps asset IDs to their primary creator address
(define-map asset-creators { asset-id: uint } { creator: principal })

;; Maps asset IDs to secondary market royalty percentage (in basis points - out of 10000)
;; Example: 1000 = 10%, 250 = 2.5%
(define-map asset-royalty-rates { asset-id: uint } { rate: uint })

;; Tracks royalty collaborators and their percentage splits for each asset
;; Each collaborator gets a percentage of the total royalty amount
(define-map asset-collaborators 
  { asset-id: uint, collaborator: principal } 
  { percentage: uint })

;; Keeps track of all collaborators for each asset to enable iteration
(define-map asset-collaborator-list
  { asset-id: uint }
  { collaborators: (list 10 principal) })

;; Tracks total royalties earned by each creator across all assets
(define-map creator-earnings { creator: principal } { total-earned: uint })

;; ===============================================
;; Private Functions
;; ===============================================

;; Verifies that the sum of all collaborator percentages equals 100% (10000 basis points)
(define-private (verify-percentage-sum (asset-id uint))
  (let ((collaborators (get collaborators (default-to { collaborators: (list) } (map-get? asset-collaborator-list { asset-id: asset-id }))))
        (total-percentage (fold check-percentage-sum collaborators u0)))
    (is-eq total-percentage PERCENTAGE_PRECISION)))

(define-private (check-percentage-sum (collaborator principal) (current-sum uint))
  (let ((collab-percentage (get percentage (default-to { percentage: u0 } 
                                (map-get? asset-collaborators { asset-id: asset-id, collaborator: collaborator })))))
    (+ current-sum collab-percentage)))

;; Calculates a royalty amount based on a percentage
(define-private (calculate-royalty-amount (sale-amount uint) (rate uint))
  (/ (* sale-amount rate) u10000))

;; Distributes royalty to a single collaborator
(define-private (pay-collaborator (collaborator principal) (asset-id uint) (royalty-amount uint))
  (let ((collaborator-info (default-to { percentage: u0 } 
                              (map-get? asset-collaborators { asset-id: asset-id, collaborator: collaborator })))
        (collaborator-percentage (get percentage collaborator-info))
        (collaborator-amount (/ (* royalty-amount collaborator-percentage) PERCENTAGE_PRECISION))
        (current-earnings (default-to { total-earned: u0 } (map-get? creator-earnings { creator: collaborator }))))
    
    ;; Only attempt to transfer if amount is greater than zero
    (if (> collaborator-amount u0)
        (begin
          ;; Update total earnings for this creator
          (map-set creator-earnings 
            { creator: collaborator } 
            { total-earned: (+ (get total-earned current-earnings) collaborator-amount) })
          
          ;; Perform STX transfer to collaborator
          (stx-transfer? collaborator-amount tx-sender collaborator))
        true)))

;; Helper to distribute royalties to all collaborators for an asset
(define-private (distribute-royalties-to-collaborators (asset-id uint) (royalty-amount uint))
  (let ((collaborator-list (get collaborators (default-to { collaborators: (list) } 
                              (map-get? asset-collaborator-list { asset-id: asset-id })))))
    (fold distribute-to-single-collaborator collaborator-list true)))

(define-private (distribute-to-single-collaborator (collaborator principal) (previous-result bool))
  (if previous-result
      (pay-collaborator collaborator asset-id royalty-amount)
      false))

;; ===============================================
;; Read-Only Functions
;; ===============================================

;; Get the primary creator of an asset
(define-read-only (get-asset-creator (asset-id uint))
  (get creator (default-to { creator: tx-sender } (map-get? asset-creators { asset-id: asset-id }))))

;; Get the royalty rate for secondary sales of an asset
(define-read-only (get-asset-royalty-rate (asset-id uint))
  (get rate (default-to { rate: u0 } (map-get? asset-royalty-rates { asset-id: asset-id }))))

;; Get the percentage allocation for a specific collaborator
(define-read-only (get-collaborator-percentage (asset-id uint) (collaborator principal))
  (get percentage (default-to { percentage: u0 } 
    (map-get? asset-collaborators { asset-id: asset-id, collaborator: collaborator }))))

;; Get all collaborators for a specific asset
(define-read-only (get-asset-collaborators (asset-id uint))
  (get collaborators (default-to { collaborators: (list) } (map-get? asset-collaborator-list { asset-id: asset-id }))))

;; Get total earnings for a creator across all assets
(define-read-only (get-creator-earnings (creator principal))
  (get total-earned (default-to { total-earned: u0 } (map-get? creator-earnings { creator: creator }))))

;; ===============================================
;; Public Functions
;; ===============================================

;; Register a new audio asset with its royalty information
(define-public (register-asset (asset-id uint) (royalty-rate uint))
  (begin
    ;; Check if asset already exists
    (asserts! (is-none (map-get? asset-creators { asset-id: asset-id })) ERR_ASSET_ALREADY_EXISTS)
    
    ;; Validate royalty rate (max 50% or 5000 basis points)
    (asserts! (<= royalty-rate u5000) ERR_INVALID_ROYALTY_PERCENTAGE)
    
    ;; Set the creator as the tx-sender
    (map-set asset-creators { asset-id: asset-id } { creator: tx-sender })
    
    ;; Set the royalty rate
    (map-set asset-royalty-rates { asset-id: asset-id } { rate: royalty-rate })
    
    ;; Initialize the collaborator list with just the creator at 100%
    (map-set asset-collaborators 
      { asset-id: asset-id, collaborator: tx-sender } 
      { percentage: PERCENTAGE_PRECISION })
    
    (map-set asset-collaborator-list
      { asset-id: asset-id }
      { collaborators: (list tx-sender) })
    
    (ok asset-id)))

;; Add a collaborator with a specific percentage split
(define-public (add-collaborator (asset-id uint) (collaborator principal) (percentage uint))
  (let ((asset-creator (get-asset-creator asset-id))
        (collaborator-list (get-asset-collaborators asset-id)))
    
    ;; Verify the caller is the asset creator
    (asserts! (is-eq tx-sender asset-creator) ERR_UNAUTHORIZED)
    
    ;; Verify the collaborator isn't already registered
    (asserts! (is-none (map-get? asset-collaborators { asset-id: asset-id, collaborator: collaborator })) 
              ERR_COLLABORATOR_ALREADY_EXISTS)
    
    ;; Verify percentage is not zero and within limits
    (asserts! (and (> percentage u0) (<= percentage PERCENTAGE_PRECISION)) ERR_INVALID_ROYALTY_PERCENTAGE)
    
    ;; Verify we're not exceeding max collaborators
    (asserts! (< (len collaborator-list) MAX_COLLABORATORS) ERR_MAX_COLLABORATORS_REACHED)
    
    ;; Add collaborator to the map
    (map-set asset-collaborators 
      { asset-id: asset-id, collaborator: collaborator } 
      { percentage: percentage })
    
    ;; Add to the collaborator list
    (map-set asset-collaborator-list
      { asset-id: asset-id }
      { collaborators: (append collaborator-list collaborator) })
    
    ;; Note: At this point, the percentage sum may not be 100%. The creator needs to adjust other percentages.
    (ok true)))

;; Update a collaborator's percentage split
(define-public (update-collaborator-percentage (asset-id uint) (collaborator principal) (percentage uint))
  (let ((asset-creator (get-asset-creator asset-id)))
    
    ;; Verify the caller is the asset creator
    (asserts! (is-eq tx-sender asset-creator) ERR_UNAUTHORIZED)
    
    ;; Verify the collaborator exists
    (asserts! (is-some (map-get? asset-collaborators { asset-id: asset-id, collaborator: collaborator })) 
              ERR_COLLABORATOR_NOT_FOUND)
    
    ;; Verify percentage is within limits
    (asserts! (and (>= percentage u0) (<= percentage PERCENTAGE_PRECISION)) ERR_INVALID_ROYALTY_PERCENTAGE)
    
    ;; Update collaborator's percentage
    (map-set asset-collaborators 
      { asset-id: asset-id, collaborator: collaborator } 
      { percentage: percentage })
    
    (ok true)))

;; Remove a collaborator
(define-public (remove-collaborator (asset-id uint) (collaborator principal))
  (let ((asset-creator (get-asset-creator asset-id))
        (collaborator-list (get-asset-collaborators asset-id)))
    
    ;; Verify the caller is the asset creator
    (asserts! (is-eq tx-sender asset-creator) ERR_UNAUTHORIZED)
    
    ;; Verify the collaborator exists
    (asserts! (is-some (map-get? asset-collaborators { asset-id: asset-id, collaborator: collaborator })) 
              ERR_COLLABORATOR_NOT_FOUND)
    
    ;; Verify we're not removing the creator
    (asserts! (not (is-eq collaborator asset-creator)) ERR_UNAUTHORIZED)
    
    ;; Remove collaborator from the map
    (map-delete asset-collaborators { asset-id: asset-id, collaborator: collaborator })
    
    ;; Update the collaborator list (filter out the removed collaborator)
    (map-set asset-collaborator-list
      { asset-id: asset-id }
      { collaborators: (filter filter-collaborator collaborator-list) })
    
    (ok true)))

(define-private (filter-collaborator (collab principal))
  (not (is-eq collab collaborator)))

;; Process royalty payment for a sale (primary or secondary)
(define-public (process-royalty-payment (asset-id uint) (sale-amount uint))
  (let ((royalty-rate (get-asset-royalty-rate asset-id))
        (royalty-amount (calculate-royalty-amount sale-amount royalty-rate)))
    
    ;; Verify the asset exists
    (asserts! (is-some (map-get? asset-creators { asset-id: asset-id })) ERR_ASSET_NOT_FOUND)
    
    ;; Verify non-zero amount
    (asserts! (> sale-amount u0) ERR_ZERO_AMOUNT)
    
    ;; Distribute royalties to all collaborators
    (asserts! (distribute-royalties-to-collaborators asset-id royalty-amount) ERR_PAYMENT_FAILED)
    
    (ok royalty-amount)))

;; Verify that all collaborator percentages add up to exactly 100%
(define-public (verify-collaborator-percentages (asset-id uint))
  (begin
    ;; Verify the asset exists
    (asserts! (is-some (map-get? asset-creators { asset-id: asset-id })) ERR_ASSET_NOT_FOUND)
    
    ;; Check if percentages add up to 100%
    (asserts! (verify-percentage-sum asset-id) ERR_PERCENTAGE_SUM_NOT_100)
    
    (ok true)))