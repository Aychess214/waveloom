;; marketplace.clar - Waveloom Audio NFT Marketplace
;; This contract facilitates the buying, selling, and trading of audio NFTs 
;; with flexible pricing and licensing options on the Waveloom platform.

;; ===============================
;; Error Codes
;; ===============================
(define-constant ERR-NOT-AUTHORIZED (err u1001))
(define-constant ERR-LISTING-DOES-NOT-EXIST (err u1002))
(define-constant ERR-LISTING-CLOSED (err u1003))
(define-constant ERR-ALREADY-LISTED (err u1004))
(define-constant ERR-INVALID-PRICE (err u1005))
(define-constant ERR-NOT-NFT-OWNER (err u1006))
(define-constant ERR-INSUFFICIENT-BALANCE (err u1007))
(define-constant ERR-AUCTION-IN-PROGRESS (err u1008))
(define-constant ERR-AUCTION-ENDED (err u1009))
(define-constant ERR-BID-TOO-LOW (err u1010))
(define-constant ERR-ESCROW-FAILED (err u1011))
(define-constant ERR-WITHDRAWAL-FAILED (err u1012))
(define-constant ERR-INVALID-LICENSE-TYPE (err u1013))
(define-constant ERR-OFFER-DOES-NOT-EXIST (err u1014))
(define-constant ERR-INVALID-LISTING-TYPE (err u1015))
(define-constant ERR-SELF-TRANSFER (err u1016))

;; ===============================
;; Data Structures
;; ===============================

;; License types representing different usage rights
(define-constant LICENSE-PERSONAL u1)  ;; Personal use only
(define-constant LICENSE-COMMERCIAL-LIMITED u2)  ;; Limited commercial use
(define-constant LICENSE-COMMERCIAL-FULL u3)  ;; Full commercial rights
(define-constant LICENSE-EXCLUSIVE u4)  ;; Exclusive rights (transfers full ownership)

;; Listing types
(define-constant LISTING-TYPE-FIXED-PRICE u1)
(define-constant LISTING-TYPE-AUCTION u2)

;; Data structure to track listings
;; Stores all active listings of audio NFTs
(define-map listings
  { listing-id: uint }
  {
    seller: principal,
    nft-contract: principal,
    nft-id: uint,
    price: uint,
    license-type: uint,
    listing-type: uint,
    start-block: uint,
    end-block: (optional uint),
    active: bool
  }
)

;; Data structure to track auction bids
(define-map auction-bids
  { listing-id: uint }
  {
    highest-bidder: (optional principal),
    highest-bid: uint,
    bid-count: uint
  }
)

;; Data structure to track offers for NFTs
(define-map offers
  { nft-contract: principal, nft-id: uint, buyer: principal }
  {
    amount: uint,
    license-type: uint,
    expiry-block: uint
  }
)

;; Transaction history 
(define-map transaction-history
  { tx-id: uint }
  {
    seller: principal,
    buyer: principal,
    nft-contract: principal,
    nft-id: uint,
    price: uint,
    license-type: uint,
    block-height: uint
  }
)

;; Platform fee percentage (in basis points, e.g., 250 = 2.5%)
(define-data-var platform-fee-bps uint u250)

;; Platform fee recipient
(define-data-var fee-recipient principal tx-sender)

;; Contract administrator
(define-data-var contract-admin principal tx-sender)

;; Counter for listing IDs
(define-data-var next-listing-id uint u1)

;; Counter for transaction history IDs
(define-data-var next-tx-id uint u1)

;; ===============================
;; Private Functions
;; ===============================

;; Calculate platform fee for a given amount
(define-private (calculate-fee (amount uint))
  (/ (* amount (var-get platform-fee-bps)) u10000)
)

;; Check if caller is the contract administrator
(define-private (is-admin)
  (is-eq tx-sender (var-get contract-admin))
)

;; Generate a new listing ID
(define-private (generate-listing-id)
  (let ((id (var-get next-listing-id)))
    (var-set next-listing-id (+ id u1))
    id
  )
)

;; Generate a new transaction ID
(define-private (generate-tx-id)
  (let ((id (var-get next-tx-id)))
    (var-set next-tx-id (+ id u1))
    id
  )
)

;; Validate license type
(define-private (is-valid-license-type (license-type uint))
  (or 
    (is-eq license-type LICENSE-PERSONAL)
    (is-eq license-type LICENSE-COMMERCIAL-LIMITED)
    (is-eq license-type LICENSE-COMMERCIAL-FULL)
    (is-eq license-type LICENSE-EXCLUSIVE)
  )
)

;; Validate listing type
(define-private (is-valid-listing-type (listing-type uint))
  (or 
    (is-eq listing-type LISTING-TYPE-FIXED-PRICE)
    (is-eq listing-type LISTING-TYPE-AUCTION)
  )
)

;; Record a transaction in the history
(define-private (record-transaction (seller principal) (buyer principal) (nft-contract principal) (nft-id uint) (price uint) (license-type uint))
  (let ((tx-id (generate-tx-id)))
    (map-set transaction-history
      { tx-id: tx-id }
      {
        seller: seller,
        buyer: buyer,
        nft-contract: nft-contract,
        nft-id: nft-id,
        price: price,
        license-type: license-type,
        block-height: block-height
      }
    )
    tx-id
  )
)

;; Transfer NFT from seller to buyer - handles either direct transfer or licensing
(define-private (transfer-nft (nft-contract principal) (nft-id uint) (sender principal) (recipient principal) (license-type uint))
  (if (is-eq license-type LICENSE-EXCLUSIVE)
    ;; For exclusive rights, transfer the entire NFT
    (contract-call? nft-contract transfer nft-id sender recipient)
    ;; For other license types, we're granting a license but not transferring the NFT
    ;; In a production environment, this would likely call a licensing function on the NFT contract
    (as-contract (contract-call? nft-contract grant-license nft-id recipient license-type))
  )
)

;; Process payment for a purchase, handles fee calculation and transfers
(define-private (process-payment (seller principal) (amount uint))
  (let 
    (
      (fee (calculate-fee amount))
      (seller-amount (- amount fee))
    )
    (if (> fee u0)
      ;; Transfer fee to platform
      (and
        (try! (as-contract (stx-transfer? fee tx-sender (var-get fee-recipient))))
        (as-contract (stx-transfer? seller-amount tx-sender seller))
      )
      ;; No fee, transfer full amount
      (as-contract (stx-transfer? amount tx-sender seller))
    )
  )
)

;; ===============================
;; Read-Only Functions
;; ===============================

;; Get listing details by ID
(define-read-only (get-listing (listing-id uint))
  (map-get? listings { listing-id: listing-id })
)

;; Get current auction information for a listing
(define-read-only (get-auction-info (listing-id uint))
  (map-get? auction-bids { listing-id: listing-id })
)

;; Get offer details
(define-read-only (get-offer (nft-contract principal) (nft-id uint) (buyer principal))
  (map-get? offers { nft-contract: nft-contract, nft-id: nft-id, buyer: buyer })
)

;; Get transaction details
(define-read-only (get-transaction (tx-id uint))
  (map-get? transaction-history { tx-id: tx-id })
)

;; Get current platform fee
(define-read-only (get-platform-fee)
  (var-get platform-fee-bps)
)

;; Check if a listing is currently active
(define-read-only (is-listing-active (listing-id uint))
  (match (map-get? listings { listing-id: listing-id })
    listing (and 
              (get active listing)
              (match (get end-block listing)
                end-height (< block-height end-height)
                true
              )
            )
    false
  )
)

;; ===============================
;; Public Functions
;; ===============================

;; Create a new fixed-price listing
(define-public (create-fixed-price-listing (nft-contract principal) (nft-id uint) (price uint) (license-type uint))
  (let 
    (
      (listing-id (generate-listing-id))
      (seller tx-sender)
    )
    (asserts! (> price u0) ERR-INVALID-PRICE)
    (asserts! (is-valid-license-type license-type) ERR-INVALID-LICENSE-TYPE)
    
    ;; Check that the seller owns the NFT
    (asserts! (is-eq (unwrap! (contract-call? nft-contract get-owner nft-id) ERR-NOT-NFT-OWNER) seller) ERR-NOT-NFT-OWNER)
    
    ;; For exclusive licenses, we need to verify the seller has full rights
    (if (is-eq license-type LICENSE-EXCLUSIVE)
      (asserts! (unwrap! (contract-call? nft-contract can-transfer nft-id seller) ERR-NOT-NFT-OWNER) ERR-NOT-NFT-OWNER)
      true
    )
    
    ;; Create the listing
    (map-set listings
      { listing-id: listing-id }
      {
        seller: seller,
        nft-contract: nft-contract,
        nft-id: nft-id,
        price: price,
        license-type: license-type,
        listing-type: LISTING-TYPE-FIXED-PRICE,
        start-block: block-height,
        end-block: none,
        active: true
      }
    )
    
    (ok listing-id)
  )
)

;; Create an auction listing
(define-public (create-auction-listing (nft-contract principal) (nft-id uint) (reserve-price uint) (license-type uint) (duration-blocks uint))
  (let 
    (
      (listing-id (generate-listing-id))
      (seller tx-sender)
      (end-block (+ block-height duration-blocks))
    )
    (asserts! (> reserve-price u0) ERR-INVALID-PRICE)
    (asserts! (> duration-blocks u0) ERR-INVALID-PRICE)
    (asserts! (is-valid-license-type license-type) ERR-INVALID-LICENSE-TYPE)
    
    ;; Check that the seller owns the NFT
    (asserts! (is-eq (unwrap! (contract-call? nft-contract get-owner nft-id) ERR-NOT-NFT-OWNER) seller) ERR-NOT-NFT-OWNER)
    
    ;; For exclusive licenses, we need to verify the seller has full rights
    (if (is-eq license-type LICENSE-EXCLUSIVE)
      (asserts! (unwrap! (contract-call? nft-contract can-transfer nft-id seller) ERR-NOT-NFT-OWNER) ERR-NOT-NFT-OWNER)
      true
    )
    
    ;; Create the auction listing
    (map-set listings
      { listing-id: listing-id }
      {
        seller: seller,
        nft-contract: nft-contract,
        nft-id: nft-id,
        price: reserve-price,  ;; Used as the reserve price for auctions
        license-type: license-type,
        listing-type: LISTING-TYPE-AUCTION,
        start-block: block-height,
        end-block: (some end-block),
        active: true
      }
    )
    
    ;; Initialize the auction bids
    (map-set auction-bids
      { listing-id: listing-id }
      {
        highest-bidder: none,
        highest-bid: u0,
        bid-count: u0
      }
    )
    
    (ok listing-id)
  )
)

;; Purchase a fixed-price listing
(define-public (purchase-listing (listing-id uint))
  (let 
    (
      (listing (unwrap! (map-get? listings { listing-id: listing-id }) ERR-LISTING-DOES-NOT-EXIST))
      (buyer tx-sender)
    )
    ;; Validate the purchase
    (asserts! (get active listing) ERR-LISTING-CLOSED)
    (asserts! (is-eq (get listing-type listing) LISTING-TYPE-FIXED-PRICE) ERR-INVALID-LISTING-TYPE)
    (asserts! (not (is-eq buyer (get seller listing))) ERR-SELF-TRANSFER)
    
    ;; Transfer payment to escrow
    (try! (stx-transfer? (get price listing) buyer (as-contract tx-sender)))
    
    ;; Transfer NFT or license to buyer
    (try! (as-contract (transfer-nft (get nft-contract listing) (get nft-id listing) (get seller listing) buyer (get license-type listing))))
    
    ;; Process payment to seller (including fee handling)
    (try! (process-payment (get seller listing) (get price listing)))
    
    ;; Mark listing as inactive
    (map-set listings
      { listing-id: listing-id }
      (merge listing { active: false })
    )
    
    ;; Record the transaction
    (let 
      (
        (tx-id (record-transaction 
          (get seller listing) 
          buyer 
          (get nft-contract listing) 
          (get nft-id listing) 
          (get price listing) 
          (get license-type listing)
        ))
      )
      (ok tx-id)
    )
  )
)

;; Place a bid on an auction listing
(define-public (place-bid (listing-id uint) (bid-amount uint))
  (let 
    (
      (listing (unwrap! (map-get? listings { listing-id: listing-id }) ERR-LISTING-DOES-NOT-EXIST))
      (auction-info (unwrap! (map-get? auction-bids { listing-id: listing-id }) ERR-LISTING-DOES-NOT-EXIST))
      (bidder tx-sender)
      (end-block (unwrap! (get end-block listing) ERR-LISTING-DOES-NOT-EXIST))
    )
    ;; Validate the bid
    (asserts! (get active listing) ERR-LISTING-CLOSED)
    (asserts! (is-eq (get listing-type listing) LISTING-TYPE-AUCTION) ERR-INVALID-LISTING-TYPE)
    (asserts! (< block-height end-block) ERR-AUCTION-ENDED)
    (asserts! (not (is-eq bidder (get seller listing))) ERR-SELF-TRANSFER)
    
    ;; Check if bid amount is valid
    (if (is-some (get highest-bidder auction-info))
      ;; If there's already a bid, the new bid must be higher
      (asserts! (> bid-amount (get highest-bid auction-info)) ERR-BID-TOO-LOW)
      ;; If it's the first bid, it must meet the reserve price
      (asserts! (>= bid-amount (get price listing)) ERR-BID-TOO-LOW)
    )
    
    ;; Refund the previous highest bidder if there was one
    (match (get highest-bidder auction-info)
      previous-bidder (try! (as-contract (stx-transfer? (get highest-bid auction-info) tx-sender previous-bidder)))
      true
    )
    
    ;; Transfer the bid amount to the contract escrow
    (try! (stx-transfer? bid-amount bidder (as-contract tx-sender)))
    
    ;; Update the auction information
    (map-set auction-bids
      { listing-id: listing-id }
      {
        highest-bidder: (some bidder),
        highest-bid: bid-amount,
        bid-count: (+ (get bid-count auction-info) u1)
      }
    )
    
    (ok true)
  )
)

;; Finalize an auction after it has ended
(define-public (finalize-auction (listing-id uint))
  (let 
    (
      (listing (unwrap! (map-get? listings { listing-id: listing-id }) ERR-LISTING-DOES-NOT-EXIST))
      (auction-info (unwrap! (map-get? auction-bids { listing-id: listing-id }) ERR-LISTING-DOES-NOT-EXIST))
      (end-block (unwrap! (get end-block listing) ERR-LISTING-DOES-NOT-EXIST))
    )
    ;; Validate the auction finalization
    (asserts! (get active listing) ERR-LISTING-CLOSED)
    (asserts! (is-eq (get listing-type listing) LISTING-TYPE-AUCTION) ERR-INVALID-LISTING-TYPE)
    (asserts! (>= block-height end-block) ERR-AUCTION-IN-PROGRESS)
    
    ;; Check if there were any bids
    (match (get highest-bidder auction-info)
      winner 
        (begin
          ;; Transfer NFT or license to the winner
          (try! (as-contract (transfer-nft (get nft-contract listing) (get nft-id listing) (get seller listing) winner (get license-type listing))))
          
          ;; Process payment to seller
          (try! (process-payment (get seller listing) (get highest-bid auction-info)))
          
          ;; Record the transaction
          (record-transaction 
            (get seller listing) 
            winner 
            (get nft-contract listing) 
            (get nft-id listing) 
            (get highest-bid auction-info) 
            (get license-type listing)
          )
        )
      ;; No bids, do nothing with NFT (stays with seller)
      true
    )
    
    ;; Mark listing as inactive
    (map-set listings
      { listing-id: listing-id }
      (merge listing { active: false })
    )
    
    (ok true)
  )
)

;; Make an offer for an NFT (whether listed or not)
(define-public (make-offer (nft-contract principal) (nft-id uint) (offer-amount uint) (license-type uint) (blocks-valid uint))
  (let 
    (
      (buyer tx-sender)
      (expiry-block (+ block-height blocks-valid))
    )
    (asserts! (> offer-amount u0) ERR-INVALID-PRICE)
    (asserts! (> blocks-valid u0) ERR-INVALID-PRICE)
    (asserts! (is-valid-license-type license-type) ERR-INVALID-LICENSE-TYPE)
    
    ;; Transfer funds to escrow
    (try! (stx-transfer? offer-amount buyer (as-contract tx-sender)))
    
    ;; Store the offer
    (map-set offers
      { nft-contract: nft-contract, nft-id: nft-id, buyer: buyer }
      {
        amount: offer-amount,
        license-type: license-type,
        expiry-block: expiry-block
      }
    )
    
    (ok true)
  )
)

;; Accept an offer for an NFT
(define-public (accept-offer (nft-contract principal) (nft-id uint) (buyer principal))
  (let 
    (
      (offer (unwrap! (map-get? offers { nft-contract: nft-contract, nft-id: nft-id, buyer: buyer }) ERR-OFFER-DOES-NOT-EXIST))
      (seller tx-sender)
    )
    ;; Validate the offer acceptance
    (asserts! (< block-height (get expiry-block offer)) ERR-LISTING-CLOSED)
    (asserts! (is-eq (unwrap! (contract-call? nft-contract get-owner nft-id) ERR-NOT-NFT-OWNER) seller) ERR-NOT-NFT-OWNER)
    
    ;; For exclusive licenses, verify the seller has full rights
    (if (is-eq (get license-type offer) LICENSE-EXCLUSIVE)
      (asserts! (unwrap! (contract-call? nft-contract can-transfer nft-id seller) ERR-NOT-NFT-OWNER) ERR-NOT-NFT-OWNER)
      true
    )
    
    ;; Transfer NFT or license to buyer
    (try! (as-contract (transfer-nft nft-contract nft-id seller buyer (get license-type offer))))
    
    ;; Process payment to seller
    (try! (process-payment seller (get amount offer)))
    
    ;; Record the transaction
    (let 
      (
        (tx-id (record-transaction 
          seller 
          buyer 
          nft-contract 
          nft-id 
          (get amount offer) 
          (get license-type offer)
        ))
      )
      ;; Delete the offer
      (map-delete offers { nft-contract: nft-contract, nft-id: nft-id, buyer: buyer })
      
      (ok tx-id)
    )
  )
)

;; Cancel an offer (can only be done by the offer creator)
(define-public (cancel-offer (nft-contract principal) (nft-id uint))
  (let 
    (
      (buyer tx-sender)
      (offer (unwrap! (map-get? offers { nft-contract: nft-contract, nft-id: nft-id, buyer: buyer }) ERR-OFFER-DOES-NOT-EXIST))
    )
    ;; Refund the offer amount to the buyer
    (try! (as-contract (stx-transfer? (get amount offer) tx-sender buyer)))
    
    ;; Delete the offer
    (map-delete offers { nft-contract: nft-contract, nft-id: nft-id, buyer: buyer })
    
    (ok true)
  )
)

;; Cancel a listing (can only be done by the seller)
(define-public (cancel-listing (listing-id uint))
  (let 
    (
      (listing (unwrap! (map-get? listings { listing-id: listing-id }) ERR-LISTING-DOES-NOT-EXIST))
    )
    ;; Validate the cancellation
    (asserts! (get active listing) ERR-LISTING-CLOSED)
    (asserts! (is-eq tx-sender (get seller listing)) ERR-NOT-AUTHORIZED)
    
    ;; For auctions, check if there are bids
    (if (is-eq (get listing-type listing) LISTING-TYPE-AUCTION)
      (let 
        (
          (auction-info (unwrap! (map-get? auction-bids { listing-id: listing-id }) ERR-LISTING-DOES-NOT-EXIST))
        )
        ;; If there are bids, refund the highest bidder
        (match (get highest-bidder auction-info)
          bidder (try! (as-contract (stx-transfer? (get highest-bid auction-info) tx-sender bidder)))
          true
        )
      )
      true
    )
    
    ;; Mark listing as inactive
    (map-set listings
      { listing-id: listing-id }
      (merge listing { active: false })
    )
    
    (ok true)
  )
)

;; Update platform fee (admin only)
(define-public (set-platform-fee (new-fee-bps uint))
  (begin
    (asserts! (is-admin) ERR-NOT-AUTHORIZED)
    (asserts! (<= new-fee-bps u1000) ERR-INVALID-PRICE)  ;; Max 10% fee
    (var-set platform-fee-bps new-fee-bps)
    (ok true)
  )
)

;; Update fee recipient (admin only)
(define-public (set-fee-recipient (new-recipient principal))
  (begin
    (asserts! (is-admin) ERR-NOT-AUTHORIZED)
    (var-set fee-recipient new-recipient)
    (ok true)
  )
)

;; Transfer admin rights to a new address
(define-public (transfer-admin (new-admin principal))
  (begin
    (asserts! (is-admin) ERR-NOT-AUTHORIZED)
    (var-set contract-admin new-admin)
    (ok true)
  )
)