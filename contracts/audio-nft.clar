;; audio-nft
;; A contract for minting and managing audio loops as NFTs on the Stacks blockchain
;; Part of the WaveLoom project - decentralized sound editing and audio marketplace

;; =====================================
;; Error Constants
;; =====================================
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-TOKEN-ID-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-MINTED (err u102))
(define-constant ERR-INVALID-METADATA (err u103))
(define-constant ERR-COLLECTION-NOT-FOUND (err u104))
(define-constant ERR-VERSION-NOT-FOUND (err u105))
(define-constant ERR-NOT-OWNER (err u106))
(define-constant ERR-TRANSFER-FAILED (err u107))

;; =====================================
;; Data Maps and Variables
;; =====================================
;; Keep track of the last token ID assigned
(define-data-var last-token-id uint u0)

;; Store token to owner mappings
(define-map token-owner 
  { token-id: uint } 
  { owner: principal })

;; Store core audio metadata
(define-map audio-metadata
  { token-id: uint }
  {
    creator: principal,
    name: (string-ascii 100),
    description: (string-ascii 500),
    created-at: uint,
    ipfs-audio-uri: (string-ascii 100),
    ipfs-thumbnail-uri: (optional (string-ascii 100))
  })

;; Store technical audio properties
(define-map audio-properties
  { token-id: uint }
  {
    track-length-seconds: uint,
    bpm: (optional uint),
    key: (optional (string-ascii 10)),
    genre: (optional (string-ascii 50)),
    instrument-tags: (optional (list 10 (string-ascii 50))),
    license-type: (string-ascii 50)
  })

;; Track collections (albums, sample packs)
(define-map collections
  { collection-id: uint }
  {
    name: (string-ascii 100),
    creator: principal,
    description: (string-ascii 500),
    created-at: uint,
    token-ids: (list 100 uint),
    ipfs-cover-art: (optional (string-ascii 100))
  })

;; Keep track of the last collection ID assigned
(define-data-var last-collection-id uint u0)

;; Track versions of audio assets
(define-map audio-versions
  { token-id: uint, version-number: uint }
  {
    ipfs-audio-uri: (string-ascii 100),
    changes: (string-ascii 200),
    timestamp: uint
  })

;; Track the current version number for each token
(define-map current-version
  { token-id: uint }
  { version-number: uint })

;; =====================================
;; Private Functions
;; =====================================
(define-private (is-token-owner (token-id uint) (user principal))
  (let ((token-owner-data (map-get? token-owner { token-id: token-id })))
    (if (is-some token-owner-data)
        (is-eq user (get owner (unwrap! token-owner-data false)))
        false)))

(define-private (transfer-token (token-id uint) (sender principal) (recipient principal))
  (let ((token-owner-data (map-get? token-owner { token-id: token-id })))
    (if (and
          (is-some token-owner-data)
          (is-eq sender (get owner (unwrap! token-owner-data false))))
        (begin
          (map-set token-owner { token-id: token-id } { owner: recipient })
          (ok true))
        ERR-TRANSFER-FAILED)))

;; =====================================
;; Read-Only Functions
;; =====================================
;; Get the owner of a token
(define-read-only (get-token-owner (token-id uint))
  (let ((owner-data (map-get? token-owner { token-id: token-id })))
    (if (is-some owner-data)
        (ok (get owner (unwrap! owner-data ERR-TOKEN-ID-NOT-FOUND)))
        ERR-TOKEN-ID-NOT-FOUND)))

;; Get the metadata for an audio token
(define-read-only (get-audio-metadata (token-id uint))
  (match (map-get? audio-metadata { token-id: token-id })
    metadata (ok metadata)
    ERR-TOKEN-ID-NOT-FOUND))

;; Get the technical properties for an audio token
(define-read-only (get-audio-properties (token-id uint))
  (match (map-get? audio-properties { token-id: token-id })
    properties (ok properties)
    ERR-TOKEN-ID-NOT-FOUND))

;; Get collection details
(define-read-only (get-collection (collection-id uint))
  (match (map-get? collections { collection-id: collection-id })
    collection (ok collection)
    ERR-COLLECTION-NOT-FOUND))

;; Get all tokens in a collection
(define-read-only (get-collection-tokens (collection-id uint))
  (match (map-get? collections { collection-id: collection-id })
    collection (ok (get token-ids collection))
    ERR-COLLECTION-NOT-FOUND))

;; Get a specific version of an audio asset
(define-read-only (get-audio-version (token-id uint) (version-number uint))
  (match (map-get? audio-versions { token-id: token-id, version-number: version-number })
    version (ok version)
    ERR-VERSION-NOT-FOUND))

;; Get the current version number for a token
(define-read-only (get-current-version-number (token-id uint))
  (match (map-get? current-version { token-id: token-id })
    version (ok (get version-number version))
    ERR-TOKEN-ID-NOT-FOUND))

;; Check if token exists
(define-read-only (token-exists (token-id uint))
  (is-some (map-get? token-owner { token-id: token-id })))

;; =====================================
;; Public Functions
;; =====================================
;; Mint a new audio NFT
(define-public (mint-audio-nft
    (name (string-ascii 100))
    (description (string-ascii 500))
    (ipfs-audio-uri (string-ascii 100))
    (ipfs-thumbnail-uri (optional (string-ascii 100)))
    (track-length-seconds uint)
    (bpm (optional uint))
    (key (optional (string-ascii 10)))
    (genre (optional (string-ascii 50)))
    (instrument-tags (optional (list 10 (string-ascii 50))))
    (license-type (string-ascii 50)))
  (let 
    ((token-id (+ u1 (var-get last-token-id)))
     (creator tx-sender)
     (timestamp (unwrap-panic (get-block-info? time (- block-height u1)))))
    ;; Increment the token ID counter
    (var-set last-token-id token-id)
    
    ;; Store token ownership
    (map-set token-owner { token-id: token-id } { owner: creator })
    
    ;; Store metadata
    (map-set audio-metadata
      { token-id: token-id }
      {
        creator: creator,
        name: name,
        description: description,
        created-at: timestamp,
        ipfs-audio-uri: ipfs-audio-uri,
        ipfs-thumbnail-uri: ipfs-thumbnail-uri
      })
    
    ;; Store audio properties
    (map-set audio-properties
      { token-id: token-id }
      {
        track-length-seconds: track-length-seconds,
        bpm: bpm,
        key: key,
        genre: genre,
        instrument-tags: instrument-tags,
        license-type: license-type
      })
      
    ;; Initialize version tracking
    (map-set audio-versions
      { token-id: token-id, version-number: u1 }
      {
        ipfs-audio-uri: ipfs-audio-uri,
        changes: "Initial version",
        timestamp: timestamp
      })
      
    (map-set current-version
      { token-id: token-id }
      { version-number: u1 })
      
    (ok token-id)))

;; Create a new collection (album, sample pack)
(define-public (create-collection
    (name (string-ascii 100))
    (description (string-ascii 500))
    (ipfs-cover-art (optional (string-ascii 100))))
  (let 
    ((collection-id (+ u1 (var-get last-collection-id)))
     (creator tx-sender)
     (timestamp (unwrap-panic (get-block-info? time (- block-height u1)))))
    ;; Increment the collection ID counter
    (var-set last-collection-id collection-id)
    
    ;; Store collection data
    (map-set collections
      { collection-id: collection-id }
      {
        name: name,
        creator: creator,
        description: description,
        created-at: timestamp,
        token-ids: (list),
        ipfs-cover-art: ipfs-cover-art
      })
      
    (ok collection-id)))

;; Add a token to a collection
(define-public (add-token-to-collection (token-id uint) (collection-id uint))
  (let ((collection-data (map-get? collections { collection-id: collection-id })))
    (if (is-some collection-data)
        (let ((unwrapped-collection (unwrap-panic collection-data)))
          ;; Check that sender is creator of the collection
          (if (is-eq tx-sender (get creator unwrapped-collection))
              ;; Check that sender owns the token
              (if (is-token-owner token-id tx-sender)
                  (let ((current-tokens (get token-ids unwrapped-collection)))
                    ;; Update the collection with the new token
                    (map-set collections
                      { collection-id: collection-id }
                      (merge unwrapped-collection { token-ids: (append current-tokens token-id) }))
                    (ok true))
                  ERR-NOT-OWNER)
              ERR-NOT-AUTHORIZED))
        ERR-COLLECTION-NOT-FOUND)))

;; Update an existing audio NFT (create a new version)
(define-public (update-audio-version
    (token-id uint)
    (ipfs-audio-uri (string-ascii 100))
    (changes (string-ascii 200)))
  (let ((token-exists (map-get? token-owner { token-id: token-id }))
        (current-ver-data (map-get? current-version { token-id: token-id })))
    (if (and (is-some token-exists) (is-some current-ver-data))
        (let ((owner (get owner (unwrap-panic token-exists)))
              (curr-version (get version-number (unwrap-panic current-ver-data)))
              (new-version (+ u1 curr-version))
              (timestamp (unwrap-panic (get-block-info? time (- block-height u1)))))
          ;; Check if sender is token owner
          (if (is-eq tx-sender owner)
              (begin
                ;; Create new version entry
                (map-set audio-versions
                  { token-id: token-id, version-number: new-version }
                  {
                    ipfs-audio-uri: ipfs-audio-uri,
                    changes: changes,
                    timestamp: timestamp
                  })
                
                ;; Update current version pointer
                (map-set current-version
                  { token-id: token-id }
                  { version-number: new-version })
                
                ;; Update the main metadata with new URI
                (match (map-get? audio-metadata { token-id: token-id })
                  metadata (map-set audio-metadata
                             { token-id: token-id }
                             (merge metadata { ipfs-audio-uri: ipfs-audio-uri }))
                  false)
                  
                (ok new-version))
              ERR-NOT-OWNER))
        ERR-TOKEN-ID-NOT-FOUND)))

;; Transfer an audio NFT to another user
(define-public (transfer (token-id uint) (recipient principal))
  (let ((result (transfer-token token-id tx-sender recipient)))
    (match result
      success (ok success)
      error error)))

;; Update metadata for an existing audio NFT
(define-public (update-metadata 
    (token-id uint)
    (name (string-ascii 100))
    (description (string-ascii 500))
    (ipfs-thumbnail-uri (optional (string-ascii 100))))
  (let ((token-data (map-get? token-owner { token-id: token-id })))
    (if (is-some token-data)
        (let ((owner (get owner (unwrap-panic token-data))))
          (if (is-eq tx-sender owner)
              (match (map-get? audio-metadata { token-id: token-id })
                metadata (begin
                           (map-set audio-metadata
                             { token-id: token-id }
                             (merge metadata { 
                               name: name,
                               description: description,
                               ipfs-thumbnail-uri: ipfs-thumbnail-uri
                             }))
                           (ok true))
                ERR-TOKEN-ID-NOT-FOUND)
              ERR-NOT-OWNER))
        ERR-TOKEN-ID-NOT-FOUND)))

;; Update technical properties for an existing audio NFT
(define-public (update-properties
    (token-id uint)
    (bpm (optional uint))
    (key (optional (string-ascii 10)))
    (genre (optional (string-ascii 50)))
    (instrument-tags (optional (list 10 (string-ascii 50))))
    (license-type (string-ascii 50)))
  (let ((token-data (map-get? token-owner { token-id: token-id })))
    (if (is-some token-data)
        (let ((owner (get owner (unwrap-panic token-data))))
          (if (is-eq tx-sender owner)
              (match (map-get? audio-properties { token-id: token-id })
                properties (begin
                             (map-set audio-properties
                               { token-id: token-id }
                               (merge properties {
                                 bpm: bpm,
                                 key: key,
                                 genre: genre,
                                 instrument-tags: instrument-tags,
                                 license-type: license-type
                               }))
                             (ok true))
                ERR-TOKEN-ID-NOT-FOUND)
              ERR-NOT-OWNER))
        ERR-TOKEN-ID-NOT-FOUND)))