(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-unauthorized (err u103))

(define-non-fungible-token carbon-credit uint)

(define-map credits
  uint 
  {
    project-id: uint,
    amount: uint,
    price: uint,
    owner: principal,
    certified: bool,
    retired: bool
  }
)

(define-map projects 
  uint
  {
    name: (string-ascii 100),
    location: (string-ascii 50),
    verification-date: uint,
    total-credits: uint
  }
)

(define-data-var next-credit-id uint u1)
(define-data-var next-project-id uint u1)

(define-public (create-project (name (string-ascii 100)) (location (string-ascii 50)) (total-credits uint))
  (let ((project-id (var-get next-project-id)))
    (if (is-eq tx-sender contract-owner)
      (begin
        (map-set projects project-id {
          name: name,
          location: location,
          verification-date: stacks-block-height,
          total-credits: total-credits
        })
        (var-set next-project-id (+ project-id u1))
        (ok project-id))
      err-owner-only)))
(define-public (mint-credit (project-id uint) (amount uint) (price uint) (recipient principal))
  (let ((credit-id (var-get next-credit-id)))
    (if (is-eq tx-sender contract-owner)
      (begin
        (try! (nft-mint? carbon-credit credit-id recipient))
        (map-set credits credit-id {
          project-id: project-id,
          amount: amount,
          price: price,
          owner: recipient,
          certified: true,
          retired: false
        })
        (var-set next-credit-id (+ credit-id u1))
        (ok credit-id))
      err-owner-only)))

(define-public (transfer-credit (credit-id uint) (recipient principal))
  (let ((credit (unwrap! (map-get? credits credit-id) err-not-found)))
    (if (and 
          (is-eq (get owner credit) tx-sender)
          (not (get retired credit)))
      (begin
        (try! (nft-transfer? carbon-credit credit-id tx-sender recipient))
        (map-set credits credit-id (merge credit { owner: recipient }))
        (ok true))
      err-unauthorized)))

(define-public (retire-credit (credit-id uint))
  (let ((credit (unwrap! (map-get? credits credit-id) err-not-found)))
    (if (is-eq (get owner credit) tx-sender)
      (begin
        (map-set credits credit-id (merge credit { retired: true }))
        (ok true))
      err-unauthorized)))

(define-read-only (get-credit (credit-id uint))
  (map-get? credits credit-id))

(define-read-only (get-project (project-id uint))
  (map-get? projects project-id))

(define-read-only (get-credit-owner (credit-id uint))
  (nft-get-owner? carbon-credit credit-id))

(define-map marketplace-listings
  uint
  {
    credit-id: uint,
    seller: principal,
    price: uint,
    active: bool
  }
)

(define-data-var next-listing-id uint u1)

(define-public (list-credit-for-sale (credit-id uint) (sale-price uint))
  (let ((credit (unwrap! (map-get? credits credit-id) err-not-found))
        (listing-id (var-get next-listing-id)))
    (if (and 
          (is-eq (get owner credit) tx-sender)
          (not (get retired credit))
          (> sale-price u0))
      (begin
        (map-set marketplace-listings listing-id {
          credit-id: credit-id,
          seller: tx-sender,
          price: sale-price,
          active: true
        })
        (var-set next-listing-id (+ listing-id u1))
        (ok listing-id))
      err-unauthorized)))

(define-public (cancel-listing (listing-id uint))
  (let ((listing (unwrap! (map-get? marketplace-listings listing-id) err-not-found)))
    (if (is-eq (get seller listing) tx-sender)
      (begin
        (map-set marketplace-listings listing-id (merge listing { active: false }))
        (ok true))
      err-unauthorized)))

(define-public (purchase-credit (listing-id uint))
  (let ((listing (unwrap! (map-get? marketplace-listings listing-id) err-not-found))
        (credit-id (get credit-id listing))
        (credit (unwrap! (map-get? credits credit-id) err-not-found)))
    (if (and 
          (get active listing)
          (not (get retired credit)))
      (begin
        (try! (stx-transfer? (get price listing) tx-sender (get seller listing)))
        (try! (nft-transfer? carbon-credit credit-id (get seller listing) tx-sender))
        (map-set credits credit-id (merge credit { owner: tx-sender }))
        (map-set marketplace-listings listing-id (merge listing { active: false }))
        (ok true))
      err-unauthorized)))

(define-read-only (get-listing (listing-id uint))
  (map-get? marketplace-listings listing-id))

(define-read-only (get-active-listings-by-seller (seller principal))
  (ok seller))

  (define-private (batch-mint-helper (item {project-id: uint, amount: uint, price: uint, recipient: principal}))
  (let ((credit-id (var-get next-credit-id)))
    (begin
      (try! (nft-mint? carbon-credit credit-id (get recipient item)))
      (map-set credits credit-id {
        project-id: (get project-id item),
        amount: (get amount item),
        price: (get price item),
        owner: (get recipient item),
        certified: true,
        retired: false
      })
      (var-set next-credit-id (+ credit-id u1))
      (ok credit-id))))

(define-public (batch-mint-credits (credits-data (list 10 {project-id: uint, amount: uint, price: uint, recipient: principal})))
  (if (is-eq tx-sender contract-owner)
    (begin
      (map batch-mint-helper credits-data)
      (ok true))
    err-owner-only))

(define-private (batch-transfer-helper (item {credit-id: uint, recipient: principal}))
  (let ((credit (unwrap! (map-get? credits (get credit-id item)) err-not-found)))
    (if (and 
          (is-eq (get owner credit) tx-sender)
          (not (get retired credit)))
      (begin
        (try! (nft-transfer? carbon-credit (get credit-id item) tx-sender (get recipient item)))
        (map-set credits (get credit-id item) (merge credit { owner: (get recipient item) }))
        (ok true))
      err-unauthorized)))

(define-public (batch-transfer-credits (transfers (list 10 {credit-id: uint, recipient: principal})))
  (begin
    (map batch-transfer-helper transfers)
    (ok true)))

(define-private (batch-retire-helper (credit-id uint))
  (let ((credit (unwrap! (map-get? credits credit-id) err-not-found)))
    (if (is-eq (get owner credit) tx-sender)
      (begin
        (map-set credits credit-id (merge credit { retired: true }))
        (ok true))
      err-unauthorized)))

(define-public (batch-retire-credits (credit-ids (list 10 uint)))
  (begin
    (map batch-retire-helper credit-ids)
    (ok true)))

(define-read-only (get-credits-by-owner (owner principal))
  (ok owner))

(define-read-only (get-total-credits-by-project (project-id uint))
  (ok project-id))