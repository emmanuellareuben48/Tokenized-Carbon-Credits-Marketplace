(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-already-rated (err u104))
(define-constant err-invalid-rating (err u105))
(define-constant err-credit-not-retired (err u106))
(define-constant err-certificate-exists (err u107))

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

(define-map validators
  principal
  {
    registered: bool,
    reputation: uint
  }
)

(define-map credit-ratings
  { credit-id: uint, validator: principal }
  {
    score: uint,
    timestamp: uint
  }
)

(define-map credit-rating-summary
  uint
  {
    total-score: uint,
    rating-count: uint,
    average-rating: uint
  }
)

(define-map offset-certificates
  uint
  {
    credit-id: uint,
    holder: principal,
    co2-amount-tons: uint,
    issue-date: uint,
    project-name: (string-ascii 100),
    verification-hash: (buff 32)
  }
)

(define-map user-offset-totals
  principal
  {
    total-co2-offset: uint,
    certificates-count: uint,
    first-offset-date: uint
  }
)

(define-data-var next-certificate-id uint u1)

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

(define-public (register-validator)
  (begin
    (map-set validators tx-sender {
      registered: true,
      reputation: u0
    })
    (ok true)))

(define-public (rate-credit (credit-id uint) (score uint))
  (let ((credit (unwrap! (map-get? credits credit-id) err-not-found))
        (validator (unwrap! (map-get? validators tx-sender) err-unauthorized))
        (existing-rating (map-get? credit-ratings { credit-id: credit-id, validator: tx-sender }))
        (current-summary (default-to { total-score: u0, rating-count: u0, average-rating: u0 } 
                                     (map-get? credit-rating-summary credit-id))))
    (asserts! (get registered validator) err-unauthorized)
    (asserts! (and (>= score u1) (<= score u10)) err-invalid-rating)
    (asserts! (is-none existing-rating) err-already-rated)
    (begin
      (map-set credit-ratings { credit-id: credit-id, validator: tx-sender } {
        score: score,
        timestamp: stacks-block-height
      })
      (let ((new-total-score (+ (get total-score current-summary) score))
            (new-rating-count (+ (get rating-count current-summary) u1)))
        (map-set credit-rating-summary credit-id {
          total-score: new-total-score,
          rating-count: new-rating-count,
          average-rating: (/ new-total-score new-rating-count)
        }))
      (map-set validators tx-sender (merge validator { reputation: (+ (get reputation validator) u1) }))
      (ok true))))

(define-read-only (get-validator (validator-address principal))
  (map-get? validators validator-address))

(define-read-only (get-credit-rating (credit-id uint) (validator principal))
  (map-get? credit-ratings { credit-id: credit-id, validator: validator }))

(define-read-only (get-credit-average-rating (credit-id uint))
  (map-get? credit-rating-summary credit-id))

(define-read-only (is-validator-registered (validator-address principal))
  (match (map-get? validators validator-address)
    validator-data (get registered validator-data)
    false))

(define-public (generate-offset-certificate (credit-id uint))
  (let ((credit (unwrap! (map-get? credits credit-id) err-not-found))
        (project (unwrap! (map-get? projects (get project-id credit)) err-not-found))
        (certificate-id (var-get next-certificate-id))
        (current-totals (default-to 
          { total-co2-offset: u0, certificates-count: u0, first-offset-date: u0 } 
          (map-get? user-offset-totals tx-sender)))
        (verification-hash (keccak256 (concat 
          (concat (unwrap-panic (to-consensus-buff? credit-id)) (unwrap-panic (to-consensus-buff? tx-sender)))
          (unwrap-panic (to-consensus-buff? stacks-block-height))))))
    (asserts! (is-eq (get owner credit) tx-sender) err-unauthorized)
    (asserts! (get retired credit) err-credit-not-retired)
    (asserts! (is-none (map-get? offset-certificates certificate-id)) err-certificate-exists)
    (begin
      (map-set offset-certificates certificate-id {
        credit-id: credit-id,
        holder: tx-sender,
        co2-amount-tons: (get amount credit),
        issue-date: stacks-block-height,
        project-name: (get name project),
        verification-hash: verification-hash
      })
      (map-set user-offset-totals tx-sender {
        total-co2-offset: (+ (get total-co2-offset current-totals) (get amount credit)),
        certificates-count: (+ (get certificates-count current-totals) u1),
        first-offset-date: (if (is-eq (get first-offset-date current-totals) u0) 
                              stacks-block-height 
                              (get first-offset-date current-totals))
      })
      (var-set next-certificate-id (+ certificate-id u1))
      (ok certificate-id))))

(define-read-only (get-offset-certificate (certificate-id uint))
  (map-get? offset-certificates certificate-id))

(define-read-only (get-user-offset-totals (user principal))
  (map-get? user-offset-totals user))

(define-read-only (verify-certificate (certificate-id uint))
  (match (map-get? offset-certificates certificate-id)
    certificate (let ((expected-hash (keccak256 (concat 
                        (concat (unwrap-panic (to-consensus-buff? (get credit-id certificate))) (unwrap-panic (to-consensus-buff? (get holder certificate))))
                        (unwrap-panic (to-consensus-buff? (get issue-date certificate)))))))
                  (ok (is-eq (get verification-hash certificate) expected-hash)))
    err-not-found))

(define-read-only (calculate-environmental-impact (user principal))
  (match (map-get? user-offset-totals user)
    user-totals (ok (get total-co2-offset user-totals))
    (ok u0)))