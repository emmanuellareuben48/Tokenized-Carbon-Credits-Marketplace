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

;; ============================================================================
;; CARBON CREDIT AUCTION SYSTEM
;; ============================================================================

;; Auction-specific error constants
(define-constant err-auction-not-found (err u200))
(define-constant err-auction-expired (err u201))
(define-constant err-auction-not-expired (err u202))
(define-constant err-bid-too-low (err u203))
(define-constant err-self-bid (err u204))
(define-constant err-auction-not-active (err u205))
(define-constant err-no-bids (err u206))
(define-constant err-refund-failed (err u207))
(define-constant err-buyback-disabled (err u208))
(define-constant err-insufficient-treasury (err u209))
(define-constant err-buyback-not-available (err u210))

;; Treasury and buyback data structures
(define-data-var treasury-balance uint u0)
(define-data-var buyback-active bool false)
(define-data-var buyback-price-per-credit uint u0)
(define-data-var total-buybacks uint u0)

(define-map buyback-history
  uint
  {
    seller: principal,
    credit-id: uint,
    buyback-price: uint,
    timestamp: uint
  }
)

(define-data-var next-buyback-id uint u1)

;; Initialize buyback program
(define-public (initialize-buyback-program (initial-price uint))
  (if (is-eq tx-sender contract-owner)
    (begin
      (var-set buyback-active true)
      (var-set buyback-price-per-credit initial-price)
      (ok true))
    err-owner-only))

;; Deposit STX to treasury
(define-public (deposit-to-treasury (amount uint))
  (if (is-eq tx-sender contract-owner)
    (begin
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      (var-set treasury-balance (+ (var-get treasury-balance) amount))
      (ok (var-get treasury-balance)))
    err-owner-only))

;; Update buyback price
(define-public (update-buyback-price (new-price uint))
  (if (is-eq tx-sender contract-owner)
    (begin
      (var-set buyback-price-per-credit new-price)
      (ok true))
    err-owner-only))

;; Sell credit back to treasury
(define-public (sell-to-buyback (credit-id uint))
  (let ((credit (unwrap! (map-get? credits credit-id) err-not-found))
        (buyback-id (var-get next-buyback-id))
        (buyback-payout (var-get buyback-price-per-credit)))
    (asserts! (var-get buyback-active) err-buyback-disabled)
    (asserts! (is-eq (get owner credit) tx-sender) err-unauthorized)
    (asserts! (not (get retired credit)) err-credit-not-retired)
    (asserts! (> buyback-payout u0) err-buyback-not-available)
    (asserts! (>= (var-get treasury-balance) buyback-payout) err-insufficient-treasury)
    (begin
      (try! (nft-transfer? carbon-credit credit-id tx-sender (as-contract tx-sender)))
      (try! (as-contract (stx-transfer? buyback-payout tx-sender tx-sender)))
      (map-set credits credit-id (merge credit { owner: (as-contract tx-sender), retired: false }))
      (map-set buyback-history buyback-id {
        seller: tx-sender,
        credit-id: credit-id,
        buyback-price: buyback-payout,
        timestamp: stacks-block-height
      })
      (var-set treasury-balance (- (var-get treasury-balance) buyback-payout))
      (var-set total-buybacks (+ (var-get total-buybacks) u1))
      (var-set next-buyback-id (+ buyback-id u1))
      (ok true))))

;; Read-only: Get treasury balance
(define-read-only (get-treasury-balance)
  (ok (var-get treasury-balance)))

;; Read-only: Get buyback program status
(define-read-only (get-buyback-status)
  (ok {
    active: (var-get buyback-active),
    price-per-credit: (var-get buyback-price-per-credit),
    treasury-balance: (var-get treasury-balance),
    total-buybacks: (var-get total-buybacks)
  }))

;; Read-only: Get buyback history entry
(define-read-only (get-buyback-history (buyback-id uint))
  (map-get? buyback-history buyback-id))

;; Auction data structures
(define-map auctions
  uint
  {
    credit-id: uint,
    seller: principal,
    starting-price: uint,
    current-bid: uint,
    highest-bidder: (optional principal),
    start-height: uint,
    end-height: uint,
    active: bool,
    reserve-met: bool
  }
)

(define-map auction-bids
  { auction-id: uint, bidder: principal }
  {
    amount: uint,
    timestamp: uint,
    refunded: bool
  }
)

(define-map auction-history
  uint
  {
    total-bids: uint,
    final-price: uint,
    winner: (optional principal),
    completion-height: uint
  }
)

(define-data-var next-auction-id uint u1)

;; Create auction for a carbon credit
(define-public (create-auction (credit-id uint) (starting-price uint) (duration-blocks uint) (reserve-price uint))
  (let ((credit (unwrap! (map-get? credits credit-id) err-not-found))
        (auction-id (var-get next-auction-id))
        (end-height (+ stacks-block-height duration-blocks)))
    (asserts! (is-eq (get owner credit) tx-sender) err-unauthorized)
    (asserts! (not (get retired credit)) err-unauthorized)
    (asserts! (> starting-price u0) err-invalid-amount)
    (asserts! (> duration-blocks u0) err-invalid-amount)
    (asserts! (>= reserve-price starting-price) err-invalid-amount)
    (begin
      (map-set auctions auction-id {
        credit-id: credit-id,
        seller: tx-sender,
        starting-price: starting-price,
        current-bid: starting-price,
        highest-bidder: none,
        start-height: stacks-block-height,
        end-height: end-height,
        active: true,
        reserve-met: (is-eq reserve-price starting-price)
      })
      (var-set next-auction-id (+ auction-id u1))
      (ok auction-id))))

;; Place bid on auction
(define-public (place-bid (auction-id uint) (bid-amount uint))
  (let ((auction (unwrap! (map-get? auctions auction-id) err-auction-not-found))
        (credit (unwrap! (map-get? credits (get credit-id auction)) err-not-found)))
    (asserts! (get active auction) err-auction-not-active)
    (asserts! (<= stacks-block-height (get end-height auction)) err-auction-expired)
    (asserts! (not (is-eq tx-sender (get seller auction))) err-self-bid)
    (asserts! (> bid-amount (get current-bid auction)) err-bid-too-low)
    (asserts! (not (get retired credit)) err-unauthorized)
    (begin
      ;; Refund previous highest bidder if exists
      (match (get highest-bidder auction)
        prev-bidder 
          (let ((prev-bid-data (unwrap! (map-get? auction-bids { auction-id: auction-id, bidder: prev-bidder }) err-not-found)))
            (try! (stx-transfer? (get amount prev-bid-data) tx-sender prev-bidder))
            (map-set auction-bids { auction-id: auction-id, bidder: prev-bidder } 
                     (merge prev-bid-data { refunded: true })))
        true)
      ;; Record new bid
      (map-set auction-bids { auction-id: auction-id, bidder: tx-sender } {
        amount: bid-amount,
        timestamp: stacks-block-height,
        refunded: false
      })
      ;; Transfer bid amount to contract
      (try! (stx-transfer? bid-amount tx-sender (as-contract tx-sender)))
      ;; Update auction with new highest bid
      (map-set auctions auction-id (merge auction {
        current-bid: bid-amount,
        highest-bidder: (some tx-sender),
        reserve-met: true
      }))
      (ok true))))

;; Complete auction and transfer credit to winner
(define-public (complete-auction (auction-id uint))
  (let ((auction (unwrap! (map-get? auctions auction-id) err-auction-not-found))
        (credit (unwrap! (map-get? credits (get credit-id auction)) err-not-found)))
    (asserts! (get active auction) err-auction-not-active)
    (asserts! (> stacks-block-height (get end-height auction)) err-auction-not-expired)
    (asserts! (get reserve-met auction) err-no-bids)
    (match (get highest-bidder auction)
      winner 
        (begin
          ;; Transfer credit to winner
          (try! (nft-transfer? carbon-credit (get credit-id auction) (get seller auction) winner))
          ;; Update credit ownership
          (map-set credits (get credit-id auction) (merge credit { owner: winner }))
          ;; Transfer payment to seller
          (try! (as-contract (stx-transfer? (get current-bid auction) tx-sender (get seller auction))))
          ;; Mark auction as completed
          (map-set auctions auction-id (merge auction { active: false }))
          ;; Record auction history
          (map-set auction-history auction-id {
            total-bids: u1, ;; Simplified - could be enhanced to count all bids
            final-price: (get current-bid auction),
            winner: (some winner),
            completion-height: stacks-block-height
          })
          (ok winner))
      err-no-bids)))

;; Cancel auction (only by seller, only if no bids)
(define-public (cancel-auction (auction-id uint))
  (let ((auction (unwrap! (map-get? auctions auction-id) err-auction-not-found)))
    (asserts! (is-eq tx-sender (get seller auction)) err-unauthorized)
    (asserts! (get active auction) err-auction-not-active)
    (asserts! (is-none (get highest-bidder auction)) err-unauthorized)
    (begin
      (map-set auctions auction-id (merge auction { active: false }))
      (ok true))))

;; Emergency refund for failed auction
(define-public (emergency-refund (auction-id uint) (bidder principal))
  (let ((auction (unwrap! (map-get? auctions auction-id) err-auction-not-found))
        (bid-data (unwrap! (map-get? auction-bids { auction-id: auction-id, bidder: bidder }) err-not-found)))
    (asserts! (not (get active auction)) err-auction-not-active)
    (asserts! (not (get refunded bid-data)) err-refund-failed)
    (asserts! (> stacks-block-height (+ (get end-height auction) u144)) err-auction-not-expired) ;; 24 hours after auction end
    (begin
      (try! (as-contract (stx-transfer? (get amount bid-data) tx-sender bidder)))
      (map-set auction-bids { auction-id: auction-id, bidder: bidder } 
               (merge bid-data { refunded: true }))
      (ok true))))

;; Read-only functions for auction system
(define-read-only (get-auction (auction-id uint))
  (map-get? auctions auction-id))

(define-read-only (get-auction-bid (auction-id uint) (bidder principal))
  (map-get? auction-bids { auction-id: auction-id, bidder: bidder }))

(define-read-only (get-auction-history (auction-id uint))
  (map-get? auction-history auction-id))

(define-read-only (is-auction-active (auction-id uint))
  (match (map-get? auctions auction-id)
    auction (and (get active auction) (<= stacks-block-height (get end-height auction)))
    false))

(define-read-only (get-time-remaining (auction-id uint))
  (match (map-get? auctions auction-id)
    auction (if (<= stacks-block-height (get end-height auction))
               (ok (- (get end-height auction) stacks-block-height))
               (ok u0))
    err-auction-not-found))

(define-read-only (estimate-auction-value (auction-id uint))
  (match (map-get? auctions auction-id)
    auction (let ((time-remaining (unwrap! (get-time-remaining auction-id) err-auction-not-found))
                  (current-bid (get current-bid auction))
                  (time-factor (if (> time-remaining u0) 
                                 (/ (* time-remaining u100) (- (get end-height auction) (get start-height auction)))
                                 u0)))
              (ok (+ current-bid (/ (* current-bid time-factor) u1000))))
    err-auction-not-found))
