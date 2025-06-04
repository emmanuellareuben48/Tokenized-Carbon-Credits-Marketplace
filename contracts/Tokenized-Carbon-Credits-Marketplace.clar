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
