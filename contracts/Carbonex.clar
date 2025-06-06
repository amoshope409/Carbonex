(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_AMOUNT (err u103))
(define-constant ERR_INSUFFICIENT_BALANCE (err u104))
(define-constant ERR_NOT_OWNER (err u105))
(define-constant ERR_INVALID_PRICE (err u106))
(define-constant ERR_CREDIT_NOT_FOR_SALE (err u107))

(define-non-fungible-token carbon-credit uint)

(define-data-var next-credit-id uint u1)
(define-data-var platform-fee uint u250)

(define-map carbon-credits
  uint
  {
    issuer: principal,
    project-name: (string-ascii 100),
    co2-amount: uint,
    verification-standard: (string-ascii 50),
    vintage-year: uint,
    created-at: uint
  }
)

(define-map credit-listings
  uint
  {
    seller: principal,
    price: uint,
    listed-at: uint
  }
)

(define-map issuer-registry
  principal
  {
    name: (string-ascii 100),
    verified: bool,
    registered-at: uint
  }
)

(define-map user-balances
  principal
  uint
)

(define-public (register-issuer (name (string-ascii 100)))
  (let ((issuer tx-sender))
    (asserts! (is-none (map-get? issuer-registry issuer)) ERR_ALREADY_EXISTS)
    (map-set issuer-registry issuer {
      name: name,
      verified: false,
      registered-at: stacks-block-height
    })
    (ok true)
  )
)

(define-public (verify-issuer (issuer principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (is-some (map-get? issuer-registry issuer)) ERR_NOT_FOUND)
    (map-set issuer-registry issuer
      (merge (unwrap-panic (map-get? issuer-registry issuer)) {verified: true})
    )
    (ok true)
  )
)

(define-public (mint-carbon-credit 
  (project-name (string-ascii 100))
  (co2-amount uint)
  (verification-standard (string-ascii 50))
  (vintage-year uint)
)
  (let (
    (credit-id (var-get next-credit-id))
    (issuer tx-sender)
    (issuer-data (unwrap! (map-get? issuer-registry issuer) ERR_NOT_AUTHORIZED))
  )
    (asserts! (> co2-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= vintage-year u2000) ERR_INVALID_AMOUNT)
    (asserts! (<= vintage-year u2030) ERR_INVALID_AMOUNT)
    (asserts! (get verified issuer-data) ERR_NOT_AUTHORIZED)
    (try! (nft-mint? carbon-credit credit-id issuer))
    (map-set carbon-credits credit-id {
      issuer: issuer,
      project-name: project-name,
      co2-amount: co2-amount,
      verification-standard: verification-standard,
      vintage-year: vintage-year,
      created-at: stacks-block-height
    })
    (var-set next-credit-id (+ credit-id u1))
    (ok credit-id)
  )
)

(define-public (list-credit-for-sale (credit-id uint) (price uint))
  (let ((owner (unwrap! (nft-get-owner? carbon-credit credit-id) ERR_NOT_FOUND)))
    (asserts! (is-eq tx-sender owner) ERR_NOT_OWNER)
    (asserts! (> price u0) ERR_INVALID_PRICE)
    (asserts! (is-none (map-get? credit-listings credit-id)) ERR_ALREADY_EXISTS)
    (map-set credit-listings credit-id {
      seller: tx-sender,
      price: price,
      listed-at: stacks-block-height
    })
    (ok true)
  )
)

(define-public (update-listing-price (credit-id uint) (new-price uint))
  (let ((listing (unwrap! (map-get? credit-listings credit-id) ERR_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get seller listing)) ERR_NOT_OWNER)
    (asserts! (> new-price u0) ERR_INVALID_PRICE)
    (map-set credit-listings credit-id
      (merge listing {price: new-price})
    )
    (ok true)
  )
)

(define-public (remove-listing (credit-id uint))
  (let ((listing (unwrap! (map-get? credit-listings credit-id) ERR_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get seller listing)) ERR_NOT_OWNER)
    (map-delete credit-listings credit-id)
    (ok true)
  )
)

(define-public (buy-carbon-credit (credit-id uint))
  (let (
    (listing (unwrap! (map-get? credit-listings credit-id) ERR_CREDIT_NOT_FOR_SALE))
    (seller (get seller listing))
    (price (get price listing))
    (fee (/ (* price (var-get platform-fee)) u10000))
    (seller-amount (- price fee))
    (buyer tx-sender)
  )
    (asserts! (not (is-eq buyer seller)) ERR_NOT_AUTHORIZED)
    (try! (stx-transfer? price buyer seller))
    (if (> fee u0)
      (try! (stx-transfer? fee seller CONTRACT_OWNER))
      true
    )
    (try! (nft-transfer? carbon-credit credit-id seller buyer))
    (map-delete credit-listings credit-id)
    (ok true)
  )
)

(define-public (retire-carbon-credit (credit-id uint))
  (let ((owner (unwrap! (nft-get-owner? carbon-credit credit-id) ERR_NOT_FOUND)))
    (asserts! (is-eq tx-sender owner) ERR_NOT_OWNER)
    (try! (nft-burn? carbon-credit credit-id owner))
    (map-delete credit-listings credit-id)
    (map-set user-balances owner 
      (+ (default-to u0 (map-get? user-balances owner)) u1)
    )
    (ok true)
  )
)

(define-public (set-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (<= new-fee u1000) ERR_INVALID_AMOUNT)
    (var-set platform-fee new-fee)
    (ok true)
  )
)

(define-read-only (get-carbon-credit (credit-id uint))
  (map-get? carbon-credits credit-id)
)

(define-read-only (get-credit-listing (credit-id uint))
  (map-get? credit-listings credit-id)
)

(define-read-only (get-issuer-info (issuer principal))
  (map-get? issuer-registry issuer)
)

(define-read-only (get-credit-owner (credit-id uint))
  (nft-get-owner? carbon-credit credit-id)
)

(define-read-only (get-user-retired-balance (user principal))
  (default-to u0 (map-get? user-balances user))
)

(define-read-only (get-platform-fee)
  (var-get platform-fee)
)

(define-read-only (get-next-credit-id)
  (var-get next-credit-id)
)

(define-read-only (get-contract-owner)
  CONTRACT_OWNER
)