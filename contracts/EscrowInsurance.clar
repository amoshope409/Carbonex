;; Escrow & Insurance System for Carbon Credit Trades
;; Provides secure escrow services with optional insurance coverage

(define-constant CONTRACT_OWNER tx-sender)
(define-constant CARBONEX_CONTRACT 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.Carbonex)

;; Error constants (200-210 range to avoid conflicts)
(define-constant ERR_NOT_AUTHORIZED (err u200))
(define-constant ERR_NOT_FOUND (err u201))
(define-constant ERR_INVALID_STATUS (err u202))
(define-constant ERR_INVALID_AMOUNT (err u203))
(define-constant ERR_NOT_PARTY (err u204))
(define-constant ERR_ALREADY_CLAIMED (err u205))
(define-constant ERR_INSUFFICIENT_FUNDS (err u206))
(define-constant ERR_INVALID_PREMIUM (err u207))
(define-constant ERR_DISPUTE_THRESHOLD (err u208))
(define-constant ERR_ALREADY_EXISTS (err u209))
(define-constant ERR_TRANSFER_FAILED (err u210))

;; Status constants
(define-constant STATUS_PENDING u0)
(define-constant STATUS_FUNDED u1)
(define-constant STATUS_COMPLETED u2)
(define-constant STATUS_DISPUTED u3)
(define-constant STATUS_CANCELLED u4)

;; Data variables
(define-data-var next-escrow-id uint u1)
(define-data-var base-premium-rate uint u200) ;; 2% base rate
(define-data-var insurance-pool uint u0)
(define-data-var dispute-threshold uint u10000000) ;; 10,000 STX threshold for insurance eligibility

;; Escrow records
(define-map escrows
  uint
  {
    buyer: principal,
    seller: principal,
    credit-id: uint,
    price: uint,
    premium: uint,
    status: uint,
    created-at: uint,
    expires-at: uint
  }
)

;; Insurance claims
(define-map claims
  uint
  {
    claimant: principal,
    filed-at: uint,
    resolved: bool,
    payout-amount: uint
  }
)

;; Create escrow for a carbon credit trade
(define-public (create-escrow (credit-id uint) (seller principal) (price uint))
  (let (
    (escrow-id (var-get next-escrow-id))
    (buyer tx-sender)
    (credit-valuation (contract-call? .Carbonex get-credit-valuation credit-id))
    (liquidity-score (match credit-valuation valuation (get liquidity-score valuation) u50))
    (premium (calculate-premium price liquidity-score))
  )
    ;; Validate inputs
    (asserts! (> price u0) ERR_INVALID_AMOUNT)
    (asserts! (not (is-eq buyer seller)) ERR_NOT_AUTHORIZED)
    (asserts! (is-some (contract-call? .Carbonex get-credit-owner credit-id)) ERR_NOT_FOUND)
    (asserts! (is-eq seller (unwrap! (contract-call? .Carbonex get-credit-owner credit-id) ERR_NOT_AUTHORIZED)) ERR_NOT_AUTHORIZED)
    
    ;; Create escrow record
    (map-set escrows escrow-id {
      buyer: buyer,
      seller: seller,
      credit-id: credit-id,
      price: price,
      premium: premium,
      status: STATUS_PENDING,
      created-at: stacks-block-height,
      expires-at: (+ stacks-block-height u144) ;; 24 hours expiry
    })
    
    (var-set next-escrow-id (+ escrow-id u1))
    (ok escrow-id)
  )
)

;; Buyer deposits funds (price + premium)
(define-public (deposit-funds (escrow-id uint))
  (let (
    (escrow (unwrap! (map-get? escrows escrow-id) ERR_NOT_FOUND))
    (buyer (get buyer escrow))
    (total-amount (+ (get price escrow) (get premium escrow)))
  )
    (asserts! (is-eq tx-sender buyer) ERR_NOT_PARTY)
    (asserts! (is-eq (get status escrow) STATUS_PENDING) ERR_INVALID_STATUS)
    (asserts! (< stacks-block-height (get expires-at escrow)) ERR_NOT_AUTHORIZED)
    
    ;; Transfer funds to contract
    (try! (stx-transfer? total-amount buyer (as-contract tx-sender)))
    
    ;; Update escrow status and insurance pool
    (map-set escrows escrow-id (merge escrow {status: STATUS_FUNDED}))
    (var-set insurance-pool (+ (var-get insurance-pool) (get premium escrow)))
    
    (ok true)
  )
)

;; Buyer confirms receipt and releases funds
(define-public (confirm-receipt (escrow-id uint))
  (let (
    (escrow (unwrap! (map-get? escrows escrow-id) ERR_NOT_FOUND))
    (buyer (get buyer escrow))
    (seller (get seller escrow))
    (price (get price escrow))
    (credit-id (get credit-id escrow))
  )
    (asserts! (is-eq tx-sender buyer) ERR_NOT_PARTY)
    (asserts! (is-eq (get status escrow) STATUS_FUNDED) ERR_INVALID_STATUS)
    
    ;; Transfer NFT to buyer via Carbonex contract
    (try! (as-contract (contract-call? .Carbonex retire-carbon-credit credit-id)))
    
    ;; Transfer payment to seller
    (try! (as-contract (stx-transfer? price tx-sender seller)))
    
    ;; Update status
    (map-set escrows escrow-id (merge escrow {status: STATUS_COMPLETED}))
    
    (ok true)
  )
)

;; Raise a dispute
(define-public (raise-dispute (escrow-id uint) (reason (string-ascii 200)))
  (let (
    (escrow (unwrap! (map-get? escrows escrow-id) ERR_NOT_FOUND))
    (caller tx-sender)
  )
    (asserts! (or (is-eq caller (get buyer escrow)) (is-eq caller (get seller escrow))) ERR_NOT_PARTY)
    (asserts! (is-eq (get status escrow) STATUS_FUNDED) ERR_INVALID_STATUS)
    
    ;; Update status to disputed
    (map-set escrows escrow-id (merge escrow {status: STATUS_DISPUTED}))
    
    (ok true)
  )
)

;; Resolve dispute (contract owner only)
(define-public (resolve-dispute (escrow-id uint) (winner-is-buyer bool))
  (let (
    (escrow (unwrap! (map-get? escrows escrow-id) ERR_NOT_FOUND))
    (buyer (get buyer escrow))
    (seller (get seller escrow))
    (price (get price escrow))
    (premium (get premium escrow))
  )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status escrow) STATUS_DISPUTED) ERR_INVALID_STATUS)
    
    (if winner-is-buyer
      ;; Buyer wins - refund price + premium, seller gets nothing
      (begin
        (try! (as-contract (stx-transfer? (+ price premium) tx-sender buyer)))
        (var-set insurance-pool (- (var-get insurance-pool) premium))
      )
      ;; Seller wins - seller gets price, buyer gets premium refund
      (begin
        (try! (as-contract (stx-transfer? price tx-sender seller)))
        (try! (as-contract (stx-transfer? premium tx-sender buyer)))
        (var-set insurance-pool (- (var-get insurance-pool) premium))
        (try! (as-contract (contract-call? .Carbonex retire-carbon-credit (get credit-id escrow))))
      )
    )
    
    (map-set escrows escrow-id (merge escrow {status: STATUS_COMPLETED}))
    (ok true)
  )
)

;; Claim insurance (for high-value disputed transactions)
(define-public (claim-insurance (escrow-id uint))
  (let (
    (escrow (unwrap! (map-get? escrows escrow-id) ERR_NOT_FOUND))
    (buyer (get buyer escrow))
    (price (get price escrow))
    (existing-claim (map-get? claims escrow-id))
  )
    (asserts! (is-eq tx-sender buyer) ERR_NOT_PARTY)
    (asserts! (>= price (var-get dispute-threshold)) ERR_DISPUTE_THRESHOLD)
    (asserts! (is-eq (get status escrow) STATUS_DISPUTED) ERR_INVALID_STATUS)
    (asserts! (is-none existing-claim) ERR_ALREADY_CLAIMED)
    
    ;; Calculate payout (max 80% of transaction value)
    (let (
      (max-payout (* price u8000 (/ u1 u10000)))
      (payout-amount (if (< price max-payout) price max-payout))
      (available-insurance (var-get insurance-pool))
    )
      (asserts! (>= available-insurance payout-amount) ERR_INSUFFICIENT_FUNDS)
      
      ;; Process insurance claim
      (try! (as-contract (stx-transfer? payout-amount tx-sender buyer)))
      (var-set insurance-pool (- available-insurance payout-amount))
      
      ;; Record claim
      (map-set claims escrow-id {
        claimant: buyer,
        filed-at: stacks-block-height,
        resolved: true,
        payout-amount: payout-amount
      })
      
      (ok payout-amount)
    )
  )
)

;; Update premium rate (owner only)
(define-public (set-premium-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (<= new-rate u1000) ERR_INVALID_PREMIUM) ;; Max 10%
    (var-set base-premium-rate new-rate)
    (ok true)
  )
)

;; Calculate premium based on price and liquidity risk
(define-private (calculate-premium (price uint) (liquidity-score uint))
  (let (
    (base-rate (var-get base-premium-rate))
    (risk-adjustment (- u100 liquidity-score))
    (total-rate (+ base-rate risk-adjustment))
    (premium (/ (* price total-rate) u10000))
  )
    (if (> premium u0) premium u1)
  )
)

;; Read-only functions
(define-read-only (get-escrow (escrow-id uint))
  (map-get? escrows escrow-id)
)

(define-read-only (get-claim (escrow-id uint))
  (map-get? claims escrow-id)
)

(define-read-only (get-premium-for-price (price uint) (liquidity-score uint))
  (calculate-premium price liquidity-score)
)

(define-read-only (get-insurance-pool-balance)
  (var-get insurance-pool)
)

(define-read-only (get-next-escrow-id)
  (var-get next-escrow-id)
)

(define-read-only (get-base-premium-rate)
  (var-get base-premium-rate)
)

(define-read-only (get-dispute-threshold)
  (var-get dispute-threshold)
)
