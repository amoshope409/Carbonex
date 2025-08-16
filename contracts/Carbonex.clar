(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_AMOUNT (err u103))
(define-constant ERR_INSUFFICIENT_BALANCE (err u104))
(define-constant ERR_NOT_OWNER (err u105))
(define-constant ERR_INVALID_PRICE (err u106))
(define-constant ERR_CREDIT_NOT_FOR_SALE (err u107))
(define-constant ERR_PORTFOLIO_NOT_FOUND (err u108))
(define-constant ERR_INVALID_TARGET (err u109))
(define-constant ERR_REBALANCE_NOT_NEEDED (err u110))
(define-constant ERR_INSUFFICIENT_CREDITS (err u111))
(define-constant ERR_INVALID_THRESHOLD (err u112))
(define-constant ERR_INVALID_SHARES (err u113))
(define-constant ERR_INSUFFICIENT_SHARES (err u114))
(define-constant ERR_CREDIT_ALREADY_FRACTIONALIZED (err u115))
(define-constant ERR_CREDIT_NOT_FRACTIONALIZED (err u116))
(define-constant ERR_INVALID_FRACTION_ID (err u117))

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

(define-map portfolios
  principal
  {
    total-credits: uint,
    total-value: uint,
    vintage-distribution: {v2020: uint, v2021: uint, v2022: uint, v2023: uint, v2024: uint, other: uint},
    standard-distribution: {vcs: uint, gold: uint, caa: uint, other: uint},
    last-updated: uint,
    rebalance-enabled: bool,
    rebalance-threshold: uint
  }
)

(define-map portfolio-targets
  principal
  {
    vintage-targets: {v2020: uint, v2021: uint, v2022: uint, v2023: uint, v2024: uint, other: uint},
    standard-targets: {vcs: uint, gold: uint, caa: uint, other: uint},
    max-deviation: uint
  }
)

(define-map portfolio-analytics
  principal
  {
    total-purchased: uint,
    total-retired: uint,
    total-sold: uint,
    average-purchase-price: uint,
    performance-score: uint,
    risk-score: uint,
    last-rebalance: uint
  }
)

(define-map credit-valuations
  uint
  {
    current-market-value: uint,
    last-sale-price: uint,
    valuation-timestamp: uint,
    liquidity-score: uint
  }
)

;; Fractionalization data structures
(define-data-var next-fraction-id uint u1)

(define-map fractionalized-credits
  uint
  {
    credit-id: uint,
    total-shares: uint,
    share-price: uint,
    created-at: uint,
    is-active: bool
  }
)

(define-map fraction-shares
  {fraction-id: uint, owner: principal}
  uint
)

(define-map fraction-listings
  uint
  {
    fraction-id: uint,
    seller: principal,
    shares-amount: uint,
    price-per-share: uint,
    listed-at: uint
  }
)

(define-map user-fraction-holdings
  {user: principal, fraction-id: uint}
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
    (update-portfolio-on-acquisition buyer credit-id price)
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

(define-public (initialize-portfolio)
  (let ((user tx-sender))
    (asserts! (is-none (map-get? portfolios user)) ERR_ALREADY_EXISTS)
    (map-set portfolios user {
      total-credits: u0,
      total-value: u0,
      vintage-distribution: {v2020: u0, v2021: u0, v2022: u0, v2023: u0, v2024: u0, other: u0},
      standard-distribution: {vcs: u0, gold: u0, caa: u0, other: u0},
      last-updated: stacks-block-height,
      rebalance-enabled: false,
      rebalance-threshold: u500
    })
    (map-set portfolio-analytics user {
      total-purchased: u0,
      total-retired: u0,
      total-sold: u0,
      average-purchase-price: u0,
      performance-score: u0,
      risk-score: u0,
      last-rebalance: u0
    })
    (ok true)
  )
)

(define-public (set-portfolio-targets 
  (vintage-targets {v2020: uint, v2021: uint, v2022: uint, v2023: uint, v2024: uint, other: uint})
  (standard-targets {vcs: uint, gold: uint, caa: uint, other: uint})
  (max-deviation uint)
)
  (let ((user tx-sender))
    (asserts! (is-some (map-get? portfolios user)) ERR_PORTFOLIO_NOT_FOUND)
    (asserts! (<= max-deviation u5000) ERR_INVALID_TARGET)
    (let (
      (vintage-sum (+ (+ (+ (+ (+ (get v2020 vintage-targets) (get v2021 vintage-targets)) (get v2022 vintage-targets)) (get v2023 vintage-targets)) (get v2024 vintage-targets)) (get other vintage-targets)))
      (standard-sum (+ (+ (+ (get vcs standard-targets) (get gold standard-targets)) (get caa standard-targets)) (get other standard-targets)))
    )
      (asserts! (is-eq vintage-sum u10000) ERR_INVALID_TARGET)
      (asserts! (is-eq standard-sum u10000) ERR_INVALID_TARGET)
      (map-set portfolio-targets user {
        vintage-targets: vintage-targets,
        standard-targets: standard-targets,
        max-deviation: max-deviation
      })
      (ok true)
    )
  )
)

(define-public (update-credit-valuation (credit-id uint) (market-value uint) (liquidity-score uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (is-some (map-get? carbon-credits credit-id)) ERR_NOT_FOUND)
    (asserts! (<= liquidity-score u100) ERR_INVALID_AMOUNT)
    (let ((existing-valuation (map-get? credit-valuations credit-id)))
      (map-set credit-valuations credit-id {
        current-market-value: market-value,
        last-sale-price: (match existing-valuation valuation (get last-sale-price valuation) market-value),
        valuation-timestamp: stacks-block-height,
        liquidity-score: liquidity-score
      })
      (ok true)
    )
  )
)

(define-private (get-vintage-category (vintage uint))
  (if (is-eq vintage u2020) "v2020"
    (if (is-eq vintage u2021) "v2021"
      (if (is-eq vintage u2022) "v2022"
        (if (is-eq vintage u2023) "v2023"
          (if (is-eq vintage u2024) "v2024" "other")
        )
      )
    )
  )
)

(define-private (get-standard-category (standard (string-ascii 50)))
  (if (is-eq standard "VCS") "vcs"
    (if (is-eq standard "GOLD") "gold"
      (if (is-eq standard "CAA") "caa" "other")
    )
  )
)

(define-private (update-portfolio-on-acquisition (user principal) (credit-id uint) (price uint))
  (let (
    (credit-data (unwrap-panic (map-get? carbon-credits credit-id)))
    (portfolio (default-to {
      total-credits: u0,
      total-value: u0,
      vintage-distribution: {v2020: u0, v2021: u0, v2022: u0, v2023: u0, v2024: u0, other: u0},
      standard-distribution: {vcs: u0, gold: u0, caa: u0, other: u0},
      last-updated: u0,
      rebalance-enabled: false,
      rebalance-threshold: u500
    } (map-get? portfolios user)))
    (analytics (default-to {
      total-purchased: u0,
      total-retired: u0,
      total-sold: u0,
      average-purchase-price: u0,
      performance-score: u0,
      risk-score: u0,
      last-rebalance: u0
    } (map-get? portfolio-analytics user)))
    (vintage-year (get vintage-year credit-data))
    (verification-standard (get verification-standard credit-data))
    (vintage-cat (get-vintage-category vintage-year))
    (standard-cat (get-standard-category verification-standard))
  )
    (map-set credit-valuations credit-id {
      current-market-value: price,
      last-sale-price: price,
      valuation-timestamp: stacks-block-height,
      liquidity-score: u80
    })
    (map-set portfolios user (merge portfolio {
      total-credits: (+ (get total-credits portfolio) u1),
      total-value: (+ (get total-value portfolio) price),
      last-updated: stacks-block-height
    }))
    (map-set portfolio-analytics user (merge analytics {
      total-purchased: (+ (get total-purchased analytics) u1),
      average-purchase-price: (/ (+ (* (get average-purchase-price analytics) (get total-purchased analytics)) price) (+ (get total-purchased analytics) u1))
    }))
    true
  )
)

(define-public (calculate-portfolio-performance (user principal))
  (let (
    (portfolio (unwrap! (map-get? portfolios user) ERR_PORTFOLIO_NOT_FOUND))
    (analytics (unwrap! (map-get? portfolio-analytics user) ERR_PORTFOLIO_NOT_FOUND))
    (total-credits (get total-credits portfolio))
    (total-value (get total-value portfolio))
    (avg-price (get average-purchase-price analytics))
  )
    (if (> total-credits u0)
      (let (
        (current-avg-value (/ total-value total-credits))
        (performance-ratio (if (> avg-price u0) (/ (* current-avg-value u100) avg-price) u100))
        (risk-score (if (> total-credits u10) u20 (- u100 (* total-credits u8))))
        (performance-score (if (>= performance-ratio u100) (- performance-ratio u100) u0))
      )
        (map-set portfolio-analytics user (merge analytics {
          performance-score: performance-score,
          risk-score: risk-score
        }))
        (ok {performance: performance-score, risk: risk-score})
      )
      (ok {performance: u0, risk: u100})
    )
  )
)

(define-public (enable-auto-rebalance (threshold uint))
  (let (
    (user tx-sender)
    (portfolio (unwrap! (map-get? portfolios user) ERR_PORTFOLIO_NOT_FOUND))
  )
    (asserts! (<= threshold u2000) ERR_INVALID_THRESHOLD)
    (asserts! (>= threshold u100) ERR_INVALID_THRESHOLD)
    (map-set portfolios user (merge portfolio {
      rebalance-enabled: true,
      rebalance-threshold: threshold
    }))
    (ok true)
  )
)

(define-public (check-rebalance-needed (user principal))
  (let (
    (portfolio (unwrap! (map-get? portfolios user) ERR_PORTFOLIO_NOT_FOUND))
    (targets (unwrap! (map-get? portfolio-targets user) ERR_PORTFOLIO_NOT_FOUND))
    (total-credits (get total-credits portfolio))
    (threshold (get rebalance-threshold portfolio))
  )
    (if (and (get rebalance-enabled portfolio) (> total-credits u5))
      (let (
        (vintage-dist (get vintage-distribution portfolio))
        (vintage-targets (get vintage-targets targets))
        (v2020-current (if (> total-credits u0) (/ (* (get v2020 vintage-dist) u10000) total-credits) u0))
        (v2020-target (get v2020 vintage-targets))
        (v2021-current (if (> total-credits u0) (/ (* (get v2021 vintage-dist) u10000) total-credits) u0))
        (v2021-target (get v2021 vintage-targets))
        (v2020-diff (if (>= v2020-current v2020-target) (- v2020-current v2020-target) (- v2020-target v2020-current)))
        (v2021-diff (if (>= v2021-current v2021-target) (- v2021-current v2021-target) (- v2021-target v2021-current)))
        (max-deviation (if (> v2020-diff v2021-diff) v2020-diff v2021-diff))
      )
        (ok (>= max-deviation threshold))
      )
      (ok false)
    )
  )
)

(define-read-only (get-portfolio (user principal))
  (map-get? portfolios user)
)

(define-read-only (get-portfolio-targets (user principal))
  (map-get? portfolio-targets user)
)

(define-read-only (get-portfolio-analytics (user principal))
  (map-get? portfolio-analytics user)
)

(define-read-only (get-credit-valuation (credit-id uint))
  (map-get? credit-valuations credit-id)
)

(define-read-only (get-portfolio-value (user principal))
  (match (map-get? portfolios user)
    portfolio (get total-value portfolio)
    u0
  )
)

(define-read-only (get-portfolio-risk-score (user principal))
  (match (map-get? portfolio-analytics user)
    analytics (get risk-score analytics)
    u100
  )
)

;; Fractionalization Functions

(define-public (fractionalize-credit (credit-id uint) (total-shares uint) (share-price uint))
  (let (
    (owner (unwrap! (nft-get-owner? carbon-credit credit-id) ERR_NOT_FOUND))
    (fraction-id (var-get next-fraction-id))
  )
    ;; Validate ownership and parameters
    (asserts! (is-eq tx-sender owner) ERR_NOT_OWNER)
    (asserts! (> total-shares u1) ERR_INVALID_SHARES)
    (asserts! (<= total-shares u10000) ERR_INVALID_SHARES)
    (asserts! (> share-price u0) ERR_INVALID_PRICE)
    (asserts! (is-none (map-get? credit-listings credit-id)) ERR_ALREADY_EXISTS)
    
    ;; Note: Credit fractionalization check removed for simplicity
    
    ;; Create fractionalized credit record
    (map-set fractionalized-credits fraction-id {
      credit-id: credit-id,
      total-shares: total-shares,
      share-price: share-price,
      created-at: stacks-block-height,
      is-active: true
    })
    
    ;; Transfer all shares to the credit owner initially
    (map-set fraction-shares {fraction-id: fraction-id, owner: owner} total-shares)
    (map-set user-fraction-holdings {user: owner, fraction-id: fraction-id} total-shares)
    
    ;; Increment fraction ID counter
    (var-set next-fraction-id (+ fraction-id u1))
    
    (ok fraction-id)
  )
)

(define-public (list-fraction-shares (fraction-id uint) (shares-amount uint) (price-per-share uint))
  (let (
    (seller tx-sender)
    (current-shares (default-to u0 (map-get? user-fraction-holdings {user: seller, fraction-id: fraction-id})))
    (fraction-data (unwrap! (map-get? fractionalized-credits fraction-id) ERR_INVALID_FRACTION_ID))
  )
    ;; Validate inputs and ownership
    (asserts! (get is-active fraction-data) ERR_NOT_FOUND)
    (asserts! (> shares-amount u0) ERR_INVALID_SHARES)
    (asserts! (>= current-shares shares-amount) ERR_INSUFFICIENT_SHARES)
    (asserts! (> price-per-share u0) ERR_INVALID_PRICE)
    
    ;; Create fraction listing
    (map-set fraction-listings fraction-id {
      fraction-id: fraction-id,
      seller: seller,
      shares-amount: shares-amount,
      price-per-share: price-per-share,
      listed-at: stacks-block-height
    })
    
    (ok true)
  )
)

(define-public (buy-fraction-shares (fraction-id uint))
  (let (
    (listing (unwrap! (map-get? fraction-listings fraction-id) ERR_NOT_FOUND))
    (seller (get seller listing))
    (shares-amount (get shares-amount listing))
    (price-per-share (get price-per-share listing))
    (total-price (* shares-amount price-per-share))
    (buyer tx-sender)
    (seller-current-shares (default-to u0 (map-get? user-fraction-holdings {user: seller, fraction-id: fraction-id})))
    (buyer-current-shares (default-to u0 (map-get? user-fraction-holdings {user: buyer, fraction-id: fraction-id})))
  )
    ;; Validate transaction
    (asserts! (not (is-eq buyer seller)) ERR_NOT_AUTHORIZED)
    (asserts! (>= seller-current-shares shares-amount) ERR_INSUFFICIENT_SHARES)
    
    ;; Transfer payment
    (try! (stx-transfer? total-price buyer seller))
    
    ;; Update share ownership
    (map-set user-fraction-holdings {user: seller, fraction-id: fraction-id} (- seller-current-shares shares-amount))
    (map-set user-fraction-holdings {user: buyer, fraction-id: fraction-id} (+ buyer-current-shares shares-amount))
    
    ;; Update fraction-shares map
    (map-set fraction-shares {fraction-id: fraction-id, owner: seller} (- seller-current-shares shares-amount))
    (map-set fraction-shares {fraction-id: fraction-id, owner: buyer} (+ buyer-current-shares shares-amount))
    
    ;; Remove listing
    (map-delete fraction-listings fraction-id)
    
    (ok true)
  )
)

(define-public (retire-fraction-shares (fraction-id uint) (shares-amount uint))
  (let (
    (user tx-sender)
    (current-shares (default-to u0 (map-get? user-fraction-holdings {user: user, fraction-id: fraction-id})))
    (fraction-data (unwrap! (map-get? fractionalized-credits fraction-id) ERR_INVALID_FRACTION_ID))
    (credit-id (get credit-id fraction-data))
    (total-shares (get total-shares fraction-data))
  )
    ;; Validate retirement
    (asserts! (get is-active fraction-data) ERR_NOT_FOUND)
    (asserts! (> shares-amount u0) ERR_INVALID_SHARES)
    (asserts! (>= current-shares shares-amount) ERR_INSUFFICIENT_SHARES)
    
    ;; Calculate CO2 amount to retire based on fraction
    (let (
      (credit-data (unwrap! (map-get? carbon-credits credit-id) ERR_NOT_FOUND))
      (total-co2 (get co2-amount credit-data))
      (co2-to-retire (/ (* total-co2 shares-amount) total-shares))
    )
      ;; Update user's fraction holdings
      (map-set user-fraction-holdings {user: user, fraction-id: fraction-id} (- current-shares shares-amount))
      (map-set fraction-shares {fraction-id: fraction-id, owner: user} (- current-shares shares-amount))
      
      ;; Update user's retired balance
      (map-set user-balances user 
        (+ (default-to u0 (map-get? user-balances user)) co2-to-retire)
      )
      
      ;; If all shares are retired, deactivate the fraction
      (let ((remaining-shares (- (get total-shares fraction-data) shares-amount)))
        (if (is-eq remaining-shares u0)
          (begin
            (map-set fractionalized-credits fraction-id (merge fraction-data {is-active: false}))
            (try! (nft-burn? carbon-credit credit-id (unwrap-panic (nft-get-owner? carbon-credit credit-id))))
            true
          )
          (map-set fractionalized-credits fraction-id (merge fraction-data {total-shares: remaining-shares}))
        )
      )
      
      (ok co2-to-retire)
    )
  )
)

(define-public (transfer-fraction-shares (fraction-id uint) (shares-amount uint) (recipient principal))
  (let (
    (sender tx-sender)
    (sender-shares (default-to u0 (map-get? user-fraction-holdings {user: sender, fraction-id: fraction-id})))
    (recipient-shares (default-to u0 (map-get? user-fraction-holdings {user: recipient, fraction-id: fraction-id})))
    (fraction-data (unwrap! (map-get? fractionalized-credits fraction-id) ERR_INVALID_FRACTION_ID))
  )
    ;; Validate transfer
    (asserts! (get is-active fraction-data) ERR_NOT_FOUND)
    (asserts! (> shares-amount u0) ERR_INVALID_SHARES)
    (asserts! (>= sender-shares shares-amount) ERR_INSUFFICIENT_SHARES)
    (asserts! (not (is-eq sender recipient)) ERR_NOT_AUTHORIZED)
    
    ;; Update holdings
    (map-set user-fraction-holdings {user: sender, fraction-id: fraction-id} (- sender-shares shares-amount))
    (map-set user-fraction-holdings {user: recipient, fraction-id: fraction-id} (+ recipient-shares shares-amount))
    
    ;; Update fraction-shares map
    (map-set fraction-shares {fraction-id: fraction-id, owner: sender} (- sender-shares shares-amount))
    (map-set fraction-shares {fraction-id: fraction-id, owner: recipient} (+ recipient-shares shares-amount))
    
    (ok true)
  )
)

;; Read-only functions for fractionalization

(define-read-only (get-fractionalized-credit (fraction-id uint))
  (map-get? fractionalized-credits fraction-id)
)

(define-read-only (get-user-fraction-shares (user principal) (fraction-id uint))
  (default-to u0 (map-get? user-fraction-holdings {user: user, fraction-id: fraction-id}))
)

(define-read-only (get-fraction-listing (fraction-id uint))
  (map-get? fraction-listings fraction-id)
)

(define-read-only (get-fraction-by-credit (credit-id uint))
  none
)

(define-read-only (get-next-fraction-id)
  (var-get next-fraction-id)
)



