;; Carbon Credit Price Discovery Oracle
;; Advanced price intelligence and market analytics

(define-constant ERR-NOT-AUTHORIZED (err u300))
(define-constant ERR-INVALID-PRICE (err u301))
(define-constant ERR-ORACLE-NOT-FOUND (err u302))
(define-constant ERR-STALE-DATA (err u303))
(define-constant ERR-INSUFFICIENT-DATA (err u304))

;; Oracle configuration
(define-data-var contract-admin principal tx-sender)
(define-data-var price-staleness-threshold uint u144) ;; 144 blocks = ~24 hours
(define-data-var min-data-points uint u5)

;; Price feed sources and weights
(define-map price-sources
    principal
    {
        name: (string-ascii 50),
        weight: uint,
        reliability-score: uint,
        active: bool,
        last-update: uint
    }
)

;; Historical price data for each credit type
(define-map price-history
    {vintage: uint, standard: (string-ascii 50), block-height: uint}
    {
        weighted-price: uint,
        volume-traded: uint,
        source-count: uint,
        confidence-score: uint,
        market-sentiment: (string-ascii 10)
    }
)

;; Current market prices with predictions
(define-map current-prices
    {vintage: uint, standard: (string-ascii 50)}
    {
        current-price: uint,
        predicted-price-7d: uint,
        predicted-price-30d: uint,
        price-trend: (string-ascii 10),
        volatility-index: uint,
        last-updated: uint,
        confidence-level: uint
    }
)

;; Market sentiment indicators
(define-map market-indicators
    uint ;; block-height
    {
        overall-sentiment: (string-ascii 10),
        trading-volume: uint,
        active-listings: uint,
        price-variance: uint,
        market-cap: uint,
        trend-direction: (string-ascii 10)
    }
)

;; Source price submissions
(define-map source-submissions
    {source: principal, vintage: uint, standard: (string-ascii 50), block: uint}
    {
        submitted-price: uint,
        volume-data: uint,
        market-context: (string-ascii 100),
        submission-time: uint
    }
)

;; Register authorized price source
(define-public (register-price-source (source principal) (name (string-ascii 50)) (weight uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-NOT-AUTHORIZED)
        (asserts! (> weight u0) ERR-INVALID-PRICE)
        (asserts! (<= weight u100) ERR-INVALID-PRICE)
        
        (map-set price-sources source {
            name: name,
            weight: weight,
            reliability-score: u80,
            active: true,
            last-update: stacks-block-height
        })
        (ok true)
    )
)

;; Submit price data from authorized source
(define-public (submit-price-data 
    (vintage uint) 
    (standard (string-ascii 50)) 
    (price uint) 
    (volume uint) 
    (context (string-ascii 100)))
    (let 
        (
            (source-data (unwrap! (map-get? price-sources tx-sender) ERR-NOT-AUTHORIZED))
            (current-block stacks-block-height)
        )
        (asserts! (get active source-data) ERR-NOT-AUTHORIZED)
        (asserts! (> price u0) ERR-INVALID-PRICE)
        
        ;; Record submission
        (map-set source-submissions {source: tx-sender, vintage: vintage, standard: standard, block: current-block} {
            submitted-price: price,
            volume-data: volume,
            market-context: context,
            submission-time: current-block
        })
        
        ;; Update source reliability
        (map-set price-sources tx-sender (merge source-data {last-update: current-block}))
        
        ;; Trigger price calculation
        (calculate-weighted-price vintage standard)
    )
)

;; Calculate weighted price from multiple sources
(define-private (calculate-weighted-price (vintage uint) (standard (string-ascii 50)))
    (let 
        (
            (current-block stacks-block-height)
            (cutoff-block (- current-block (var-get price-staleness-threshold)))
        )
        ;; Get recent submissions and calculate weighted average
        (let 
            (
                (price-data (aggregate-recent-prices vintage standard cutoff-block))
                (weighted-price (get weighted-avg price-data))
                (data-points (get count price-data))
                (total-volume (get volume price-data))
            )
            (if (>= data-points (var-get min-data-points))
                (begin
                    ;; Store historical data
                    (map-set price-history {vintage: vintage, standard: standard, block-height: current-block} {
                        weighted-price: weighted-price,
                        volume-traded: total-volume,
                        source-count: data-points,
                        confidence-score: (calculate-confidence-score data-points total-volume),
                        market-sentiment: (determine-market-sentiment weighted-price vintage standard)
                    })
                    
                    ;; Update current prices with predictions
                    (let 
                        (
                            (price-trend (calculate-price-trend vintage standard))
                            (volatility (calculate-volatility vintage standard))
                            (predictions (generate-price-predictions weighted-price price-trend volatility))
                        )
                        (map-set current-prices {vintage: vintage, standard: standard} {
                            current-price: weighted-price,
                            predicted-price-7d: (get pred-7d predictions),
                            predicted-price-30d: (get pred-30d predictions),
                            price-trend: price-trend,
                            volatility-index: volatility,
                            last-updated: current-block,
                            confidence-level: (calculate-confidence-score data-points total-volume)
                        })
                        (ok weighted-price)
                    )
                )
                ERR-INSUFFICIENT-DATA
            )
        )
    )
)

;; Aggregate recent price data
(define-private (aggregate-recent-prices (vintage uint) (standard (string-ascii 50)) (cutoff-block uint))
    ;; Simplified aggregation - in real implementation would iterate through submissions
    {
        weighted-avg: u100000, ;; $100 per credit placeholder
        count: u6,
        volume: u500
    }
)

;; Calculate confidence score based on data quality
(define-private (calculate-confidence-score (data-points uint) (volume uint))
    (let 
        (
            (point-score (if (>= data-points u10) u50 (* data-points u5)))
            (volume-score (if (>= volume u1000) u40 (/ (* volume u40) u1000)))
        )
        (+ point-score volume-score u10) ;; Base confidence of 10
    )
)

;; Determine market sentiment
(define-private (determine-market-sentiment (current-price uint) (vintage uint) (standard (string-ascii 50)))
    (let 
        (
            (historical-price (get-historical-average-price vintage standard))
        )
        (if (> current-price (+ historical-price (/ historical-price u10)))
            "bullish"
            (if (< current-price (- historical-price (/ historical-price u10)))
                "bearish"
                "neutral"
            )
        )
    )
)

;; Calculate price trend direction
(define-private (calculate-price-trend (vintage uint) (standard (string-ascii 50)))
    (let 
        (
            (recent-avg (get-recent-price-average vintage standard))
            (historical-avg (get-historical-average-price vintage standard))
        )
        (if (> recent-avg historical-avg)
            "up"
            (if (< recent-avg historical-avg)
                "down"
                "stable"
            )
        )
    )
)

;; Calculate price volatility index
(define-private (calculate-volatility (vintage uint) (standard (string-ascii 50)))
    ;; Simplified volatility calculation - would use standard deviation in practice
    (let 
        (
            (price-range (get-price-range vintage standard))
            (avg-price (get-recent-price-average vintage standard))
        )
        (if (> avg-price u0)
            (/ (* price-range u100) avg-price)
            u0
        )
    )
)

;; Generate price predictions
(define-private (generate-price-predictions (current-price uint) (trend (string-ascii 10)) (volatility uint))
    (let 
        (
            (trend-factor (if (is-eq trend "up") u105 
                         (if (is-eq trend "down") u95 u100)))
            (volatility-adjustment (/ volatility u10))
        )
        {
            pred-7d: (+ (/ (* current-price trend-factor) u100) volatility-adjustment),
            pred-30d: (+ (/ (* current-price (* trend-factor trend-factor)) u10000) (* volatility-adjustment u2))
        }
    )
)

;; Helper functions for price calculations
(define-private (get-historical-average-price (vintage uint) (standard (string-ascii 50)))
    u95000 ;; Placeholder - $95 average
)

(define-private (get-recent-price-average (vintage uint) (standard (string-ascii 50)))
    u102000 ;; Placeholder - $102 recent average
)

(define-private (get-price-range (vintage uint) (standard (string-ascii 50)))
    u15000 ;; Placeholder - $15 price range
)

;; Update market indicators
(define-public (update-market-indicators 
    (trading-volume uint) 
    (active-listings uint) 
    (market-cap uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-NOT-AUTHORIZED)
        
        (let 
            (
                (current-block stacks-block-height)
                (overall-sentiment (calculate-overall-sentiment trading-volume active-listings))
                (trend-direction (determine-overall-trend))
            )
            (map-set market-indicators current-block {
                overall-sentiment: overall-sentiment,
                trading-volume: trading-volume,
                active-listings: active-listings,
                price-variance: (calculate-market-variance),
                market-cap: market-cap,
                trend-direction: trend-direction
            })
            (ok true)
        )
    )
)

;; Calculate overall market sentiment
(define-private (calculate-overall-sentiment (volume uint) (listings uint))
    (if (and (> volume u1000) (> listings u20))
        "bullish"
        (if (and (< volume u200) (< listings u5))
            "bearish"
            "neutral"
        )
    )
)

;; Determine overall market trend
(define-private (determine-overall-trend)
    "up" ;; Simplified - would analyze multiple indicators
)

;; Calculate market-wide price variance
(define-private (calculate-market-variance)
    u125 ;; Placeholder variance
)

;; Read-only functions
(define-read-only (get-current-price (vintage uint) (standard (string-ascii 50)))
    (map-get? current-prices {vintage: vintage, standard: standard})
)

(define-read-only (get-price-history (vintage uint) (standard (string-ascii 50)) (target-block uint))
    (map-get? price-history {vintage: vintage, standard: standard, block-height: target-block})
)

(define-read-only (get-market-indicators (target-block uint))
    (map-get? market-indicators target-block)
)

(define-read-only (get-price-source (source principal))
    (map-get? price-sources source)
)

(define-read-only (get-latest-market-data)
    (ok {
        current-block: stacks-block-height,
        data-staleness-threshold: (var-get price-staleness-threshold),
        min-required-sources: (var-get min-data-points),
        oracle-admin: (var-get contract-admin)
    })
)

(define-read-only (is-price-data-fresh (vintage uint) (standard (string-ascii 50)))
    (match (map-get? current-prices {vintage: vintage, standard: standard})
        price-data 
        (let 
            (
                (last-update (get last-updated price-data))
                (staleness-threshold (var-get price-staleness-threshold))
            )
            (ok (>= (+ last-update staleness-threshold) stacks-block-height))
        )
        (ok false)
    )
)

;; Price estimation for credit valuation
(define-read-only (estimate-credit-value (vintage uint) (standard (string-ascii 50)) (co2-amount uint))
    (match (map-get? current-prices {vintage: vintage, standard: standard})
        price-data 
        (let 
            (
                (price-per-ton (get current-price price-data))
                (confidence (get confidence-level price-data))
                (estimated-value (/ (* price-per-ton co2-amount) u1000)) ;; Assuming co2-amount is in kg
            )
            (ok {
                estimated-value: estimated-value,
                confidence-level: confidence,
                price-per-ton: price-per-ton,
                valuation-timestamp: stacks-block-height
            })
        )
        ERR-ORACLE-NOT-FOUND
    )
)

;; Administrative functions
(define-public (set-staleness-threshold (new-threshold uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-NOT-AUTHORIZED)
        (asserts! (> new-threshold u0) ERR-INVALID-PRICE)
        (var-set price-staleness-threshold new-threshold)
        (ok true)
    )
)

(define-public (deactivate-price-source (source principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-NOT-AUTHORIZED)
        
        (match (map-get? price-sources source)
            source-data 
            (begin
                (map-set price-sources source (merge source-data {active: false}))
                (ok true)
            )
            ERR-ORACLE-NOT-FOUND
        )
    )
)

(define-public (transfer-admin (new-admin principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-NOT-AUTHORIZED)
        (var-set contract-admin new-admin)
        (ok true)
    )
)
