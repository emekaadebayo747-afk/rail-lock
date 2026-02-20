;; RailLock - Decentralized Impact Investing Platform

;; ============================================================
;; CONSTANTS
;; ============================================================

(define-constant CONTRACT-OWNER tx-sender)

(define-constant ERR-NOT-AUTHORIZED        (err u100))
(define-constant ERR-PROJECT-NOT-FOUND     (err u101))
(define-constant ERR-MILESTONE-NOT-FOUND   (err u102))
(define-constant ERR-ALREADY-EXISTS        (err u103))
(define-constant ERR-INVALID-AMOUNT        (err u104))
(define-constant ERR-MILESTONE-COMPLETE    (err u105))
(define-constant ERR-PROJECT-CLOSED        (err u106))
(define-constant ERR-INSUFFICIENT-BALANCE  (err u107))
(define-constant ERR-INSURANCE-POOL-EMPTY  (err u108))
(define-constant ERR-INVALID-SCORE         (err u109))
(define-constant ERR-ALREADY-VOTED        (err u110))

;; Impact score boundaries (0-100)
(define-constant MAX-IMPACT-SCORE u100)

;; Insurance pool contribution rate: 5% of each investment (in basis points)
(define-constant INSURANCE-RATE-BPS u500)
(define-constant BPS-DENOMINATOR u10000)

;; ============================================================
;; DATA MAPS AND VARS
;; ============================================================

;; --- RAIL Token (fungible) ---
(define-fungible-token RAIL)

;; --- LOCK Token (fungible, minted on milestone verification) ---
(define-fungible-token LOCK)

;; --- Global counters ---
(define-data-var next-project-id uint u1)
(define-data-var next-milestone-id uint u1)
(define-data-var insurance-pool-balance uint u0)

;; --- Projects (Impact Railways) ---
;; Status: u0 = active, u1 = completed, u2 = closed/failed
(define-map projects
  { project-id: uint }
  {
    owner: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    total-funding-goal: uint,
    total-funded: uint,
    total-milestones: uint,
    milestones-completed: uint,
    status: uint,
    impact-score: uint,
    created-at: uint
  }
)

;; --- Milestones ---
;; Status: u0 = pending, u1 = verified, u2 = failed
(define-map milestones
  { milestone-id: uint }
  {
    project-id: uint,
    title: (string-ascii 100),
    description: (string-ascii 300),
    funding-tranche: uint,    ;; STX locked for this milestone
    status: uint,
    oracle-data-hash: (optional (buff 32)),  ;; hash of off-chain verification data
    validator-votes-yes: uint,
    validator-votes-no: uint,
    votes-needed: uint,
    completed-at: (optional uint)
  }
)

;; --- Per-project milestone list (index -> milestone-id) ---
(define-map project-milestones
  { project-id: uint, index: uint }
  { milestone-id: uint }
)

;; --- Investor positions ---
(define-map investments
  { investor: principal, project-id: uint }
  {
    amount: uint,          ;; total STX committed
    rail-minted: uint,     ;; RAIL tokens received
    lock-earned: uint,     ;; LOCK tokens earned so far
    last-claimed-milestone: uint
  }
)

;; --- Validator registry ---
(define-map validators
  { validator: principal }
  { is-active: bool, votes-cast: uint }
)

;; --- Validator votes per milestone (prevent double voting) ---
(define-map validator-votes
  { validator: principal, milestone-id: uint }
  { voted: bool, vote: bool }
)

;; --- Dynamic NFT: Impact Certificate ---
;; One per investor per project; evolves as milestones complete
(define-non-fungible-token impact-certificate uint)
(define-data-var next-cert-id uint u1)

(define-map certificate-data
  { cert-id: uint }
  {
    owner: principal,
    project-id: uint,
    invested-amount: uint,
    impact-score-snapshot: uint,
    issued-at: uint
  }
)

;; ============================================================
;; PRIVATE HELPERS
;; ============================================================

(define-private (is-owner)
  (is-eq tx-sender CONTRACT-OWNER)
)

(define-private (is-validator (who principal))
  (match (map-get? validators { validator: who })
    v (get is-active v)
    false
  )
)

(define-private (calc-insurance-contribution (amount uint))
  (/ (* amount INSURANCE-RATE-BPS) BPS-DENOMINATOR)
)

;; ============================================================
;; ADMIN FUNCTIONS
;; ============================================================

;; Register a community validator
(define-public (register-validator (validator principal))
  (begin
    (asserts! (is-owner) ERR-NOT-AUTHORIZED)
    (map-set validators
      { validator: validator }
      { is-active: true, votes-cast: u0 }
    )
    (ok true)
  )
)

;; Deactivate a validator
(define-public (deactivate-validator (validator principal))
  (begin
    (asserts! (is-owner) ERR-NOT-AUTHORIZED)
    (match (map-get? validators { validator: validator })
      v (begin
          (map-set validators
            { validator: validator }
            (merge v { is-active: false })
          )
          (ok true)
        )
      ERR-NOT-AUTHORIZED
    )
  )
)

;; ============================================================
;; PROJECT (IMPACT RAILWAY) MANAGEMENT
;; ============================================================

;; Propose a new Impact Railway project
(define-public (propose-project
    (title (string-ascii 100))
    (description (string-ascii 500))
    (total-funding-goal uint)
    (total-milestones uint))
  (let
    (
      (pid (var-get next-project-id))
    )
    (asserts! (> total-funding-goal u0) ERR-INVALID-AMOUNT)
    (asserts! (> total-milestones u0) ERR-INVALID-AMOUNT)
    (map-set projects
      { project-id: pid }
      {
        owner: tx-sender,
        title: title,
        description: description,
        total-funding-goal: total-funding-goal,
        total-funded: u0,
        total-milestones: total-milestones,
        milestones-completed: u0,
        status: u0,
        impact-score: u0,
        created-at: block-height
      }
    )
    (var-set next-project-id (+ pid u1))
    (ok pid)
  )
)

;; Add a milestone to a project (only project owner before any funding)
(define-public (add-milestone
    (project-id uint)
    (title (string-ascii 100))
    (description (string-ascii 300))
    (funding-tranche uint)
    (votes-needed uint))
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
      (mid (var-get next-milestone-id))
      (current-index (get total-milestones project))
    )
    (asserts! (is-eq tx-sender (get owner project)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status project) u0) ERR-PROJECT-CLOSED)
    (asserts! (> funding-tranche u0) ERR-INVALID-AMOUNT)
    (asserts! (> votes-needed u0) ERR-INVALID-AMOUNT)
    (map-set milestones
      { milestone-id: mid }
      {
        project-id: project-id,
        title: title,
        description: description,
        funding-tranche: funding-tranche,
        status: u0,
        oracle-data-hash: none,
        validator-votes-yes: u0,
        validator-votes-no: u0,
        votes-needed: votes-needed,
        completed-at: none
      }
    )
    ;; Track order within project
    (map-set project-milestones
      { project-id: project-id, index: mid }
      { milestone-id: mid }
    )
    (var-set next-milestone-id (+ mid u1))
    (ok mid)
  )
)

;; ============================================================
;; INVESTMENT (Progressive Impact Release)
;; ============================================================

;; Invest STX into a project; receive RAIL tokens 1:1 (in microSTX)
;; 5% goes to the insurance pool
(define-public (invest (project-id uint) (amount uint))
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
      (insurance-cut (calc-insurance-contribution amount))
      (net-investment (- amount insurance-cut))
      (existing (default-to
        { amount: u0, rail-minted: u0, lock-earned: u0, last-claimed-milestone: u0 }
        (map-get? investments { investor: tx-sender, project-id: project-id })
      ))
    )
    (asserts! (is-eq (get status project) u0) ERR-PROJECT-CLOSED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)

    ;; Transfer STX to contract (held in escrow)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))

    ;; Update insurance pool
    (var-set insurance-pool-balance (+ (var-get insurance-pool-balance) insurance-cut))

    ;; Mint RAIL tokens 1:1 with net investment (in microSTX units)
    (try! (ft-mint? RAIL net-investment tx-sender))

    ;; Update project funded total
    (map-set projects
      { project-id: project-id }
      (merge project { total-funded: (+ (get total-funded project) net-investment) })
    )

    ;; Record investment
    (map-set investments
      { investor: tx-sender, project-id: project-id }
      (merge existing
        {
          amount: (+ (get amount existing) net-investment),
          rail-minted: (+ (get rail-minted existing) net-investment)
        }
      )
    )
    (ok net-investment)
  )
)

;; ============================================================
;; MILESTONE VERIFICATION (Oracle + Community Validators)
;; ============================================================

;; Submit oracle data hash for a milestone (project owner or admin)
(define-public (submit-oracle-data (milestone-id uint) (data-hash (buff 32)))
  (let
    (
      (ms (unwrap! (map-get? milestones { milestone-id: milestone-id }) ERR-MILESTONE-NOT-FOUND))
      (project (unwrap! (map-get? projects { project-id: (get project-id ms) }) ERR-PROJECT-NOT-FOUND))
    )
    (asserts! (or (is-eq tx-sender (get owner project)) (is-owner)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status ms) u0) ERR-MILESTONE-COMPLETE)
    (map-set milestones
      { milestone-id: milestone-id }
      (merge ms { oracle-data-hash: (some data-hash) })
    )
    (ok true)
  )
)

;; Validator casts a vote on a milestone (true = verified, false = failed)
(define-public (vote-on-milestone (milestone-id uint) (vote bool))
  (let
    (
      (ms (unwrap! (map-get? milestones { milestone-id: milestone-id }) ERR-MILESTONE-NOT-FOUND))
      (vote-record (map-get? validator-votes { validator: tx-sender, milestone-id: milestone-id }))
      (validator-info (unwrap! (map-get? validators { validator: tx-sender }) ERR-NOT-AUTHORIZED))
    )
    (asserts! (get is-active validator-info) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status ms) u0) ERR-MILESTONE-COMPLETE)
    (asserts! (is-none vote-record) ERR-ALREADY-VOTED)

    ;; Record vote
    (map-set validator-votes
      { validator: tx-sender, milestone-id: milestone-id }
      { voted: true, vote: vote }
    )

    ;; Tally
    (let
      (
        (new-yes (if vote (+ (get validator-votes-yes ms) u1) (get validator-votes-yes ms)))
        (new-no  (if (not vote) (+ (get validator-votes-no ms) u1) (get validator-votes-no ms)))
        (needed  (get votes-needed ms))
      )
      (map-set milestones
        { milestone-id: milestone-id }
        (merge ms { validator-votes-yes: new-yes, validator-votes-no: new-no })
      )

      ;; Update validator stats
      (map-set validators
        { validator: tx-sender }
        (merge validator-info { votes-cast: (+ (get votes-cast validator-info) u1) })
      )

      ;; Auto-finalize if threshold reached
      (if (>= new-yes needed)
        (try! (finalize-milestone-verified milestone-id))
        true
      )
      (if (>= new-no needed)
        (try! (finalize-milestone-failed milestone-id))
        true
      )

      (ok true)
    )
  )
)

;; Internal: mark milestone as verified and release tranche funds
(define-private (finalize-milestone-verified (milestone-id uint))
  (let
    (
      (ms (unwrap! (map-get? milestones { milestone-id: milestone-id }) ERR-MILESTONE-NOT-FOUND))
      (project (unwrap! (map-get? projects { project-id: (get project-id ms) }) ERR-PROJECT-NOT-FOUND))
      (tranche (get funding-tranche ms))
    )
    ;; Mark milestone complete
    (map-set milestones
      { milestone-id: milestone-id }
      (merge ms { status: u1, completed-at: (some block-height) })
    )

    ;; Update project milestone count
    (let ((new-completed (+ (get milestones-completed project) u1)))
      (map-set projects
        { project-id: (get project-id ms) }
        (merge project
          {
            milestones-completed: new-completed,
            status: (if (>= new-completed (get total-milestones project)) u1 u0)
          }
        )
      )
    )

    ;; Release tranche to project owner
    (as-contract
      (stx-transfer? tranche tx-sender (get owner project))
    )
  )
)

;; Internal: mark milestone as failed; compensate from insurance pool
(define-private (finalize-milestone-failed (milestone-id uint))
  (let
    (
      (ms (unwrap! (map-get? milestones { milestone-id: milestone-id }) ERR-MILESTONE-NOT-FOUND))
      (tranche (get funding-tranche ms))
      (pool-bal (var-get insurance-pool-balance))
      (payout (if (>= pool-bal tranche) tranche pool-bal))
    )
    (map-set milestones
      { milestone-id: milestone-id }
      (merge ms { status: u2, completed-at: (some block-height) })
    )

    ;; Partial or full compensation from insurance pool back to project escrow
    (var-set insurance-pool-balance (- pool-bal payout))
    (ok true)
  )
)

;; ============================================================
;; LOCK TOKEN MINTING (Claim after milestone verified)
;; ============================================================

;; Investor claims LOCK tokens proportional to their share of a verified milestone
(define-public (claim-lock-tokens (project-id uint) (milestone-id uint))
  (let
    (
      (ms (unwrap! (map-get? milestones { milestone-id: milestone-id }) ERR-MILESTONE-NOT-FOUND))
      (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
      (investment (unwrap!
        (map-get? investments { investor: tx-sender, project-id: project-id })
        ERR-INSUFFICIENT-BALANCE
      ))
      (total-funded (get total-funded project))
    )
    (asserts! (is-eq (get project-id ms) project-id) ERR-MILESTONE-NOT-FOUND)
    (asserts! (is-eq (get status ms) u1) ERR-MILESTONE-NOT-FOUND)  ;; must be verified
    (asserts! (> total-funded u0) ERR-INVALID-AMOUNT)

    ;; Proportional LOCK tokens = (investor-amount / total-funded) * milestone-tranche
    (let
      (
        (investor-share (get amount investment))
        (lock-amount (/ (* investor-share (get funding-tranche ms)) total-funded))
      )
      (asserts! (> lock-amount u0) ERR-INVALID-AMOUNT)
      (try! (ft-mint? LOCK lock-amount tx-sender))
      (map-set investments
        { investor: tx-sender, project-id: project-id }
        (merge investment
          {
            lock-earned: (+ (get lock-earned investment) lock-amount),
            last-claimed-milestone: milestone-id
          }
        )
      )
      (ok lock-amount)
    )
  )
)

;; ============================================================
;; IMPACT CERTIFICATES (Dynamic NFT)
;; ============================================================

;; Mint an impact certificate for an investor in a project
(define-public (mint-impact-certificate (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
      (investment (unwrap!
        (map-get? investments { investor: tx-sender, project-id: project-id })
        ERR-INSUFFICIENT-BALANCE
      ))
      (cert-id (var-get next-cert-id))
    )
    (asserts! (> (get amount investment) u0) ERR-INVALID-AMOUNT)
    (try! (nft-mint? impact-certificate cert-id tx-sender))
    (map-set certificate-data
      { cert-id: cert-id }
      {
        owner: tx-sender,
        project-id: project-id,
        invested-amount: (get amount investment),
        impact-score-snapshot: (get impact-score project),
        issued-at: block-height
      }
    )
    (var-set next-cert-id (+ cert-id u1))
    (ok cert-id)
  )
)

;; Update the impact score of a project (oracle/admin driven)
(define-public (update-impact-score (project-id uint) (score uint))
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
    )
    (asserts! (or (is-owner) (is-eq tx-sender (get owner project))) ERR-NOT-AUTHORIZED)
    (asserts! (<= score MAX-IMPACT-SCORE) ERR-INVALID-SCORE)
    (map-set projects
      { project-id: project-id }
      (merge project { impact-score: score })
    )
    (ok score)
  )
)

;; ============================================================
;; GOVERNANCE: Community project close (majority validator vote)
;; ============================================================

;; Admin can force-close a failed/abandoned project
(define-public (close-project (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
    )
    (asserts! (or (is-owner) (is-eq tx-sender (get owner project))) ERR-NOT-AUTHORIZED)
    (map-set projects
      { project-id: project-id }
      (merge project { status: u2 })
    )
    (ok true)
  )
)

;; ============================================================
;; READ-ONLY FUNCTIONS
;; ============================================================

(define-read-only (get-project (project-id uint))
  (map-get? projects { project-id: project-id })
)

(define-read-only (get-milestone (milestone-id uint))
  (map-get? milestones { milestone-id: milestone-id })
)

(define-read-only (get-investment (investor principal) (project-id uint))
  (map-get? investments { investor: investor, project-id: project-id })
)

(define-read-only (get-certificate (cert-id uint))
  (map-get? certificate-data { cert-id: cert-id })
)

(define-read-only (get-insurance-pool-balance)
  (var-get insurance-pool-balance)
)

(define-read-only (get-rail-balance (who principal))
  (ft-get-balance RAIL who)
)

(define-read-only (get-lock-balance (who principal))
  (ft-get-balance LOCK who)
)

(define-read-only (get-validator-info (who principal))
  (map-get? validators { validator: who })
)

(define-read-only (get-next-project-id)
  (var-get next-project-id)
)

(define-read-only (get-next-milestone-id)
  (var-get next-milestone-id)
)
