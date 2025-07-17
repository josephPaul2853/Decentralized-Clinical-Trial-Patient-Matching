;; Clinical Trial Patient Matching System
;; Handles patient registration, trial matching, and progress tracking

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-input (err u103))
(define-constant err-not-authorized (err u104))
(define-constant err-trial-full (err u105))
(define-constant err-not-eligible (err u106))
(define-constant err-trial-ended (err u107))

;; Data Variables
(define-data-var trial-counter uint u0)
(define-data-var patient-counter uint u0)
(define-data-var regulatory-authority (optional principal) none)

;; Patient Profile Structure
(define-map patient-profiles
  { patient-id: uint }
  {
    patient-address: principal,
    age-group: uint,              ;; 1=18-30, 2=31-50, 3=51-65, 4=65+
    medical-conditions: (list 10 uint), ;; Encoded condition IDs
    medications: (list 10 uint),   ;; Encoded medication IDs
    privacy-hash: (buff 32),       ;; Hash of sensitive data
    registration-block: uint,
    active: bool,
    consent-version: uint
  }
)

;; Clinical Trial Structure
(define-map clinical-trials
  { trial-id: uint }
  {
    sponsor: principal,
    trial-name: (string-ascii 100),
    phase: uint,                   ;; 1=Phase I, 2=Phase II, 3=Phase III, 4=Phase IV
    required-conditions: (list 10 uint),
    excluded-conditions: (list 10 uint),
    age-min: uint,
    age-max: uint,
    target-enrollment: uint,
    current-enrollment: uint,
    compensation-per-visit: uint,
    total-visits: uint,
    start-block: uint,
    end-block: uint,
    status: uint,                  ;; 0=recruiting, 1=active, 2=completed, 3=terminated
    regulatory-approved: bool
  }
)

;; Patient-Trial Matching
(define-map patient-trials
  { patient-id: uint, trial-id: uint }
  {
    match-score: uint,             ;; 0-100 compatibility score
    enrollment-block: uint,
    status: uint,                  ;; 0=matched, 1=enrolled, 2=completed, 3=withdrawn
    visits-completed: uint,
    total-compensation: uint,
    last-visit-block: uint
  }
)

;; Trial Progress Tracking
(define-map trial-visits
  { patient-id: uint, trial-id: uint, visit-number: uint }
  {
    visit-block: uint,
    completion-status: bool,
    compensation-paid: uint,
    data-hash: (buff 32)          ;; Hash of visit data
  }
)

;; Eligibility Criteria Templates
(define-map eligibility-criteria
  { criteria-id: uint }
  {
    condition-requirements: (list 10 uint),
    age-requirements: { min: uint, max: uint },
    medication-restrictions: (list 10 uint),
    active: bool
  }
)

;; Compensation Distribution Tracking
(define-map compensation-ledger
  { patient-id: uint, trial-id: uint }
  {
    total-earned: uint,
    total-paid: uint,
    pending-amount: uint,
    payment-schedule: (list 20 uint), ;; Block heights for payments
    last-payment-block: uint
  }
)

;; Read-only functions

(define-read-only (get-patient-profile (patient-id uint))
  (map-get? patient-profiles { patient-id: patient-id })
)

(define-read-only (get-trial-info (trial-id uint))
  (map-get? clinical-trials { trial-id: trial-id })
)

(define-read-only (get-patient-trial-status (patient-id uint) (trial-id uint))
  (map-get? patient-trials { patient-id: patient-id, trial-id: trial-id })
)

(define-read-only (get-trial-counter)
  (var-get trial-counter)
)

(define-read-only (get-patient-counter)
  (var-get patient-counter)
)

(define-read-only (calculate-matching-score (patient-id uint) (trial-id uint))
  (let (
    (patient-data (unwrap! (get-patient-profile patient-id) u0))
    (trial-data (unwrap! (get-trial-info trial-id) u0))
    (patient-conditions (get medical-conditions patient-data))
    (required-conditions (get required-conditions trial-data))
    (excluded-conditions (get excluded-conditions trial-data))
  )
    (let (
      (condition-match (calculate-condition-compatibility patient-conditions required-conditions))
      (exclusion-penalty (calculate-exclusion-penalty patient-conditions excluded-conditions))
      (age-match (calculate-age-compatibility (get age-group patient-data)
                                            (get age-min trial-data)
                                            (get age-max trial-data)))
    )
      (if (> exclusion-penalty u0)
        u0  ;; Ineligible due to exclusion criteria
        (/ (+ condition-match age-match) u2)
      )
    )
  )
)

(define-read-only (calculate-condition-compatibility (patient-conditions (list 10 uint)) (required-conditions (list 10 uint)))
  (let (
    (matches (fold check-condition-match required-conditions u0))
    (total-required (len required-conditions))
  )
    (if (is-eq total-required u0)
      u100
      (/ (* matches u100) total-required)
    )
  )
)

(define-read-only (calculate-exclusion-penalty (patient-conditions (list 10 uint)) (excluded-conditions (list 10 uint)))
  (fold check-exclusion-violation excluded-conditions u0)
)

(define-read-only (calculate-age-compatibility (patient-age-group uint) (min-age uint) (max-age uint))
  (let (
    (patient-range (get-age-range patient-age-group))
  )
    (if (and (>= (get max patient-range) min-age) (<= (get min patient-range) max-age))
      u100
      u0
    )
  )
)

;; Check if patient is eligible for a specific trial
(define-read-only (is-patient-eligible-for-trial (patient-id uint) (trial-id uint))
  (is-eligible-for-trial patient-id trial-id)
)

(define-read-only (get-compensation-summary (patient-id uint) (trial-id uint))
  (map-get? compensation-ledger { patient-id: patient-id, trial-id: trial-id })
)

;; Private helper functions

(define-private (check-condition-match (condition uint) (acc uint))
  (+ acc (if (> condition u0) u1 u0))
)

(define-private (check-exclusion-violation (condition uint) (acc uint))
  (if (> condition u0) u1 acc)
)

(define-private (get-age-range (age-group uint))
  (if (is-eq age-group u1)
    { min: u18, max: u30 }
    (if (is-eq age-group u2)
      { min: u31, max: u50 }
      (if (is-eq age-group u3)
        { min: u51, max: u65 }
        (if (is-eq age-group u4)
          { min: u66, max: u100 }
          { min: u0, max: u0 }  ;; Default for invalid age group
        )
      )
    )
  )
)

(define-private (is-eligible-for-trial (patient-id uint) (trial-id uint))
  (let (
    (match-score (calculate-matching-score patient-id trial-id))
    (trial-data (unwrap! (get-trial-info trial-id) false))
  )
    (and
      (> match-score u70)  ;; Minimum 70% match
      (is-eq (get status trial-data) u0)  ;; Trial is recruiting
      (< (get current-enrollment trial-data) (get target-enrollment trial-data))  ;; Not full
      (get regulatory-approved trial-data)  ;; Approved by regulators
    )
  )
)

;; Public functions

(define-public (register-patient (age-group uint) (medical-conditions (list 10 uint)) (medications (list 10 uint)) (privacy-hash (buff 32)))
  (let (
    (patient-id (+ (var-get patient-counter) u1))
  )
    (asserts! (and (>= age-group u1) (<= age-group u4)) err-invalid-input)
    (asserts! (< (len medical-conditions) u11) err-invalid-input)
    (asserts! (< (len medications) u11) err-invalid-input)

    (map-set patient-profiles
      { patient-id: patient-id }
      {
        patient-address: tx-sender,
        age-group: age-group,
        medical-conditions: medical-conditions,
        medications: medications,
        privacy-hash: privacy-hash,
        registration-block: stacks-block-height,
        active: true,
        consent-version: u1
      }
    )

    (var-set patient-counter patient-id)
    (ok patient-id)
  )
)

(define-public (create-trial (trial-name (string-ascii 100)) (phase uint) (required-conditions (list 10 uint)) (excluded-conditions (list 10 uint)) (age-min uint) (age-max uint) (target-enrollment uint) (compensation-per-visit uint) (total-visits uint) (duration-blocks uint))
  (let (
    (trial-id (+ (var-get trial-counter) u1))
    (end-block (+ stacks-block-height duration-blocks))
  )
    (asserts! (and (>= phase u1) (<= phase u4)) err-invalid-input)
    (asserts! (< age-min age-max) err-invalid-input)
    (asserts! (> target-enrollment u0) err-invalid-input)
    (asserts! (> total-visits u0) err-invalid-input)

    (map-set clinical-trials
      { trial-id: trial-id }
      {
        sponsor: tx-sender,
        trial-name: trial-name,
        phase: phase,
        required-conditions: required-conditions,
        excluded-conditions: excluded-conditions,
        age-min: age-min,
        age-max: age-max,
        target-enrollment: target-enrollment,
        current-enrollment: u0,
        compensation-per-visit: compensation-per-visit,
        total-visits: total-visits,
        start-block: stacks-block-height,
        end-block: end-block,
        status: u0,
        regulatory-approved: false
      }
    )

    (var-set trial-counter trial-id)
    (ok trial-id)
  )
)

(define-public (enroll-patient (patient-id uint) (trial-id uint))
  (let (
    (patient-data (unwrap! (get-patient-profile patient-id) err-not-found))
    (trial-data (unwrap! (get-trial-info trial-id) err-not-found))
    (match-score (calculate-matching-score patient-id trial-id))
  )
    (asserts! (is-eq (get patient-address patient-data) tx-sender) err-not-authorized)
    (asserts! (> match-score u70) err-not-eligible)
    (asserts! (< (get current-enrollment trial-data) (get target-enrollment trial-data)) err-trial-full)
    (asserts! (is-eq (get status trial-data) u0) err-trial-ended)
    (asserts! (get regulatory-approved trial-data) err-not-authorized)

    ;; Update patient-trial mapping
    (map-set patient-trials
      { patient-id: patient-id, trial-id: trial-id }
      {
        match-score: match-score,
        enrollment-block: stacks-block-height,
        status: u1,
        visits-completed: u0,
        total-compensation: u0,
        last-visit-block: stacks-block-height
      }
    )

    ;; Update trial enrollment count
    (map-set clinical-trials
      { trial-id: trial-id }
      (merge trial-data { current-enrollment: (+ (get current-enrollment trial-data) u1) })
    )

    ;; Initialize compensation ledger
    (map-set compensation-ledger
      { patient-id: patient-id, trial-id: trial-id }
      {
        total-earned: u0,
        total-paid: u0,
        pending-amount: u0,
        payment-schedule: (list),
        last-payment-block: u0
      }
    )

    (ok true)
  )
)

(define-public (complete-visit (patient-id uint) (trial-id uint) (visit-number uint) (data-hash (buff 32)))
  (let (
    (patient-trial (unwrap! (get-patient-trial-status patient-id trial-id) err-not-found))
    (trial-data (unwrap! (get-trial-info trial-id) err-not-found))
    (patient-data (unwrap! (get-patient-profile patient-id) err-not-found))
  )
    (asserts! (is-eq (get patient-address patient-data) tx-sender) err-not-authorized)
    (asserts! (is-eq (get status patient-trial) u1) err-not-authorized)
    (asserts! (<= visit-number (get total-visits trial-data)) err-invalid-input)

    ;; Record visit completion
    (map-set trial-visits
      { patient-id: patient-id, trial-id: trial-id, visit-number: visit-number }
      {
        visit-block: stacks-block-height,
        completion-status: true,
        compensation-paid: (get compensation-per-visit trial-data),
        data-hash: data-hash
      }
    )

    ;; Update patient-trial progress
    (map-set patient-trials
      { patient-id: patient-id, trial-id: trial-id }
      (merge patient-trial {
        visits-completed: (+ (get visits-completed patient-trial) u1),
        total-compensation: (+ (get total-compensation patient-trial) (get compensation-per-visit trial-data)),
        last-visit-block: stacks-block-height
      })
    )

    ;; Update compensation ledger
    (let (
      (comp-data (unwrap! (get-compensation-summary patient-id trial-id) err-not-found))
    )
      (map-set compensation-ledger
        { patient-id: patient-id, trial-id: trial-id }
        (merge comp-data {
          total-earned: (+ (get total-earned comp-data) (get compensation-per-visit trial-data)),
          pending-amount: (+ (get pending-amount comp-data) (get compensation-per-visit trial-data))
        })
      )
    )

    (ok true)
  )
)

(define-public (approve-trial (trial-id uint))
  (let (
    (trial-data (unwrap! (get-trial-info trial-id) err-not-found))
    (regulator (unwrap! (var-get regulatory-authority) err-not-authorized))
  )
    (asserts! (is-eq tx-sender regulator) err-not-authorized)

    (map-set clinical-trials
      { trial-id: trial-id }
      (merge trial-data { regulatory-approved: true })
    )

    (ok true)
  )
)

(define-public (set-regulatory-authority (authority principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set regulatory-authority (some authority))
    (ok true)
  )
)

(define-public (distribute-compensation (patient-id uint) (trial-id uint))
  (let (
    (trial-data (unwrap! (get-trial-info trial-id) err-not-found))
    (comp-data (unwrap! (get-compensation-summary patient-id trial-id) err-not-found))
    (patient-data (unwrap! (get-patient-profile patient-id) err-not-found))
  )
    (asserts! (is-eq tx-sender (get sponsor trial-data)) err-not-authorized)
    (asserts! (> (get pending-amount comp-data) u0) err-invalid-input)

    ;; Transfer compensation (simplified - in production would use actual STX transfer)
    (map-set compensation-ledger
      { patient-id: patient-id, trial-id: trial-id }
      (merge comp-data {
        total-paid: (+ (get total-paid comp-data) (get pending-amount comp-data)),
        pending-amount: u0,
        last-payment-block: stacks-block-height
      })
    )

    (ok (get pending-amount comp-data))
  )
)

(define-public (update-trial-status (trial-id uint) (new-status uint))
  (let (
    (trial-data (unwrap! (get-trial-info trial-id) err-not-found))
  )
    (asserts! (is-eq tx-sender (get sponsor trial-data)) err-not-authorized)
    (asserts! (<= new-status u3) err-invalid-input)

    (map-set clinical-trials
      { trial-id: trial-id }
      (merge trial-data { status: new-status })
    )

    (ok true)
  )
)

(define-public (withdraw-from-trial (patient-id uint) (trial-id uint))
  (let (
    (patient-trial (unwrap! (get-patient-trial-status patient-id trial-id) err-not-found))
    (patient-data (unwrap! (get-patient-profile patient-id) err-not-found))
    (trial-data (unwrap! (get-trial-info trial-id) err-not-found))
  )
    (asserts! (is-eq (get patient-address patient-data) tx-sender) err-not-authorized)
    (asserts! (is-eq (get status patient-trial) u1) err-not-authorized)

    ;; Update patient-trial status to withdrawn
    (map-set patient-trials
      { patient-id: patient-id, trial-id: trial-id }
      (merge patient-trial { status: u3 })
    )

    ;; Update trial enrollment count
    (map-set clinical-trials
      { trial-id: trial-id }
      (merge trial-data { current-enrollment: (- (get current-enrollment trial-data) u1) })
    )

    (ok true)
  )
)
