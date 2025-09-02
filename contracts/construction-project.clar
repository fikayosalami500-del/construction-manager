
;; title: construction-project
;; version: 1.0.0
;; summary: Building development platform with permit tracking, inspection scheduling, contractor payments, and project milestone verification
;; description: Smart contract for managing construction projects including permit tracking, inspections, contractor payments, and milestone verification

;; constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-UNAUTHORIZED (err u102))
(define-constant ERR-INVALID-STATUS (err u103))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u104))
(define-constant ERR-ALREADY-EXISTS (err u105))

;; data vars
(define-data-var project-nonce uint u0)

;; data maps
(define-map projects 
  uint 
  {
    owner: principal,
    contractor: (optional principal),
    title: (string-ascii 100),
    description: (string-ascii 500),
    budget: uint,
    paid: uint,
    permit-status: (string-ascii 20),
    project-status: (string-ascii 20),
    created-at: uint,
    completion-date: (optional uint)
  }
)

(define-map inspections
  {project-id: uint, inspection-id: uint}
  {
    inspector: principal,
    scheduled-date: uint,
    status: (string-ascii 20),
    notes: (string-ascii 300),
    created-at: uint
  }
)

(define-map milestones
  {project-id: uint, milestone-id: uint}
  {
    title: (string-ascii 100),
    description: (string-ascii 300),
    payment-amount: uint,
    status: (string-ascii 20),
    completed-at: (optional uint),
    created-at: uint
  }
)

(define-map project-counters
  uint
  {inspection-count: uint, milestone-count: uint}
)

;; public functions

;; Create a new construction project
(define-public (create-project (title (string-ascii 100)) (description (string-ascii 500)) (budget uint))
  (let
    (
      (project-id (+ (var-get project-nonce) u1))
    )
    (asserts! (> budget u0) ERR-INSUFFICIENT-PAYMENT)
    (map-set projects project-id
      {
        owner: tx-sender,
        contractor: none,
        title: title,
        description: description,
        budget: budget,
        paid: u0,
        permit-status: "pending",
        project-status: "planning",
        created-at: stacks-block-height,
        completion-date: none
      }
    )
    (map-set project-counters project-id {inspection-count: u0, milestone-count: u0})
    (var-set project-nonce project-id)
    (ok project-id)
  )
)

;; Assign contractor to project
(define-public (assign-contractor (project-id uint) (contractor principal))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR-NOT-FOUND))
    )
    (asserts! (is-eq (get owner project) tx-sender) ERR-UNAUTHORIZED)
    (map-set projects project-id
      (merge project {contractor: (some contractor)})
    )
    (ok true)
  )
)

;; Update permit status
(define-public (update-permit-status (project-id uint) (status (string-ascii 20)))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR-NOT-FOUND))
    )
    (asserts! (is-eq (get owner project) tx-sender) ERR-UNAUTHORIZED)
    (map-set projects project-id
      (merge project {permit-status: status})
    )
    (ok true)
  )
)

;; Schedule inspection
(define-public (schedule-inspection (project-id uint) (inspector principal) (scheduled-date uint) (notes (string-ascii 300)))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR-NOT-FOUND))
      (counters (unwrap! (map-get? project-counters project-id) ERR-NOT-FOUND))
      (inspection-id (+ (get inspection-count counters) u1))
    )
    (asserts! (is-eq (get owner project) tx-sender) ERR-UNAUTHORIZED)
    (map-set inspections {project-id: project-id, inspection-id: inspection-id}
      {
        inspector: inspector,
        scheduled-date: scheduled-date,
        status: "scheduled",
        notes: notes,
        created-at: stacks-block-height
      }
    )
    (map-set project-counters project-id
      (merge counters {inspection-count: inspection-id})
    )
    (ok inspection-id)
  )
)

;; Complete inspection
(define-public (complete-inspection (project-id uint) (inspection-id uint) (status (string-ascii 20)) (notes (string-ascii 300)))
  (let
    (
      (inspection (unwrap! (map-get? inspections {project-id: project-id, inspection-id: inspection-id}) ERR-NOT-FOUND))
    )
    (asserts! (is-eq (get inspector inspection) tx-sender) ERR-UNAUTHORIZED)
    (map-set inspections {project-id: project-id, inspection-id: inspection-id}
      (merge inspection {status: status, notes: notes})
    )
    (ok true)
  )
)

;; Create milestone
(define-public (create-milestone (project-id uint) (title (string-ascii 100)) (description (string-ascii 300)) (payment-amount uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR-NOT-FOUND))
      (counters (unwrap! (map-get? project-counters project-id) ERR-NOT-FOUND))
      (milestone-id (+ (get milestone-count counters) u1))
    )
    (asserts! (is-eq (get owner project) tx-sender) ERR-UNAUTHORIZED)
    (asserts! (> payment-amount u0) ERR-INSUFFICIENT-PAYMENT)
    (map-set milestones {project-id: project-id, milestone-id: milestone-id}
      {
        title: title,
        description: description,
        payment-amount: payment-amount,
        status: "pending",
        completed-at: none,
        created-at: stacks-block-height
      }
    )
    (map-set project-counters project-id
      (merge counters {milestone-count: milestone-id})
    )
    (ok milestone-id)
  )
)

;; Complete milestone
(define-public (complete-milestone (project-id uint) (milestone-id uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR-NOT-FOUND))
      (milestone (unwrap! (map-get? milestones {project-id: project-id, milestone-id: milestone-id}) ERR-NOT-FOUND))
      (contractor (unwrap! (get contractor project) ERR-NOT-FOUND))
    )
    (asserts! (is-eq (get owner project) tx-sender) ERR-UNAUTHORIZED)
    (asserts! (is-eq (get status milestone) "pending") ERR-INVALID-STATUS)
    
    ;; Mark milestone as completed
    (map-set milestones {project-id: project-id, milestone-id: milestone-id}
      (merge milestone {status: "completed", completed-at: (some stacks-block-height)})
    )
    
    ;; Update project paid amount
    (map-set projects project-id
      (merge project {paid: (+ (get paid project) (get payment-amount milestone))})
    )
    
    ;; Transfer payment to contractor
    (try! (stx-transfer? (get payment-amount milestone) tx-sender contractor))
    
    (ok true)
  )
)

;; Update project status
(define-public (update-project-status (project-id uint) (status (string-ascii 20)))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR-NOT-FOUND))
    )
    (asserts! (is-eq (get owner project) tx-sender) ERR-UNAUTHORIZED)
    (let
      (
        (updated-project (merge project {project-status: status}))
      )
      (map-set projects project-id
        (if (is-eq status "completed")
          (merge updated-project {completion-date: (some stacks-block-height)})
          updated-project
        )
      )
    )
    (ok true)
  )
)

;; read only functions

;; Get project details
(define-read-only (get-project (project-id uint))
  (map-get? projects project-id)
)

;; Get inspection details
(define-read-only (get-inspection (project-id uint) (inspection-id uint))
  (map-get? inspections {project-id: project-id, inspection-id: inspection-id})
)

;; Get milestone details
(define-read-only (get-milestone (project-id uint) (milestone-id uint))
  (map-get? milestones {project-id: project-id, milestone-id: milestone-id})
)

;; Get project counters
(define-read-only (get-project-counters (project-id uint))
  (map-get? project-counters project-id)
)

;; Get current project nonce
(define-read-only (get-project-nonce)
  (var-get project-nonce)
)
