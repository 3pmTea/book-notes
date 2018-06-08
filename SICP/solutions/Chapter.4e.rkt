; ========== E4.55
(supervisor ?x (Bitdiddle Ben))

(job ?name (accounting . ?j))

(address ?name (Slumerville . ?addr))

; ========== E4.56
(and (supervisor ?name (Bitdiddle Ben))
     (address ?name ?addr))

(or (salary (Bitdiddle Ben) ?salary-Ben)
    (and (salary ?person ?salary)
         (list-value < ?salary ?salary-Ben)))

(or (and (not (job ?person (computer . ?type)))
         (supervisor ?somebody ?person))
    (job ?person ?x))

; ========== E4.57
(rule (same-job ?person1 ?person2)
      (and (job ?person1 ?ajob)
           (job ?person2 ?ajob)))

(rule (can-replace ?person1 ?person2)
      (and (or (same-job ?person1 ?person2)
               (and (job ?person1 ?job1)
                    (job ?person2 ?job2)
                    (can-do-job ?job1 ?job2)))
           (not (same ?person1 ?person2))))
; a
(can-replace ?person (Fect Cy D))
; b
(and (can-replace ?person1 ?person2)
     (salary ?person1 ?salary1)
     (salary ?person2 ?salary2)
     (list-value < ?salary1 ?salary2))

; ========== E4.58
(rule (big-shot ?person)
      (not (and (job ?person (?division . ?rest1))
                (job ?supervisor (?division . ?rest2))
                (supervisor ?person ?supervisor))))
