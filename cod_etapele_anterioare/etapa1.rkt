#lang racket
(require "suffix-tree.rkt")

(provide (all-defined-out))

; TODO 2
; Implementați o funcție care primește două cuvinte (liste
; de caractere) w1 și w2 și calculează cel mai lung prefix
; comun al acestora, împreună cu restul celor două cuvinte
; după eliminarea prefixului comun.
; ex:
; (longest-common-prefix '(#\w #\h #\y) '(#\w #\h #\e #\n))
; => '((#\w #\h) (#\y) (#\e #\n))
; Folosiți recursivitate pe coadă.
(define (longest-common-prefix w1 w2)

      (helper-exercise-2 '() w1 w2)
  )
  

(define (helper-exercise-2 prefix w1 w2)

  (cond
    ((or (null? w1) (null? w2)) (list prefix w1 w2))
    ((equal? (car w1) (car w2)) (helper-exercise-2 (append prefix (list (car w1))) (cdr w1) (cdr w2)))
    (else (list prefix w1 w2))

    )

  )


; TODO 3
; Implementați recursiv o funcție care primește o listă nevidă 
; de cuvinte care încep cu același caracter și calculează cel 
; mai lung prefix comun al acestora.
; Opriți căutarea (parcurgerea) în momentul în care aveți garanția 
; că prefixul comun curent este prefixul comun final.
(define (longest-common-prefix-of-list words)

  (if (equal? (car (helper-exercise-3 words)) '())
      '()
      (car (helper-exercise-3 words))
      )
  
  )

(define (helper-exercise-3 words)

  (if (null? (cdr words))
      (list (car words))
      (longest-common-prefix (car (helper-exercise-3 (cdr words))) (car words))
      
      )

  )


;; Următoarele două funcții sunt utile căutării unui șablon
;; (pattern) într-un text cu ajutorul arborelui de sufixe.
;; Ideea de căutare este următoarea:
;; - dacă șablonul există în text, atunci există un sufix care
;;   începe cu acest șablon, deci există o cale care începe din
;;   rădăcina arborelui care se potrivește cu șablonul
;; - vom căuta ramura a cărei etichetă începe cu prima literă
;;   din șablon
;; - dacă nu găsim această ramură, șablonul nu apare în text
;; - dacă șablonul este conținut integral în eticheta ramurii,
;;   atunci el apare în text
;; - dacă șablonul se potrivește cu eticheta dar nu este conținut
;;   în ea (de exemplu șablonul "nana$" se potrivește cu eticheta
;;   "na"), atunci continuăm căutarea în subarborele ramurii
;; - dacă șablonul nu se potrivește cu eticheta (de exemplu
;;   șablonul "numai" nu se potrivește cu eticheta "na"), atunci
;;   el nu apare în text (altfel, eticheta ar fi fost "n", nu
;;   "na", pentru că eticheta este cel mai lung prefix comun al
;;   sufixelor din subarborele său)


; TODO 4
; Implementați funcția match-pattern-with-label care primește un
; arbore de sufixe și un șablon nevid și realizează un singur pas 
; din procesul prezentat mai sus - identifică ramura arborelui a
; cărei etichetă începe cu prima literă din șablon, apoi
; determină cât de bine se potrivește șablonul cu eticheta,
; întorcând ca rezultat:
; - true, dacă șablonul este conținut integral în etichetă
; - lista (etichetă, nou pattern, subarbore), dacă șablonul se
;   potrivește cu eticheta dar nu este conținut în ea
;   (ex: ("na", "na$", subarborele de sub eticheta "na")
;   pentru șablonul inițial "nana$" și eticheta "na")
; - lista (false, cel mai lung prefix comun între etichetă și
;   șablon), dacă șablonul nu s-a potrivit cu eticheta sau nu
;   s-a găsit din start o etichetă care începe cu litera dorită
;   (ex1: (false, "n") pentru șablonul "numai" și eticheta "na")
;   (ex2: (false, "") pentru etichetă negăsită)
; Obs: deși exemplele folosesc stringuri pentru claritate, vă
; reamintim că în realitate lucrăm cu liste de caractere.
(define (match-pattern-with-label st pattern)

  (if (equal? (get-ch-branch st (car pattern)) #f)
      (list #f '())
      (helper-exercise-4 st pattern)

      )
  
  )

(define (helper-exercise-4 st pattern)

  (let ((branch-label (get-branch-label (get-ch-branch st (car pattern))))
        (longest-c-prefix (car (longest-common-prefix pattern (get-branch-label (get-ch-branch st (car pattern)))))))
    (cond
      ((= (length longest-c-prefix) (length pattern)) #t)
      ((< (length longest-c-prefix) (length branch-label)) (list #f longest-c-prefix))
      (else (list longest-c-prefix (drop pattern (length longest-c-prefix)) (cdr (get-ch-branch st (car pattern)))
                  )
            )
      )
    )
  )


; varianta fara let
  #|
  (cond
    ((= (length (car (longest-common-prefix pattern (get-branch-label (get-ch-branch st (car pattern)))))) (length pattern)) #t)

    ((< (length (car (longest-common-prefix pattern (get-branch-label (get-ch-branch st (car pattern)))))) (length (get-branch-label (get-ch-branch st (car pattern)))))
     (list #f (car (longest-common-prefix pattern (get-branch-label (get-ch-branch st (car pattern))))))
     )
    
    (else (list (car (longest-common-prefix pattern (get-branch-label (get-ch-branch st (car pattern)))))
             (drop pattern (length (car (longest-common-prefix pattern (get-branch-label (get-ch-branch st (car pattern)))))))
             (cdr (get-ch-branch st (car pattern)))
            )
          )
    )

  )
|#

 
; TODO 5
; Implementați funcția st-has-pattern? care primește un
; arbore de sufixe și un șablon și întoarce true dacă șablonul
; apare în arbore, respectiv false în caz contrar.


(define (st-has-pattern? st pattern)

  (cond
    ((null? st) #t)
    ((null? pattern) #t)
    ((equal? (match-pattern-with-label st pattern) #t) #t)
    ; mai jos este cazul in care ajungem pe frunza:
    ((and (null? (cdr (cdr (match-pattern-with-label st pattern)))) (not (equal? (longest-common-prefix (get-branch-label st) pattern) pattern))) #f)
    (else (st-has-pattern? (car (cdr (cdr (match-pattern-with-label st pattern)))) (car (cdr (match-pattern-with-label st pattern)))))
    )

  )









