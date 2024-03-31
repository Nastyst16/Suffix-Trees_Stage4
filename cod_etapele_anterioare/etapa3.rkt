#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")
(require "etapa2.rkt")

(provide (all-defined-out))

;; Această etapă este dedicată aplicațiilor 
;; arborelui de sufixe:
;; - găsirea unui șablon într-un text
;; - cel mai lung subșir comun a două texte
;; - găsirea unui subșir de lungime dată care se
;;   repetă în text
;; Conform convenției din etapele anterioare, un text
;; este întotdeauna reprezentat ca listă de caractere.
;; Rezultatele funcțiilor de mai jos sunt de asemenea
;; reprezentate ca liste de caractere.


; TODO 1
; Implementați funcția substring? care primește un text și
; un șablon nevid și întoarce true dacă șablonul apare în 
; text, respectiv false în caz contrar.
; Observație: ați implementat deja logica principală a
; acestei căutări în etapa 1, în funcția st-has-pattern?,
; care este un operator al tipului ST. Acum aveți toate
; uneltele necesare implementării operatorului corespunzător
; pentru tipul text (pentru că în etapa 2 ați implementat
; construcția arborelui de sufixe asociat unui text).
(define (substring? text pattern)

  (st-has-pattern? (text->ast text) pattern)

  )


; TODO 2
; Implementați funcția longest-common-substring care primește
; două texte și determină cel mai lung subșir comun al
; acestora, folosind algoritmul următor:
; 1. Construiește arborele de sufixe ST1 pentru primul text.
; 2. Pentru fiecare sufix din al doilea text (de la cel mai
;    lung la cel mai scurt), găsește cea mai lungă potrivire 
;    cu sufixele din primul text, urmând căile relevante în ST1.
; 3. Rezultatul final este cel mai lung rezultat identificat
;    la pasul 2 (în caz de egalitate a lungimii, păstrăm
;    primul șir găsit).
; Folosiți named let pentru a parcurge sufixele.
; Observație: pentru sufixele din al doilea text nu dorim 
; marcajul de final $ pentru a nu crește artificial lungimea 
; șirului comun cu acest caracter.
; Hint: Revizitați funcția match-pattern-with-label (etapa 1).
(define (longest-common-substring text1 text2)

  (let ((st-text1 (text->ast text1))
         (suffixes-text2 (get-suffixes text2))
         )

    (let iter ((st st-text1) (suffixes suffixes-text2) (rez '()))
    (cond
      ((null? suffixes) rez)
      ((< (length rez) (length(helper-builder st (car suffixes)))) (iter st (cdr suffixes) (helper-builder st (car suffixes))))
      (else (iter st (cdr suffixes) rez)))
      )
    )
  )

(define (helper-builder st suffix)

  (if (st-has-pattern? st suffix)
      suffix
      (helper-builder st (drop-right suffix 1))
      )
  )

(define input-list (string->list "xabxabxaaxbbxabxabxaaxbb"))
; TODO 3
; Implementați funcția repeated-substring-of-given-length
; care primește un text și un număr natural len și
; parcurge arborele de sufixe al textului până găsește un
; subșir de lungime len care se repetă în text.
; Dacă acest subșir nu există, funcția întoarce false.
; Obs: din felul în care este construit arborele de sufixe
; (pe baza alfabetului sortat), rezultatul va fi primul 
; asemenea subșir din punct de vedere alfabetic.
; Ideea este următoarea: orice cale în arborele de sufixe
; compact care se termină cu un nod intern (un nod care 
; are copii, nu este o frunză) reprezintă un subșir care
; se repetă, pentru că orice asemenea cale reprezintă un
; prefix comun pentru două sau mai multe sufixe ale textului.
; Folosiți interfața definită în fișierul suffix-tree
; atunci când manipulați arborele.
(define (repeated-substring-of-given-length text len)

  (let* ((st (text->cst text))
         (rez (helper-calling-by-every-branch st len '()))
         )

    (if (null? rez)
        #f
        rez
        )
    )
  )


(define (helper-calling-by-every-branch st len rez)

  (let ((other-branches (other-branches st))
        (first-branch (first-branch st))
        )

    (cond
      ((st-empty? st) '())
      ; conditie pentru eficienta
      ((not (list? first-branch)) (helper-calling-by-every-branch other-branches len rez)) ; nodul este o frunza;
      
      (else

       (let ((substring-found (helper-for-each-branch first-branch len rez)))

         (if (>= (length substring-found) len)
             (take substring-found len)
             (helper-calling-by-every-branch other-branches len rez)
             )
         )
     
       )
    
      )
    )
  )

(define (helper-for-each-branch st len substring)
  
  (cond 
      ((null? st) substring )
      ((not (list? (first-branch st))) substring ) ; suntem pe frunza
      ((and (pair? st) (= (length st) 1)) substring)

      (else

       (let* ((label-root (get-branch-label st))
              (appended-substring (append substring label-root))
              )

         (if (>= (length appended-substring) len)
             appended-substring
             (helper-calling-by-every-branch st len appended-substring)
             )
         
         )
       )
      )
  )
