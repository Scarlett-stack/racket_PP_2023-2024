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
  (define (vreau-acc w1 w2 acc)
  (cond 
  ((or (st-empty? w1) (st-empty? w2)) (list (reverse acc) w1 w2)) ;ma opresc si construiesc noul arbore
  ((char=? (car w1) (car w2)) (vreau-acc (get-branch-subtree w1) (get-branch-subtree w2) (cons (car w1) acc)))
  (else (list  (reverse acc) w1 w2)) ;din cauza recursivitati acc e invers deci ii fac reverse + resturile din cuvinte
  )
  )
  (vreau-acc w1 w2 '()))
;(longest-common-prefix (string->list "why") (string->list "when"))

; TODO 3
; Implementați recursiv o funcție care primește o listă nevidă 
; de cuvinte care încep cu același caracter și calculează cel 
; mai lung prefix comun al acestora.
; Opriți căutarea (parcurgerea) în momentul în care aveți garanția 
; că prefixul comun curent este prefixul comun final.


(define (longest-common-prefix-of-list words) ;ne folosim de longest common prefix anterior 
  (if (null? (cdr words)) ;verificare importanta!!! altfel primesc contract violation la cst 
      (car words)
  (let*
      ((prefix-curent (car(longest-common-prefix (car words) (cadr words)))) ) ;vom lua 2 cate 2 cuvinte
 (define (caut-prefix words prefix)  ;iau o functie auxiliara 
    (if (null? words)
         prefix
    (caut-prefix (cdr words) (car (longest-common-prefix prefix (car words)))))) ;pereche intre urm cuvant si prefixul rezultat

  (if  (null? words) 
      '()
      (caut-prefix (cdr words) prefix-curent))))) ;
;(define (longest-common-prefix-of-list words) ;ne folosim de longest common prefix anterior 
;(define prefix-curent (car(longest-common-prefix (car words) (cadr words)))) 
; (define (caut-prefix words prefix)
   ; (if (null? words)
       ;  prefix
  ;  (caut-prefix (cdr words) (car (longest-common-prefix prefix (car words)))))) ;pereche intre urm cuvant si prefixul rezultat

 ; (if (null? words)
   ;   '()
   ;   (caut-prefix (cdr words) prefix-curent)))

 ; (car (longest-common-prefix (car (map string->list (list "when" "where" "why" "who"))) (caddr (map string->list (list "when" "where" "why" "who")))))
  ;(longest-common-prefix-of-list (map string->list (list "when" "where" "why" "who")))
   ;(longest-common-prefix-of-list (map string->list (list "when" "where" "why" "who")))
;(longest-common-prefix (string->list "why") (string->list "when"))

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
(define chr (car pattern))
  (define branch (get-ch-branch st chr))
  (if (not branch)
      (list #f '())
      (if (equal? pattern (get-branch-label branch))
          #t
         (begin ; ok aparent nu ma lasa cu define nici asa folosesc let 
            (let ((prefixc (car (longest-common-prefix (get-branch-label branch) pattern)))
               (sablon (cddr (longest-common-prefix (get-branch-label branch) pattern)))
                )
            (if (equal? prefixc pattern)
                #t
                (if (equal? prefixc (get-branch-label branch))
                    (list (get-branch-label branch) (car sablon) (get-branch-subtree branch))
                    (list #f (car sablon)))))))))


; (cdr (string->list "baba"))
; (match-pattern-with-label stree-1 (string->list "baba"))

 ; (get-ch-branch stree-1 (car (string->list "baba")))
 ;(cdr (cdr (get-ch-branch stree-1 (car (string->list "baba")))))
 ;(match-pattern-with-label stree-1 (string->list "na"))
;(match-pattern-with-label stree-1 (string->list "ananb"))
; TODO 5
; Implementați funcția st-has-pattern? care primește un
; arbore de sufixe și un șablon și întoarce true dacă șablonul
; apare în arbore, respectiv false în caz contrar.
(define (st-has-pattern? st pattern)
  (define res (match-pattern-with-label st pattern))  ; se cheama functia anterioara si  in functie de res am 3 cazuri
    (if (equal? res #t) ; e un bool true gata
        #t
      (if (equal? (car res) #f)   #f ; ok aici am false cu lista = pereche sau lista de eticheta pattern nou subarbore 
       (st-has-pattern? (caddr res) (cadr res) )))) ; continui sa verific pattern nou cu subarbore

;(st-has-pattern? stree-1 (string->list "ananb"))