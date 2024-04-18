#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).

(define (longest-common-prefix w1 w2)
  (define (vreau-acc w1 w2 acc)
  (cond 
  ((or (st-empty? w1) (st-empty? w2)) (list (reverse acc) w1 w2)) ;ma opresc si construiesc noul arbore
  ((char=? (car w1) (car w2)) (vreau-acc (get-branch-subtree w1) (get-branch-subtree w2) (cons (car w1) acc)))
  (else (list  (reverse acc) w1 w2))
  )
  )
  (vreau-acc w1 w2 '()))



; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection
(define (longest-common-prefix-of-collection words) ;ne folosim de longest common prefix anterior 
  (if (collection-empty? (collection-cdr words))
      (collection-car words)
  (let*
      ((prefix-curent (car(longest-common-prefix (collection-car words)  (collection-car (collection-cdr words))))) )
 (define (caut-prefix words prefix)
    (if (collection-empty? words)
         prefix
    (caut-prefix (collection-cdr words) (car (longest-common-prefix prefix (collection-car words)))))) ;pereche intre urm cuvant si prefixul rezultat

  (if  (collection-empty? words) 
      '()
      (caut-prefix (collection-cdr words) prefix-curent)))))



(define (match-pattern-with-label st pattern)
  (define chr (car pattern))
  (define branch (get-ch-branch st chr))  ;pereche
  (if (not branch)
      (list #f '())
      (if (equal? pattern (get-branch-label branch))
          #t
         (begin ; ok aparent nu ma lasa cu define nici asa folosesc let 
            (let ((prefixc (car (longest-common-prefix (get-branch-label branch) pattern)))
               (sablon (cddr (longest-common-prefix (get-branch-label branch) pattern))) ;treb cautat in continuare
                )
            (if (equal? prefixc pattern)
                #t
                (if (equal? prefixc (get-branch-label branch))
                    (list (get-branch-label branch) (car sablon) (cdr branch))  ;getbrnch subree
                    (list #f (car(longest-common-prefix (get-branch-label branch) pattern))))))))))


(define (st-has-pattern? st pattern)
;(define (st-has-pattern? st pattern)
  (define res (match-pattern-with-label st pattern))  ; se cheama functia anterioara si  in functie de res am 3 cazuri
    (if (equal? res #t) ; e un bool true gata
        #t
      (if (equal? (car res) #f)   #f ; ok aici am false cu lista = pereche sau lista de eticheta pattern nou subarbore 
       (st-has-pattern? (caddr res) (cadr res) )))) ; continui sa verific pattern nou cu subarbore


(define (get-suffixes text)
  (if (collection-empty? text)
       '() ;daca pun aici collection nu e ok cu checkeru
      (collection-cons text (get-suffixes(collection-cdr text)))
   ))


;(filter (lambda (cuv) (and (not (null? cuv)) (char=? (car cuv) ch))) words)))
(define (get-ch-words words ch)
 ; (displayln ch)
; (display (car(collection-car words)))
  (collection-filter (lambda (cuv) (and (not (null? cuv)) (char=?  (car cuv) ch))) words))


(define (ast-func suffixes)
  (if (collection-empty? suffixes)
      (collection-empty-stream)
 (let* ((label (collection-car (collection-car suffixes))) ; extragem eticheta
        (new-suffixes (collection-map (lambda (suffix) (cdr suffix)) suffixes))) ; eliminăm prima literă din fiecare sufix
    (cons (list label) new-suffixes))))

(define (cst-func suffixes)
  (if (collection-empty? suffixes)
      (collection-empty-stream)
  (let ((prefix (longest-common-prefix-of-collection suffixes)))
    (cons prefix
          (collection-map (lambda (suf) (caddr (longest-common-prefix  prefix suf))) suffixes)))))


; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)

;(define suffixes->st
 ; 'your-code-here)

(define (suffixes->st labeling-func suffixes alphabet)
 ; (display "alphabet ") (displayln (collection-car alphabet))
(define (ramuri-subarbore char)    ;functie pentru map ( map care face ramurile iterand prin alphabet)
  ;(displayln char)
    (let* (
           (lista-de-sufixe (get-ch-words suffixes char))   ;lista sufixelor care incep cu caracterul curent char
           (display lista-de-sufixe)
         ;  preiau labelul pentru ramura
           (label-ramura (cond
                       ((collection-empty? lista-de-sufixe) '())  ;trebuie verificate chestiile astea pentru ca dadea intr-una contract violation
                     ;((and (equal? (length lista-de-sufixe) 1) (eq? (object-name labeling-func) 'cst-func)) (collection-cons (collection-car lista-de-sufixe) collection-empty-stream)) ;altfel nu mergea pentru $ la cst din cauza cum am implementato
                       (else (labeling-func lista-de-sufixe))))  ;am nevoie de ea asa pt sufixele-noi
           (sufixele-noi (cond
                          ( (null? label-ramura) '()) ; extrag noile sufixe pentru subarbori dar mai intai verific daca pot face asta
                        (else (cdr label-ramura))))
           )
      (if (collection-empty? sufixele-noi)   ;contract violation...
         '() ; (collection-empty-stream)   ;se ajunge in situatia in care nu mai am nimic in lista de sufixe pentru ca sigur din alfabet ramane mai ales ca fol map + cont toate literele din text 
       (cons (car label-ramura) (suffixes->st labeling-func sufixele-noi alphabet)))))  ;cons intre eticheta (prefixul comun) si urmatoarele prefixe comune din restul listei
      (collection-filter (lambda (chestie) (not (null? chestie))) (collection-map ramuri-subarbore alphabet))) 



(define  (get-alphabet-from-text text)
 (collection-cons (sort (remove-duplicates text) char<?)
  (collection-empty-stream)))

(define text->st
  (lambda (text)
    (lambda (labeling-func)
      (let* ( (text-cu-dolar (append text '(#\$))) ;textul nu vine cu $
             (suffixes (get-suffixes text-cu-dolar))
            (alphabet (get-alphabet-from-text text-cu-dolar)) ;ok  nu e nevoei de string->list
             )
     (suffixes->st labeling-func suffixes (collection-car alphabet))))
    ))

(define text->ast
  (lambda (text)   ;curry
      ((text->st text) ast-func)))

(define text->cst
 (lambda (text) ;curry
      ((text->st text) cst-func)))


; nu uitați să convertiți alfabetul într-un flux

;(define text->st
;  'your-code-here)

;(define text->ast
 ; (lambda (text)   ;curry
    ;  ((text->st text) ast-func)))
;(define text->ast
 ; 'your-code-here)


;(define text->cst
 ; 'your-code-here)

; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (substring? text pattern)
  (st-has-pattern? (text->ast text) pattern))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (internal-node? branch)
  (not (st-empty? (get-branch-subtree branch)))) ;baaai frt de aici imi picau testele nu e voie cu null!!


(define (repeated-substring-of-given-length text len)
   (define arborel (text->cst text))
  (define sir-curent '())
    (let looper (
                 (subarbore arborel)
                 (sir-curent sir-curent)
                 )

      (cond
       
        ((>= (length sir-curent) len)  (take sir-curent len)) ;l-am gasit
         ((collection-empty? subarbore) #f) ;arbore gol bye bye
        (else
         ; iau nodul curent verific daca e nod intern
         (cond
           ( (internal-node? (first-branch subarbore))  
             ; merg mai departe pe subarbore
             (let ((rezultat (looper (get-branch-subtree (first-branch subarbore)) (append sir-curent (get-branch-label (first-branch subarbore))))))
               ; daca nu am gasit nimic pe subarborele curent, continuam pe celelate ramuri
                 (if (equal? #f rezultat)
                 (looper (other-branches subarbore) sir-curent)
               rezultat))
           )
           (else  ; nu e nod intern deci e frunza, trec peste
            (looper (other-branches subarbore) sir-curent)
            )
         )
       )
     )
    ))
