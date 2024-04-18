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
  (let* ( (arbore (text->cst text))
          (check (st-has-pattern? arbore pattern))
         )
    check
    ))

;(substring?  (string->list "banana")  (string->list "b"))
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

(define (traverse-subcopac st pattern-nou current-length current-longest-match)
  ;trebuie sa ne folosim de st-has-pattern? sa vad daca are sens sa updatez lungimea
  ;nu chiar ca daca intoarce fals si de fapt e fals si lista cu char ?
  ;cefac cu dolaru? nu-l pune ca e longest common prefix 
  ;returnez o percehe intre noua lungime si noul substring gasit
  (if (null? st)
      (cons current-length current-longest-match) ;am terminat returnez perechea cu ce am gasit
      (let* (
          (match-info (match-pattern-with-label st pattern-nou))
           )
        (cond 
          ((equal? match-info #t) (cons    ;intre lungimea curenta si noul branch complet gasit si branchul gasit cu append!
                                  ; current-length
                                  ; current-longest-match
                                   (+ current-length (length pattern-nou))   ;da frate ca daca am pattern-nou a si eticheta ac$ il ia ca true si fute tot
                                   (append current-longest-match pattern-nou)
                                  ; (+ current-length (length (remove-last (get-branch-label (get-ch-branch st (car pattern-nou))))))
                                  ; (append current-longest-match (remove-last (get-branch-label (get-ch-branch st (car pattern-nou)))))
                                    ) ) ;mai are sens a verific daca arborele e nul?
          ((equal? (car match-info) #f)
           ; nu l-am mai gasit nu merg mai departe in subarbore
           (if (null? (cdr match-info))
               (traverse-subcopac '() pattern-nou current-length current-longest-match) ;n-am gasit deloc ma opresc fac arborele nul?
               ;altfel adaug la lungime si la match caracterele din cdr
               (traverse-subcopac '()
                                  (caddr (longest-common-prefix (cadr match-info) pattern-nou ) )            ;aici nu e pattern nou e cddr longestcommon prefix intre cadr match-info si pattern-nou??
                                  (+ current-length (length (cadr match-info)))          ;adaug lungimea din match la lungime
                                  (append current-longest-match (cadr match-info))       ;appendez la machul current noua lista gasita 
                                  )) ) 
          (else    ;atunci am mai gasit un subarbore cu potrivire partiala deci il dau mai departe ca argument
           (traverse-subcopac (caddr match-info)   ; noul subarbore
                              (cadr match-info)   ;noul pattern 
                              (+ current-length (length (car match-info))) ; adaug noua lungime de la eticheta comuna
                              (append current-longest-match (car match-info)) ; appendez si chestoa comuna
                              )))
       ))
  )

;aici incepe functia 
(define (longest-common-substring text1 text2)
  (define arbore1 (text->cst text1))
  (define sufixe-text2 (get-suffixes text2))
  (define max 0)
  ;cu loop merg prin lista de sufixe
  (let loop (
             (suffix-list sufixe-text2)
             (lungimemax max) ;initial e 0 si se actualizeaza pe parcurs
             (match-lung '())
             (subarbore arbore1)
             )
    (cond
      ((null? suffix-list)
        match-lung)
       ; care poate sa fie si '() aici vedem cum returnam cu length mai incolo
       ;aplicam match-label-st si verificam ce intoarce apoi ne ocupam cumva si de len
      (else 
       (let* ( (match-info  (match-pattern-with-label subarbore (car suffix-list)))
               ;(current-len 0)
              )
         ;cond asta e evaueaza o singura data pentru fiecare sufix 
         (cond
           ((equal? match-info #t)   ; ok verificam lungimea
                                    (if (< lungimemax (length (car suffix-list)))
                                        (loop (cdr suffix-list) (length (car suffix-list)) (car suffix-list) subarbore)
                                        ; subarbore NU SE SCHIMBA
                                        (loop (cdr suffix-list) lungimemax match-lung subarbore)
                                        ;lungimea aluia e mai mica sau egala decat ce am pana acuma 
                                         ))  ;;merg mai departe sa vada daca mai e vreunu mai mare daca nu iterez si ies din lista si returnez pe asta
           ((and (equal? (car match-info) #f) (null? (cdr match-info)) ) (loop (cdr suffix-list) lungimemax match-lung subarbore)) ;merg mai departe pastrez ce am adunat pana acum
           ((and (equal? (car match-info) #f) (not (null? (cdr match-info))) )  (if (< lungimemax (length (cadr match-info)))  ;era car pattern adica lung 1 deci asta e tot matchul 
                                                                                    (loop (cdr suffix-list) (length (cadr match-info)) (cadr match-info) subarbore) ;setez pe 1, nu fac append aici pentru ca sigur ma opesc pe prima ramura gasita
                                                                                    (loop (cdr suffix-list) lungimemax match-lung subarbore) ;ignor si merg mai departe cu match-lung gasit anterior
                                                                                    )
            )
           (else   ;;se retuneaza lista (etichetă, nou pattern, subarbore) a gasit potrivire partiala trebuie sa mai caut si in subarbore -> fac functie
            ;(traverse-subcopac st pattern-nou current-length current-longest-match)
            ;lungimea etichetei se pune la o variabila lungime
            (let*
              (
               (prefix-comun (car match-info)) ;lungime potrivire initiala
               (restul-sablon (cadr match-info)) ;restul sablonului de cautat
               (arborel (caddr match-info)) ;daca nu fac cu caddr nu ia bine lista si am () in plus in afara
               (result-parcurgere (traverse-subcopac arborel restul-sablon (length prefix-comun) prefix-comun)) ;e pair lung + ce am mai gasit
              )
          ;ce fac mai departe?
          ;scot datele
          ;functia returneaza o pereche intre lungime si stringul gasit
              (let* (
                      (rez-numeric (car result-parcurgere))
                      (string (cdr result-parcurgere))
                      )
                ;check lungimemax
                (if (< lungimemax rez-numeric)
                    ;lungiemax se actualizeaza la fel si match-lung caruia nu treb sa ii fac append ii fac deja in fucntia aia
                    (loop (cdr suffix-list) rez-numeric string subarbore)
                    (loop (cdr suffix-list) lungimemax match-lung subarbore)
                    )
                )
            )
           )
        
         )
       
     ;(loop (cdr suffix-list) lungimemax '() subarbore) ;asta trebuie aici? nu
       
      )
    
  ))))

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

(define (internal-node? branch)
  (not (null? (get-branch-subtree branch))))
;o sa avem nevoie de functia asta ca sa verific daca ajung pe copil((first-len-chars

;aici incepe functia
(define (repeated-substring-of-given-length text len)
  (define arborel (text->cst text))
  (define sir-curent '())
    (let looper (
                 (subarbore arborel)
                 (sir-curent sir-curent)
                 )

      (cond
       
        ((>= (length sir-curent) len)  (take sir-curent len)) ;l-am gasit
         ((st-empty? subarbore) #f) ;arbore gol bye bye
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