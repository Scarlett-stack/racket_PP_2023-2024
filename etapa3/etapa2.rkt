#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")

(provide (all-defined-out))

;; În această etapă definim algoritmul de construcție a unui
;; arbore de sufixe (atât cel compact, cât și cel atomic) pe
;; baza unui text și a unui alfabet dat. Se știe că textul
;; utilizează doar simboluri prezente în alfabet.
;; Abordarea este următoarea:
;; 1. obținem toate sufixele textului
;; 2. pentru fiecare caracter din alfabet, determinăm sufixele
;;    care încep cu acel caracter: ele vor fi grupate în câte
;;    o ramură a arborelui de sufixe (exceptând caracterele
;;    care nu apar in text - acestea nu generează ramuri)
;; 3. pentru fiecare listă de sufixe care încep cu un același
;;    caracter, determinăm eticheta ramurii, respectiv noile
;;    sufixe care vor genera subarborele de sub etichetă
;;    - în cazul unui AST (atomic suffix tree), eticheta este
;;      chiar primul caracter, iar noile sufixe sunt vechile
;;      sufixe fără primul caracter
;;    - în cazul unui CST (compact suffix tree), eticheta este
;;      cel mai lung prefix comun al sufixelor, iar sufixele
;;      noi se obțin din cele vechi prin îndepărtarea acestui
;;      prefix
;; 4. transformăm fiecare rezultat de la pasul 3 într-o ramură
;;    - eticheta este deja calculată
;;    - calculul subarborilor se realizează repetând pașii 2-4
;;      pentru noile sufixe


; TODO 1
; Implementați recursiv o funcție care primește un text (listă 
; de caractere) și determină lista tuturor sufixelor acestuia
; (de la cel mai lung la cel mai scurt).
; Se știe că textul se va termina cu caracterul special "$", 
; și cu acest caracter trebuie să se termine și toate sufixele
; din lista rezultat (de la întreg textul urmat de "$" până la
; sufixul vid - reprezentat de un singur "$").
; ex:
; (get-suffixes '(#\w #\h #\y #\$))
; => '((#\w #\h #\y #\$) (#\h #\y #\$) (#\y #\$) (#\$))
; Folosiți recursivitate pe stivă.
(define (get-suffixes text)
 (if (null? text)
      '()
      (cons text (get-suffixes (cdr text)))))
     



; TODO 2
; Implementați o funcție care primește o listă de cuvinte 
; și un caracter ch și întoarce acele cuvinte din listă care 
; încep cu caracterul ch.
; Atenție, este posibil ca unele cuvinte să fie vide.
; Folosiți funcționale (și nu folosiți recursivitate explicită).
(define (get-ch-words words ch)
 ; (display ch)
 ; (display words)
  (if (null? words)
      '()
  (filter (lambda (cuv) (and (not (null? cuv)) (char=? (car cuv) ch))) words))) ;nu e nul si are prima litera identica cu ch


; TODO 3
; Implementați o funcție care primește o listă nevidă de sufixe 
; care încep cu același caracter și calculează perechea
; (etichetă AST pentru aceste sufixe, lista noilor sufixe).
; Reamintim că pentru un AST eticheta este chiar primul caracter
; (dar sub formă de listă de caractere, pentru a putea fi 
; procesată la fel ca etichetele mai complexe), iar noile sufixe 
; se obțin din cele vechi prin eliminarea acestui caracter.
; Nu folosiți recursivitate explicită.
(define (ast-func suffixes)
  (if (null? suffixes)
      '()
 (let* ((label (car (car suffixes))) ; extragem eticheta
        (new-suffixes (map (lambda (suffix) (cdr suffix)) suffixes))) ; eliminăm prima literă din fiecare sufix
    (cons (list label) new-suffixes))))


; TODO 4
; Implementați o funcție care primește o listă nevidă de sufixe 
; care încep cu același caracter și calculează perechea
; (etichetă CST pentru aceste sufixe, lista noilor sufixe).
; Reamintim că pentru un CST eticheta este cel mai lung prefix
; comun, iar noile sufixe se obțin din cele vechi prin eliminarea 
; acestui prefix.
; Nu folosiți recursivitate explicită.
;(define (cst-func suffixes)
;(define longest-com-prefix-w (longest-common-prefix-of-list suffixes) )
  ;(display longest-com-prefix-w)
; (define chestie (cons longest-com-prefix-w    ; lcp returna chestia asta  '((#\w #\h) (#\y) (#\e #\n)) 
   ;     (map (lambda (suffix) (caddr (longest-common-prefix longest-com-prefix-w suffix))) suffixes))) ;extragem restul sufixelor
;  (apply list (car chestie) (cdr chestie)))


(define (cst-func suffixes)
  (if (null? suffixes)
      '()
  (let ((prefix (longest-common-prefix-of-list suffixes)))
    (cons prefix
          (map (lambda (suf) (caddr (longest-common-prefix  prefix suf))) suffixes)))))
; TODO 5
; Implementați funcția suffixes->st care construiește un
; arbore de sufixe pe baza unei liste de sufixe, a unui 
; alfabet (listă de caractere care include toate caracterele
; din sufixe), și a unei funcții care indică modul de
; etichetare (atomic sau compact).
; Când argumentul funcție va fi ast-func, vom obține un AST.
; Când argumentul funcție va fi cst-func, vom obține un CST.
; Obs: Funcția surprinde pașii 2-4 descriși mai sus.
; Funcția suffixes->st poate fi recursivă explicit, dar
; pentru celelalte prelucrări (pașii 2 și 3) trebuie să
; folosiți funcționale.
;(define (suffixes->st labeling-func suffixes alphabet)
 ; 'your)
; 2. pentru fiecare caracter din alfabet, determinăm sufixele
;;    care încep cu acel caracter: ele vor fi grupate în câte
;;    o ramură a arborelui de sufixe (exceptând caracterele
;;    care nu apar in text - acestea nu generează ramuri)
;; 3. pentru fiecare listă de sufixe care încep cu un același
;;    caracter, determinăm eticheta ramurii, respectiv noile
;;    sufixe care vor genera subarborele de sub etichetă
;;    - în cazul unui AST (atomic suffix tree), eticheta este
;;      chiar primul caracter, iar noile sufixe sunt vechile
;;      sufixe fără primul caracter
;;    - în cazul unui CST (compact suffix tree), eticheta este
;;      cel mai lung prefix comun al sufixelor, iar sufixele
;;      noi se obțin din cele vechi prin îndepărtarea acestui
;;      prefix
;; 4. transformăm fiecare rezultat de la pasul 3 într-o ramură
;;    - eticheta este deja calculată
;;    - calculul subarborilor se realizează repetând pașii 2-4
;;      pentru noile sufixe


(define (suffixes->st labeling-func suffixes alphabet)
  (define (ramuri-subarbore char)    ;functie pentru map ( map care face ramurile iterand prin alphabet) 
    (let* (
           (lista-de-sufixe (get-ch-words suffixes char))   ;lista sufixelor care incep cu caracterul curent char
           ;(display lista-de-sufixe)
           ;preiau labelul pentru ramura
           (label-ramura (cond
                       ((null? lista-de-sufixe) '())  ;trebuie verificate chestiile astea pentru ca dadea intr-una contract violation
                    ; ((and (equal? (length lista-de-sufixe) 1) (eq? (object-name labeling-func) 'cst-func)) (list (car lista-de-sufixe))) ;altfel nu mergea pentru $ la cst din cauza cum am implementato
                       (else (labeling-func lista-de-sufixe))))  ;am nevoie de ea asa pt sufixele-noi
           (sufixele-noi (cond
                          ( (null? label-ramura) '()) ; extrag noile sufixe pentru subarbori dar mai intai verific daca pot face asta
                         (else (cdr label-ramura))))
           )
      (if (and (null? label-ramura) (null? sufixele-noi))   ;contract violation...
         '()   ;se ajunge in situatia in care nu mai am nimic in lista de sufixe pentru ca sigur din alfabet ramane mai ales ca fol map + cont toate literele din text 
       (cons (car label-ramura) (suffixes->st labeling-func sufixele-noi alphabet)))))  ;cons intre eticheta (prefixul comun) si urmatoarele prefixe comune din restul listei
      (filter (lambda (chestie) (not (null? chestie))) (map ramuri-subarbore alphabet)))  ;se pune arbore vid daca nu am gasit lista de prefixe pentru litera aia de alfabet pe care iterez asa ca trb filtrati

; TODO 6
; Această sarcină constă în implementarea a trei funcții:
; text->st, text->ast, text->cst, unde text->ast și text->cst
; trebuie obținute ca aplicații parțiale ale lui text->st.
; În acest scop, funcția text->st trebuie să fie curry.

; a) Implementați funcția text->st care primește un text
; (listă de caractere) și o funcție de etichetare și întoarce
; arborele de sufixe corespunzând acestui text cu această
; metodă de etichetare.
; Pași:
; - obținem sufixele textului la care adăugăm marcajul final $
; - obținem alfabetul sortat asociat textului prin utilizarea
;   funcțiilor de bibliotecă sort, remove-duplicates, char<?
;   (inclusiv caracterul $)
; - apelăm corespunzător funcția suffixes->st
; Vă veți defini singuri parametrii funcției text->st și ordinea 
; acestora, dar funcția trebuie să fie curry într-un mod care 
; facilitează derivarea funcțiilor text->ast și text->cst de 
; mai jos prin aplicație parțială a funcției text->st.
; Obs: Din acest motiv, checker-ul testează doar funcțiile
; text->ast și text->cst.
(define  (get-alphabet-from-text text)
  (sort (remove-duplicates text) char<?)
  )

(define text->st
  (lambda (text)
    (lambda (labeling-func)
      (let* ( (text-cu-dolar (append text '(#\$))) ;textul nu vine cu $
             (suffixes (get-suffixes text-cu-dolar))
             (alphabet (get-alphabet-from-text text-cu-dolar)) ;ok  nu e nevoei de string->list
             )
      (suffixes->st labeling-func suffixes alphabet)))
    ))

; b) Din funcția text->st derivați funcția text->ast care
; primește un text (listă de caractere) și întoarce AST-ul
; asociat textului.
(define text->ast
  (lambda (text)   ;curry
      ((text->st text) ast-func)))


; c) Din funcția text->st derivați funcția text->cst care
; primește un text (listă de caractere) și întoarce CST-ul
; asociat textului.
(define text->cst
 (lambda (text) ;curry
      ((text->st text) cst-func)))

(define st  (text->cst (string->list "banana")))