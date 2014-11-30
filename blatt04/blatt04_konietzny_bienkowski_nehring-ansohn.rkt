#lang racket

; Hier nochmal von letzter Woche, um die Rufzeichen in Aufgabe 2 zu konvertieren
(define raw-chars "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789,. ")
(define raw-words (list "Alfa" "Bravo" "Charlie" "Delta" "Echo" "Foxtrott" "Golf" "Hotel" "India" "Juliett" "Kilo" "Lima" "Mike" "November" "Oscar" "Papa" "Quebec" "Romeo" "Sierra" "Tango" "Uniform" "Viktor" "Whiskey" "X-ray" "Yankee" "Zulu" "Nadazero" "Unaone" "Bissotwo" "Terrathree" "Kartefour" "Pantafive" "Soxisix" "Setteseven" "Oktoeight" "Novenine" "Decimal" "Stop" "Space"))
(define (merge left right)
  (if (equal? left '())
      '() (append (list (cons (car left) (car right)))
                  (merge (cdr left) (cdr right)))))
(define words (merge (string->list raw-chars) raw-words))
(define (char->name c xs) (cdr (assoc c xs)))
(define (convert chars xs)
  (if (equal? '() chars)
      '() (append (list (char->name (char-upcase (car chars)) xs))
                  (convert (cdr chars) xs))))
(define (convert-text text)
  (string-join (convert (string->list text) words) " "))

; So, jetzt aber die neuen Sachen

; Aufgabe 1

(max (min 2 (- 2 5)) 0)
; 0, da (min 2 -3) == -3  und (max -3 0) == 0

'(+ (- 2 13) 11)
;'(+ (- 2 13) 11)      da dies nicht evaluiert wird

(cadr '(Alle Jahre wieder))
; (car (cdr '(Alle Jahre wieder)) == 'Jahre

(cddr '(kommt (das Weihnachtfest)))
;(cdr '((das Weihnachtsfest))
;'()

(cdadr '(kommt (das . Weihnachtfest)))
;'Weihnachtsfest
; (cdr (car (cdr '(kommt (das . Weihnachtfest)))))
; (cdr (car '((das . Weihnachtfest))))
; (cdr '(das . Weihnachtfest))
; 'Weihnachtsfest


(cons 'Listen '(ganz einfach und))
;'(Listen ganz einfach und)
; ist ja nichts anderes als
;'(Listen . (ganz . (...))

(cons 'Paare 'auch)
;'(Paare . auch)

(equal? (list 'Racket 'Prolog 'Java) '(Racket Prolog Java))
;#t
; da equal? auf Gleichheit prüft

(eq? (list 'Racket 'Prolog 'Java) (cons 'Racket '(Prolog Java)))
;#f
; da eq? auf Identität prüft

; Aufgabe 2.1

#|
<Notruf> ::= <Überschrift> <Standort> <ArtDesUnfallsUndHilfeleistung>
             <Peilzeichen> <Unterschrift> <Over>
<Überschrift> ::= <Notzeichen> <Notzeichen> <Notzeichen> <HierIst> 
                  <Schiffsname> <Schiffsname> <Schiffsname> <Rufzeichen>
                  <Notzeichen <Schiffsname> <Buchstaben> <Rufzeichen>
<Notzeichen> ::= "MAYDAY "
<HierIst> ::= "HIER IST " | "DELTA ECHO "
<Schiffsname> ::= <text>
<Rufzeichen> ::= <Buchstabe> <Buchstabe> <Buchstabe> <Buchstabe>
<Buchstabe> ::= "ALPHA " | "BRAVO " | "CHARLIE " | ... | "NOVENINE "
<Buchstaben> ::= <Buchstabe> | <Buchstabe> <Buchstaben>
<Standort> ::= "NOTFALLPOSITION " <text>
<ArtDesUnfallsUndHilfeleistung> ::= <text>
<Peilzeichen> :== "ICH SENDE DEN TRAEGER -- "
<Unterschrift> ::= <Schiffsname> <Rufzeichen>
|#

; Aufgabe 2.2

(define Notzeichen "MAYDAY ")

(define (Überschrift Schiffsname Rufzeichen)
  (string-append Notzeichen Notzeichen Notzeichen 
                 "DELTA ECHO " 
                 Schiffsname " " Schiffsname " " Schiffsname " "
                 (convert-text Rufzeichen) " "
                 Notzeichen Schiffsname " " 
                 "ICH BUCHSTABIERE "
                 (convert-text Schiffsname) " "
                 (convert-text Rufzeichen) " "))

(define (Unterschrift Schiffsname Rufzeichen)
  (string-append Schiffsname " "
                 (convert-text Rufzeichen) " "))





(define (Notruf Schiffsname Rufzeichen Position Sonstiges)
  (string-upcase (string-append (Überschrift Schiffsname Rufzeichen) 
                                "NOTFALLPOSITION " Position " "
                                Sonstiges " " "ICH SENDE DEN TRÄGER -- "
                                (Unterschrift Schiffsname Rufzeichen)
                                "OVER\n\n")))





; Aufgabe 2.3
(display (Notruf "Babette" 
                 "DEJY" 
                 "ungefaehr 10 sm nordoestlich Leuchtturm Kiel"
                 "NOTFALLZEIT 1000 UTC SCHWERER WASSEREINBRUCH WIR SINKEN KEINE VERLETZTEN VIER MANN GEHEN IN DIE RETTUNGSINSEL SCHNELLE HILFE ERFORDERLICH"))
(display (Notruf "Amira"
                 "AMRY"
                 (string-append (convert-text "53") " GRAD " (convert-text "56") " MINUTEN NORD "
                                (convert-text "006") " GRAD " (convert-text "31") " MINUTEN OST")
                 "sinken nach Kenterung in schwerer See 15 Mann an Bord das Schiff ist 15 Meter lang mit rotem Rumpf Notfallzeit 1640 UTC"))




; Aufgabe 3.1

#|
(define (hoch3 x) (* x x x))

; Innere Reduktion
(hoch3 (* 3 (+ 1 (hoch3 2))))   ;(hoch3)
(hoch3 (* 3 (+ 1 (* 2 2 2))))   ;(*)
(hoch3 (* 3 (+ 1 8)))           ;(+)
(hoch3 (* 3 9))                 ;(*)
(hoch3 27)                      ;(hoch3)
(* 27 27 27)                    ;(*)
19683

; Äußere Reduktion
(hoch3 (* 3 (+ 1 (hoch3 2))))                                           ;(hoch3)
(* (* 3 (+ 1 (hoch3 2))) (* 3 (+ 1 (hoch3 2))) (* 3 (+ 1 (hoch3 2))))   ;(hoch3)
(* (* 3 (+ 1 (* 2 2 2))) (* 3 (+ 1 (hoch3 2))) (* 3 (+ 1 (hoch3 2))))   ;(*)
(* (* 3 (+ 1 8)) (* 3 (+ 1 (hoch3 2))) (* 3 (+ 1 (hoch3 2))))           ;(+)
(* (* 3 9) (* 3 (+ 1 (hoch3 2))) (* 3 (+ 1 (hoch3 2))))                 ;(*)
(* 27 (* 3 (+ 1 (hoch3 2))) (* 3 (+ 1 (hoch3 2))))                      ;(hoch3)
(* 27 (* 3 (+ 1 (* 2 2 2))) (* 3 (+ 1 (hoch3 2))))                      ;(*)
(* 27 (* 3 (+ 1 8)) (* 3 (+ 1 (hoch3 2))))                              ;(+)
(* 27 (* 3 9) (* 3 (+ 1 (hoch3 2))))                                    ;(*)
(* 27 27 (* 3 (+ 1 (hoch3 2))))                                         ;(hoch3)
(* 27 27 (* 3 (+ 1 (* 2 2 2))))                                         ;(*)
(* 27 27 (* 3 (+ 1 8)))                                                 ;(+)
(* 27 27 (* 3 9))                                                       ;(*)
(* 27 27 27)                                                            ;(*)
19683
|#

; Aufgabe 3.2

; In Racket wird in der Regel die innere Reduktion verwendet, nur
; Spezialformen werden mit äußerer Reduktion behandelt.


; Aufgabe 3.3

; Dieses Programm endet in einer endlos tiefen Rekursion, da der Rekursionsschritt
; immer als innerstes steht und somit zuerst ausgewertet (reduziert) wird.
; Die Spezialform für `if` ist notwendig, um eben nur den einen Zweig auszu-
; werten. Dies ist besonders wichtig in rekursiven Funktionen, wie man am
; Beispiel der Fakultät sieht, um durch die Abbruchbedingung auch abzubrechen.


