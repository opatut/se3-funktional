#lang racket

; Carolin Konietzny
; Johanna Nehring-Ansohn
; Paul Bienkowski

; Brauchen wir später
(require se3-bib/flaggen-module)

(define raw-chars "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789,. ")
(define raw-words (list "Alfa" "Bravo" "Charlie" "Delta" "Echo" "Foxtrott" "Golf" "Hotel" "India" "Juliett" "Kilo" "Lima" "Mike" "November" "Oscar" "Papa" "Quebec" "Romeo" "Sierra" "Tango" "Uniform" "Viktor" "Whiskey" "X-ray" "Yankee" "Zulu" "Nadazero" "Unaone" "Bissotwo" "Terrathree" "Kartefour" "Pantafive" "Soxisix" "Setteseven" "Oktoeight" "Novenine" "Decimal" "Stop" "Space"))
(define raw-flags (list A B C D E F G H I J K L M N O P Q R S T U V W X Y Z Z0 Z1 Z2 Z3 Z4 Z5 Z6 Z7 Z8 Z9 '_ '_ '_)) 
; Die '_ sind Platzhalter für Flaggen, die es nicht gibt.

(define (merge left right)
  (if (equal? left '())
      '()
      (append (list (cons (car left) (car right)))
              (merge (cdr left) (cdr right)))))

; 1.1
(define words (merge (string->list raw-chars) raw-words))
(define flags (merge (string->list raw-chars) raw-flags))

; Erklärung
; Die Datenstruktur ist eine Liste, welche als Elemente Paare enthält. 
; In diesen Paaren steht an erster Stelle das Zeichen (char), an zweiter 
; Stelle der dazugehörige String.

; Begründung
; - einfach zu generieren ;)
; - diese Struktur kann man von beiden Seiten ansehen
; - wir wollten diese Struktur wie in der Vorlesung vorgestellt ausprobieren
; - wir nutzen Paare, damit wir gleich mit car/cdr direkt auf die Elemente 
;   Zugreifen, und nicht (speziell bei cdr) eine Liste mit einem Element erhalten

; 1.2
(define (char->name c xs) ; xs: Unsere Datenstruktur (words/flags)
  (cdr (assoc c xs)))

; 1.3 
(define (upper c)
  (let ((i (char->integer c))) 
    (if (and (>= i 97) (<= i 122))
        (integer->char (- i 32))
        c)))

; 1.4
(define (convert chars xs) ; xs: Unsere Datenstruktur (words/flags)
  (if (equal? '() chars)
      '()
      (append (list (char->name (upper (car chars)) xs))
              (convert (cdr chars) xs))))
         
(define (convert-text text)
  (string-join (convert (string->list text) words) " "))

;;; Aufgabe 2

; Hier haben wir etwas nachgedacht und bereits
; bestehenden Code wiederverwertet, um nicht das gleiche in
; grün zu programmieren.

; 2.1
; Erklärung und Begründung siehe 1.1

; 2.2
; Wir haben `char->name` und `convert` so umgebaut, dass man das die 
; Übersetzungs-Datenstruktur übergeben kann.

; 2.3
; ... nutzt die rekursive Funktion `convert` aus 1.4
(define (convert-flags text)
  (convert (string->list text) flags))

; TESTEN!!!

(convert-text "HelloWorld")
(convert-flags "HelloWorld")

