#lang racket

; Helpers
(define (square x) (expt x 2)) 
(define (index-of lst ele)
  (let loop ((lst lst)
             (idx 0))
    (cond ((empty? lst) #f)
          ((equal? (first lst) ele) idx)
          (else (loop (rest lst) (add1 idx))))))

;;; 1.1 Umrechnung ;;;

; Variante 1: Vereinfachte Funktionsdefinition
(define (degrees->radians x)
  (* (/ x 180) pi))

; Variante 2: Lambda
(define radians->degrees
  (lambda (x) (/ (* x 180) pi)))


;;; 1.2 Umkehrfunktion acos ;;;

(define (my-acos a)
  (if (zero? a) (/ pi 2) ; catch division by zero, return pi/2
      [atan (/ (sqrt (- 1 (square a))) a)]))

;;; 1.3 Kilometer und Seemeilen ;;;

(define (nm->km nm)
  (* nm 1.852))


;;; 2.1 Großkreisentfernung ;;;

(define (cos-dG Ab Al Bb Bl)
  (+ [* (sin Ab) (sin Bb)] [* (cos Ab) (cos Bb) (cos (- Bl Al))]))

(define (radians->minutes radians)
  (* 60 (radians->degrees radians)))

(define (distanzAB Ab Al Bb Bl)
  (nm->km (radians->minutes (acos (cos-dG 
                                   (degrees->radians Ab)
                                   (degrees->radians Al)
                                   (degrees->radians Bb)
                                   (degrees->radians Bl))))))

(display "Oslo -> Hong Kong ")
(distanzAB 59.93 10.75 22.20 114.10)

(display "San Francisco -> Honolulu ")
(distanzAB 37.75 -122.45 21.32 -157.83)

(display "Osterinsel -> Lima ")
(distanzAB -27.10 -109.40 -12.10 -77.05)

;;; 2.2 Anfangskurs ;;;

(define (ar Ab Al Bb Bl)
  (let* ([dG (acos (cos-dG Ab Al Bb Bl))])
    (acos (/ (- (sin Bb) (* (cos dG) (sin Ab))) (* (cos Ab) (sin dG))))))

(define (Anfangskurs Ab Al Bb Bl eastwards)
  (let* ([a (radians->degrees (ar 
                               (degrees->radians Ab)
                               (degrees->radians Al)
                               (degrees->radians Bb)
                               (degrees->radians Bl)))])
    (if eastwards a (360 - a))))

;;; 2.3 Himmelsrichtungen ;;;

(define Himmelsrichtungen 
  (vector 'N 'NNE 'NE 'ENE 'E 'ESE 'SE 'SSE 'S 'SSW 'SW 'WSW 'W 'WNW 'NW 'NNW))


#| Sucht aus dem Vektor die gewünschte Himmelsrichtung heraus. Rundet
auf 22.5 Grad (360/16), sucht im Vektor an entsprechender Stelle (modulo 16). |#
(define (Grad->Himmelsrichtung grad)
  (vector-ref Himmelsrichtungen (inexact->exact (modulo (round (* (/ grad 360) 16.0)) 16)))
  ) 

#| Findet im Vector die Richtung, multipliziert den Index mit 22.5, oder 
Fehlermeldung wenn nicht gefunden.|#
(define (Himmelsrichtung->Grad richtung)
  (let* ([a (vector-member richtung Himmelsrichtungen)])
    (if a (* a 22.5) "Bitte gültige Himmelsrichtung angeben.")))