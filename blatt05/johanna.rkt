#lang racket

#| Hausaufgabe Nr. 5

Aufg.1:
Entwickeln Sie einen Vorschlag für eine Gliederung des Programms
in Funktionen und spezifizieren und dokumentieren Sie die Schnitt-
stellen.
 
Die Gliederung sieht folgendermaßen aus: 
In der Aufgabenstellung heißt es, dass jedes Elternteil zwei Merkmale hat, von denen das Dominante auf jeden Fall sichtbar ist.
Ich werde also zuerst eine Funktion schreiben in der es darum geht, von jeweils zwei Elternmerkmalen eins pro Elternteil zufällig zu ermitteln.
Dies mache ich mit einem Programm wie dem one-of aus der Vorlesung.
Dann werden die erzielten Werte in einem weiteren Programm verglichen: "ist eins der input-Elemente 'Streifen ? Wenn ja, wird es auf jeden 
Fall dominant vererbt."
Dann übernehme ich all diese Vergleiche und füge sie in ein Programm ein, und speichere sie als Merkmal des Kindes|#
(require se3-bib/butterfly-module-2013)
(require 2htdp/image)
;(show-butterfly 'red 'stripes 'curved 'rhomb)
;(show-butterfly 'yellow 'star 'straight 'hexagon)
;(show-butterfly 'blue 'dots 'curly 'ellipse )

;; Funktion, mit der von mehreren Eingabeelementen zufällig eins ausgewählt wird:
(define (one-of set)
  (list (random−elt set) ) )

(define (random−elt choices)
  (list-ref choices
            (random (length choices) ) ) )
;;Funktion, die Entscheidet, welches Merkmal gezeigt wird (da es dominant ist):
(define (farbe x1 x2)
  (cond ((or (equal? x1 'blue) (equal? x2 'blue)) 'blue)
        ((or (equal? x1 'yellow) (equal? x2 'yellow)) 'yellow)
        ((or (equal? x1 'red) (equal? x2 'red)) 'red)
        (else "Bitte gültige Farbe eingeben")))

(define (fuehler x1 x2)
  (cond ((or (equal? x1 'curved) (equal? x2 'curved)) 'curved)
        ((or (equal? x1 'straight) (equal? x2 'straight)) 'straight)
        ((or (equal? x1 'curly) (equal? x2 'curly)) 'curly)
        (else "Bitte gültige Fühlerform eingeben")))

(define (fluegel x1 x2)
  (cond ((or (equal? x1 'hexagon) (equal? x2 'hexagon)) 'hexagon)
        ((or (equal? x1 'rhomb) (equal? x2 'rhomb)) 'rhomb)
        ((or (equal? x1 'ellipse) (equal? x2 'ellipse)) 'ellipse)
        (else "Bitte gültige Fluegelform eingeben")))

(define (muster x1 x2)
    (cond ((or (equal? x1 'stripes) (equal? x2 'stripes)) 'stripes)
        ((or (equal? x1 'dots) (equal? x2 'dots)) 'dots)
        ((or (equal? x1 'star) (equal? x2 'star)) 'star)
        (else "Bitte gültiges Muster eingeben")))

;;mit dieser Funktion erschaffen wir zu jedem Merkmal des Schmetterlings
;;ein rezessives, das weniger stark oder gleich stark ist wie das dominante,
;;auf keinen Fall jedoch stärker.
(define (recessive x)
 (append 
  (if (equal? (car x) 'blue) 
      (append (list (car x)) (one-of '(blue red yellow)))
      (if (equal? (car x) 'yellow)
          (append (list (car x)) (one-of '(yellow red)))
          (append (list (car x)) '(red))))
  (if (equal? (cadr x) 'stripes)
      (append (list (cadr x)) (one-of '(stripes dots star)))
      (if (equal? (cadr x) 'dots)
          (append (list (cadr x)) (one-of '(dots star)))
          (append (list (cadr x)) '(star))))
  (if (equal? (caddr x) 'curved)
      (append (list (caddr x)) (one-of '(curved straight curly)))
      (if (equal? (caddr x) 'straight)
          (append (list (caddr x)) (one-of '(straight curly)))
          (append (list (caddr x)) '(curly))))
  (if (equal? (cadddr x) 'hexagon)
      (append (list (cadddr x)) (one-of '(hexagon rhomb ellipse)))
      (if (equal? (list (cadddr x)) 'rhomb)
          (append (list (cadddr x)) (one-of '(rhomb ellipse)))
          (append (list (cadddr x)) '(ellipse))))))

#|mit dieser Funktion geben wir pro Elternteil ein zufällig ausgewähltes Gen weiter
also entweder das dominante oder das rezessive, und dann wird geschaut, welches
der beiden übrig gebliebenen das dominante ist. Dessen Merkmal wird dann dominantes
Merkmal unseres Kinds-Schmetterlings.|#
(define (mixparents p1 p2)
  (let ([xs (recessive p1)]
        [ys (recessive p2)])
  (append
    (list (farbe (car (one-of (list (list-ref xs 0) (list-ref xs 1))))
           (car (one-of (list (list-ref ys 0) (list-ref ys 1))))))
    (list (muster (car (one-of (list (list-ref xs 2) (list-ref ys 3))))
          (car (one-of (list (list-ref ys 2) (list-ref ys 3))))))
    (list (fuehler (car (one-of (list (list-ref xs 4) (list-ref xs 5))))
           (car (one-of (list (list-ref ys 4) (list-ref ys 5))))))
    (list (fluegel (car (one-of (list (list-ref xs 6) (list-ref xs 7))))
             (car (one-of (list (list-ref ys 6) (list-ref ys 7)))))))))
             
;;diese Funktion zeigt das Kind zweier Eltern an, beziehungsweise
;;spaltet die eingegebene Liste auf, und zeigt den Schmetterling aus einer Liste an..
(define (showchild zs)
  (show-butterfly (car zs) (cadr zs) (caddr zs) (cadddr zs)))

(define (showkidsandparents p1 p2 numberkids)
  (if (> numberkids 0)
      (beside (showchild (mixparents p1 p2))
              (showkidsandparents p1 p2 (- numberkids 1)))
      (beside (showchild p1)  
       (showchild p2))
       ))




