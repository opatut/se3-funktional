#lang racket
(require 2htdp/image)
(require lang/posn)

; Vorsicht, das Zeichnen der Scene in Aufgabe 2 dauert ca. 3-5 Minuten.

;;; Aufgabe 1

;; Lineare vs. Baumrekursion
; kopfstueck: linear, maximal ein Aufruf von kopfstueck im Funktionskörper.
; endstueck: linear, ebenso
; merge: linear, da `if` als Sonderform äußere Reduktion verwendet, und maximal einer der `merge`-Aufrufe stattfindet
; merge-sort: baumrekursiv, da im Normalfall 2x `merge-sort` aufgerufen wird

;; Geschachtelte Rekursion liegt nirgendwo vor. Kein Aufruf (rot markiert) beinhaltet
;; den Rückgabewert eines Aufrufes ebendieser Funktion in der Argumente-Liste.

;; Direkte vs. indirekte Rekursion
; Alle Funktionen sind direkt, keine indirekt rekursiv. Die einzige Funktion, die eine andere
; von uns definierte Funktion aufruft, ist merge-sort (ruft `merge` auf), allerdings ruft
; `merge` umgekehrt nicht `merge-sort` auf, also haben wir keinen Zyklus. Alle anderen
; Funktionen "bleiben unter sich".

;; Endrekursion
; kopfstueck: nein, da mit dem Ergebnis des Rekursionsschritt `cons` aufgerufen wird
; endstueck: ja, der Rekursionsschritt steht eigenständig in `cond`/`else` und sein Ergebnis wird direkt/unverändert zurückgegeben
; merge: nein, auch hier steht der Rekursionsschritt in einem `cons`
; merge-sort: die Ergebnisse (Baumrekursion, kann gar nicht endrekursiv sein!) der Rekursionsschritte werden dann auch noch ge-`merge`-d.


;;; Aufgabe 2

(display "Bitte warten, die nächsten freien Rentiere stehen Ihnen bald zur Verfügung...")

;;; Teil 1: Wolken

; Generiert eine Wolke in Reihe `num`. Dies ist wichtig für die Bestimmung der Farbe
(define (cloud num)
  (put-pinhole 30 0
    (ellipse 120 70 'solid
             (make-color (- 200 (* num 20)) (- 200 (* num 20)) (- 255 (* num 20))))))

; Generiert eine Reihe Wolken, welche alle die gleiche Farbe haben
(define (cloud-row count num)
  (if (= count 1) (cloud num)
      (overlay/pinhole (put-pinhole -30 0 (cloud-row (- count 1) num)) (cloud num))))

; Generiert `count` Wolkenreihen, hintereinander
(define (clouds count)
  (if (= count 1) (cloud-row 15 1)
      (overlay/pinhole (put-pinhole (if (= 0 (modulo count 2)) 90 30) -50 (clouds (- count 1))) (cloud-row 15 count))))

; Generiert die Wolken für das Endergebnis
(define the-clouds (clouds 8))

;;; Teil 2: Schneeflocken

(define size 20)
(define radius (/ size 2))
(define factor 0.48)
(define line-color (make-color 255 255 255))
(define line-width 1)
(define length size)

; Zeichnet eine baumartige Struktur, welche als Teil einer Schneeflocke benutzt
; wird. Diese Struktur beinhaltet sich selbst, 4-fach verschachtelt. Graphisch
; liegt sogar Baumstruktur vor, jedoch zeichnen wir das Ergebnis nur mehrfach
; übereinander, haben also (dank `let`) keine programmatische Baumrekursion. Dies
; verbessert die Laufzeit, bei gleichem Ergebnis.
(define (snowflake-step step)
  (if (> step 4) empty-image
      (let* ([line-width (round (/ line-width (expt 0.8 step)))]
             [line-pen (make-pen line-color line-width 'solid 'butt 'round)]
             [child (scale factor (snowflake-step (+ step 1)))]
             [side-child (if (= step 2) (rotate 180 child) child)]
             [children (overlay/pinhole (rotate 0 child)
                                        (rotate -60 side-child)
                                        (rotate 60 side-child))])
        (overlay/offset
         (overlay/offset (put-pinhole 0 length (line 0 length line-pen))
                         0 (- length)
                         children)
         0 (/ length 2) (scale 0.5 children)))))

; Zeichnet eine Schneeflocke aus 6 Schneeflocken-Teilen, jeweils um 60 Grad gedreht.
(define (snowflake)
  (let ((a (snowflake-step 0)))
    (clear-pinhole (overlay/pinhole (rotate 0 a)
                                    (rotate 60 a)
                                    (rotate 120 a)
                                    (rotate 180 a)
                                    (rotate 240 a)
                                    (rotate 300 a)))))

; Eine Schneeflocke, fertig gezeichnet.
(define a-snowflake (snowflake))

;;; Teil 3: Rentiere

; Hier ein paar Polygon-Vertexlisten, die wir später zum Zeichnen des Rentiers brauchen.
(define p-leg (list (make-posn 140 1021) (make-posn 143 1014) (make-posn 136 1008) (make-posn 122 1002) (make-posn 116 1009) ))
(define p-antlers (list (make-posn 122 923) (make-posn 144 912) (make-posn 151 899) (make-posn 151 888) (make-posn 148 879) (make-posn 157 866) (make-posn 153 863) (make-posn 145 871) (make-posn 144 863) (make-posn 136 865) (make-posn 140 873) (make-posn 142 885) (make-posn 135 882) (make-posn 129 879) (make-posn 126 883) (make-posn 133 889) (make-posn 142 893) (make-posn 142 902) (make-posn 124 914) (make-posn 120 913) (make-posn 122 910) (make-posn 109 902) (make-posn 114 893) (make-posn 121 890) (make-posn 120 885) (make-posn 111 888) (make-posn 112 878) (make-posn 106 878) (make-posn 105 888) (make-posn 107 892) (make-posn 104 896) (make-posn 96 890) (make-posn 94 886) (make-posn 104 865) (make-posn 118 857) (make-posn 126 858) (make-posn 126 849) (make-posn 116 851) (make-posn 110 841) (make-posn 103 844) (make-posn 109 852) (make-posn 101 857) (make-posn 87 849) (make-posn 80 855) (make-posn 96 861) (make-posn 85 889) (make-posn 98 903) (make-posn 111 911) (make-posn 109 920) ))
(define p-body (list (make-posn 5 1051) (make-posn 15 1050) (make-posn 21 1043) (make-posn 36 1039) (make-posn 43 1031) (make-posn 51 1030) (make-posn 57 1021) (make-posn 57 1015) (make-posn 64 1022) (make-posn 72 1024) (make-posn 81 1027) (make-posn 94 1026) (make-posn 104 1021) (make-posn 119 1024) (make-posn 109 1030) (make-posn 106 1037) (make-posn 109 1045) (make-posn 116 1045) (make-posn 118 1037) (make-posn 136 1029) (make-posn 145 1022) (make-posn 137 1015) (make-posn 122 1009) (make-posn 127 1004) (make-posn 139 1001) (make-posn 142 991) (make-posn 147 977) (make-posn 142 963) (make-posn 135 954) (make-posn 149 954) (make-posn 155 947) (make-posn 155 938) (make-posn 147 936) (make-posn 134 930) (make-posn 124 922) (make-posn 108 918) (make-posn 93 916) (make-posn 88 920) (make-posn 93 926) (make-posn 104 927) (make-posn 106 933) (make-posn 108 939) (make-posn 116 946) (make-posn 111 960) (make-posn 102 970) (make-posn 90 981) (make-posn 71 983) (make-posn 52 984) (make-posn 36 982) (make-posn 39 972) (make-posn 46 966) (make-posn 31 968) (make-posn 24 988) (make-posn 32 998) (make-posn 30 1015) (make-posn 36 1021) (make-posn 24 1031) (make-posn 8 1034) (make-posn 8 1039) (make-posn 1 1044) (make-posn 1 1049) ))
(define p-ear (list (make-posn 107 921) (make-posn 104 925) (make-posn 96 923) (make-posn 94 921) (make-posn 99 920) ))
(define p-tail (list (make-posn 39 972) (make-posn 49 965) (make-posn 29 968) ))
(define p-chest (list (make-posn 143 967) (make-posn 142 967) (make-posn 140 980) (make-posn 139 983) (make-posn 139 987) (make-posn 138 992) (make-posn 138 995) (make-posn 139 1001) (make-posn 147 977) ))
(define p-mouth (list (make-posn 155 949) (make-posn 148 947) (make-posn 146 947) (make-posn 145 947) (make-posn 147 949) (make-posn 154 949) ))

; Noch ein paar eigene Farben definieren...
(define brown (make-color 110 50 0))
(define lightbrown (make-color 160 100 40))

; ...und erstmal die Einzelteile zeichnen.
(define leg (polygon p-leg 'solid lightbrown))
(define antlers (polygon p-antlers 'solid lightbrown))
(define body (polygon p-body 'solid brown))
(define ear (polygon p-ear 'solid lightbrown))
(define tail (polygon p-tail 'solid 'white))
(define chest (polygon p-chest 'solid 'white))
(define mouth (polygon p-mouth 'solid 'black))
(define nose (ellipse 8 8 'solid 'red))
(define eye (ellipse 8 5 'solid 'black))

; Ein Rentier aus den einzelnen Polygonen übereinanderlegen.
(define (reindeer)
  (overlay/offset ear -20 24
  (overlay/offset mouth -70 -2
  (overlay/offset eye -43 16
  (overlay/offset chest -64 -36
  (overlay/offset nose -76 6
  (overlay/offset tail 39 -23
   (overlay/offset (overlay/offset body 38 -102 antlers) 55 67 leg))))))))

; Dieses Rentier zeichnen wir nun.
(define a-reindeer (reindeer))

; Hier malen wir mehrere (`count`) Rentiere, wobei wir die vorher gezeichneten
; in jedem Schritt kleiner skalieren. So entsteht eine Rentier-Familie :)
(define (reindeers count)
  (if (= count 1) a-reindeer
  (beside/align 'bottom
     (scale 0.8 (reindeers (- count 1))) a-reindeer)))

; Unsere Rentier-Familie besteht aus 5 Rentieren.
(define some-reindeers (reindeers 5))


;;; Gesamtergebnis

; In der Scene liegen zuhinterst die Wolken, vor dunkelblauem Hintergrund (Nachthimmel).
(define a-scene (place-image the-clouds 400 700
                             (empty-scene 800 800 (make-color 20 20 80))))

; Das Schneeflocken-Panorama platziert viele kleine und große Schneeflocken auf unserer Scene.
(define (snowflakepanorama count)
  (if (< count 1)
      a-scene
      (place-image
       (rotate (random 360)
               (scale (* (+ (random 40) 60) 0.01) a-snowflake))
       (random 800)
       (random 800)
       (snowflakepanorama (- count 1)))))

(define a-panorama (snowflakepanorama 35))

; Nun fliegt im Vordergrund noch die Rentierfamilie vorbei...
(overlay some-reindeers a-panorama)

; Fröhliche Weihnachten!