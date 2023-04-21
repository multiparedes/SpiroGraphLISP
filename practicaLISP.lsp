; ETAPA 1: 3 PUNTS

(defun guarda-informacio () 
    (putprop 'spiro '((150 105) (144 96)) 'grans)
    (putprop 'spiro '((84 35 56) (80 33 53) (75 31 50) 
                      (72 29 48) (63 25 42) (60 23 40) 
                      (56 21 37) (52 19 35) (58 17 32) 
                      (45 16 30) (42 14 28) (40 13 27) 
                      (32 9 21) (30 8 20) (24 5 16)) 'petits)
    (putprop 'spiro 150 'rgran)
    (putprop 'spiro 50 'rpetit)
    (putprop 'spiro 3 'punt)
    (putprop 'spiro 0 'inici)
    (putprop 'spiro 1.8 'escala)
    (putprop 'spiro t 'interior)
    (putprop 'spiro 0 'x)
    (putprop 'spiro 0 'y)
    (putprop 'spiro 0.2 'pas)
)

(defun vermell ()
    (color 255 0 0)
)

(defun blau ()
    (color 0 0 255)
)

(defun verd ()
    (color 0 255 0)
)

(defun negre ()
    (color 0 0 0)
)

(defun radians (g)
    (/ (* g pi) 180)
)

(defun cercle (x y radi n)
    (mou (+ x radi) y)
    (cercle2 x y radi (/ 360 n) 0)
)

(defun cercle2 (x y radi pas angle)
    (cond ((< angle 360)
        (pinta (+ x (* radi (cos (radians (+ angle pas)))))
        (+ y (* radi (sin (radians (+ angle pas))))))
        (cercle2 x y radi pas (+ angle pas)))
        (t t)
    )
)

(defun pinta (x y)
    (draw (realpart (round (+ 320 (* 1.8 x))))
    (realpart (round (+ 187 (* 1.8 y)))))
)

(defun mou (x y)
    (move (realpart (round (+ 320 (* 1.8 x))))
    (realpart (round (+ 187 (* 1.8 y)))))
)

(defun radigran (r)
    (putprop 'spiro r 'rgran)
    (cercle (get 'spiro 'x) (get 'spiro 'y) (get 'spiro 'rgran) 100)
)

(defun radipetit (r)
    (putprop 'spiro r 'rpetit)
    (cercle (* (sin (radians (get 'spiro 'inici))) (- (get 'spiro 'rgran) (get 'spiro 'rpetit)))
            (* (cos (radians (get 'spiro 'inici))) (- (get 'spiro 'rgran) (get 'spiro 'rpetit)))
            (get 'spiro 'rpetit) 
            100)
)

(defun punt (p)
    (putprop 'spiro p 'punt)
)

(defun inici (i)
    (putprop 'spiro i 'inici)
)

(defun escala (e)
    (putprop 'spiro e 'escala)
) 

(defun posicio (x y)
    (putprop 'spiro x 'x)
    (putprop 'spiro y 'y)
) 

(defun reduir (m n)
    (list (/ m (gcd m n)) (/ n (gcd m n)))
)

; ETAPA 2 (4 PUNTS)


; FORMULA 9 - INTERIOR
; x = (R - r) * cos(r*a/R) + t * cos((1-(r/R)) * a)
; y = (R - r) * sin(r*a/R) - t * sin((1-(r/R)) * a)

(defun calcularSeguentInterior(gran petita alpha te)
    (list 
        (+ (* (- gran petita) (cos (/ (* petita alpha ) gran))) (* te (cos (* (- 1 (/ petita gran)) alpha )))) ; x
        (- (* (- gran petita) (sin (/ (* petita alpha ) gran))) (* te (sin (* (- 1 (/ petita gran)) alpha )))) ; y   
    )
)

(defun prova(gran petita alpha te)
    (* (- gran petita) (cos (/ (* petita alpha ) gran)))
)

; FORMULA 10 - EXTERIOR
; x = (R + r) * cos(r*a/R) - t * cos((1+r/R) * a)
; y = (R + r) * sin(r*a/R) - t * sin((1+r/R) * a)

(defun calcularSeguentExterior(gran petita alpha te)
    (list 
        (- (* (+ gran petita) (cos (/ (* petita alpha ) gran))) (* te (cos (* (+ 1 (/ petita gran)) alpha )))) ; x
        (- (* (+ gran petita) (sin (/ (* petita alpha ) gran))) (* te (sin (* (+ 1 (/ petita gran)) alpha )))) ; y   
    )
)

; ROTAR PUNT
; x' = x * cos(alpha) + y * sin(alpha)
; y' = -x * sin(alpha) + y * cos(alpha)

(defun rotarPunt(x y alpha)
    (list 
        (+ (* x cos(alpha)) (* y sin(alpha))); x
        (+ (* (* x -1) sin(alpha)) (* y cos(alpha))); y
    )
)

(defun spirograph (p gran petit dist inc inici)
    (cond
        (null (get 'spiro 'interior)
              ()
        )
            ; VOL PINTAR INTERIOR  
        (t (

        ))
    )
)