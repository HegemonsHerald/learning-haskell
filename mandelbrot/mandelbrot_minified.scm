(import (rnrs)
	(srfi :1 lists))

(define PRECISION 400)

(define (mandelbrot c z)   (+ (* z z) c))
(define (mandelSeq c)	   (reduce (lambda (c z) (mandelbrot c z)) 0.0+0.0i (make-list PRECISION c)))
(define (isMandelbrot? c)  (<= (magnitude (mandelSeq c)) 2.0))

(define (mandelField tl br z)
  (let* [(startI (imag-part tl))
	 (startR (real-part tl))
	 (height (- (imag-part br) startI))
	 (width  (- (real-part br) startR))
	 (step   (/ 1 z))				    ; stepsize
	 (stepsI (flonum->fixnum (abs (/ height step))))    ; number of steps in imag direction
	 (stepsR (flonum->fixnum (abs (/  width step))))]   ; number of steps in real direction

    (map (lambda (rowNum)
	   (let [(rowStart (- startI (* step rowNum)))]
	     (map (lambda (colNum) (isMandelbrot? (make-rectangular (+ startR (* step colNum)) rowStart)))
		  (iota stepsR))))
	 (iota stepsI))))

(define (printMandelbrot! tl br z)
  (for-each (lambda (row) (for-each (lambda (x)
				      (if x (format #t " # ")
					    (format #t "   "))) row) (newline)) (mandelField tl br z)))

;; Scheme may not be aesthetically pleasing, but it sure is easy to use,
;; although type inference makes haskell just as much of a scripting language.
;; Still, I want Scheme in my life, so I'll have to force myself to use it
;; occasionally.
;; Do a script here and there, master it and the SRFIs. Then it won't be hard
;; at all. (I mean currently I have the Haskell idioms going on, but Scheme has
;; its own idioms, which I haven't even properly learned yet.)

;; I don't need Scheme and I'm not sure I really want Scheme. But I want the LISPs to prevail and spread, so I'm damn well going to use them!
;; Scheme will NOT become another perl or raku. Racket has a lot to offer, so does Clojure and after mastering it even plain Chez.


;;  ___ _   _                                           _   _
;; |_ _| |_( )___   _ __   ___     __ _  ___   ___   __| | | |_ ___
;;  | || __|// __| | '_ \ / _ \   / _` |/ _ \ / _ \ / _` | | __/ _ \
;;  | || |_  \__ \ | | | | (_) | | (_| | (_) | (_) | (_| | | || (_) |
;; |___|\__| |___/ |_| |_|\___/   \__, |\___/ \___/ \__,_|  \__\___/
;;                                |___/
;;
;;   ___ ___  _ __ ___  _ __   __ _ _ __ ___
;;  / __/ _ \| '_ ` _ \| '_ \ / _` | '__/ _ \
;; | (_| (_) | | | | | | |_) | (_| | | |  __/
;;  \___\___/|_| |_| |_| .__/ \__,_|_|  \___|
;;                     |_|
;;  _
;; | | __ _ _ __   __ _ _   _  __ _  __ _  ___  ___
;; | |/ _` | '_ \ / _` | | | |/ _` |/ _` |/ _ \/ __|
;; | | (_| | | | | (_| | |_| | (_| | (_| |  __/\__ \_
;; |_|\__,_|_| |_|\__, |\__,_|\__,_|\__, |\___||___(_)
;;                |___/             |___/
;;   ___  _   _                        _
;;  / _ \| |_| |__   ___ _ ____      _(_)___  ___    ___  _ __   ___
;; | | | | __| '_ \ / _ \ '__\ \ /\ / / / __|/ _ \  / _ \| '_ \ / _ \
;; | |_| | |_| | | |  __/ |   \ V  V /| \__ \  __/ | (_) | | | |  __/
;;  \___/ \__|_| |_|\___|_|    \_/\_/ |_|___/\___|  \___/|_| |_|\___|
;;
;;            _       _     _              _
;;  _ __ ___ (_) __ _| |__ | |_  __      _(_)_ __
;; | '_ ` _ \| |/ _` | '_ \| __| \ \ /\ / / | '_ \
;; | | | | | | | (_| | | | | |_   \ V  V /| | | | |_ _ _
;; |_| |_| |_|_|\__, |_| |_|\__|   \_/\_/ |_|_| |_(_|_|_)
;;              |___/
