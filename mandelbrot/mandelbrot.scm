(import (rnrs)
	(srfi :1 lists))

(define origin 0.0+0.0i)
(define PRECISION 400)

(define (mandelbrot c z)
  (+ (* z z) c))

(define (mandelSeq c)
  (reduce (lambda (c z) (mandelbrot c z))
	  origin
	  (make-list PRECISION c)))

(define (isMandelbrot? c)
  (<= (magnitude (mandelSeq c)) 2.0))

;; compute table of complex numbers in row-major, ask for each number: isMandelbrot?
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
	     (map (lambda (colNum)
		    (isMandelbrot? (make-rectangular (+ startR (* step colNum)) rowStart)))
		  (iota stepsR))))
	 (iota stepsI))))

(define (printMandelbrot! tl br z)
  (for-each (lambda (row)
	      (for-each (lambda (x)
			  (if x (format #t " # ")
			        (format #t "   "))) row)
	      (newline))
	    (mandelField tl br z)))

(define tl -2.0+2.0i)
(define br +2.0-2.0i)
(define z 15)
(define (test) (printMandelbrot! tl br z))

;; When coming up with reductions or folds it's useful to remember:
;; you may intuitively use fold as an iterator over a list, but in
;; reality it's a recursive function, that recurses left or right.
;; That means: it's really a repeated application of the supplied
;; lambda.

;; TRY PARINFER
