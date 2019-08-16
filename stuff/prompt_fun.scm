(import (srfi :1 lists)
	(srfi :13 strings)) 

(define (main)
  (format #t "λ ")
  (format #t "~a~%" (handler (get-line (current-input-port))))
  (main))

(define (handler input)
  (cond
    [(string-prefix? "uc " input) (string-upcase   (string-drop input 3))]
    [(string-prefix? "lc " input) (string-downcase (string-drop input 3))]
    [(string-prefix? "rv " input) (string-reverse  (string-drop input 3))]
    [else input]))
