#lang racket

;;; Racket translation of Graphviz utils from Land Of Lisp
;;; Steve Austin 
;;; March 2012

(require srfi/13)   ;; For string-map

(define *wizard-nodes* '((living-room (you are in the living room. 
                                           a wizard is snoring loudly on the couch.))
                         (garden (you are in a beautiful garden. 
                                      there is a well in front of you.))
                         (attic (you are in the attic. 
                                     there is a giant welding torch in the corner.))))

(define *wizard-edges* '((living-room (garden west door)
                                      (attic upstairs ladder))                             
                         (garden (living-room east door))
                         (attic (living-room downstairs ladder))))


(define (char-alphanumeric? chr)
  (or (char-alphabetic? chr) (char-numeric? chr))) 

(define (substitute-if chr a-test? str)
  (define (sub-test any-c)
    (if (a-test? any-c) chr
        any-c))
  (string-map sub-test str))

;; translated this from the CLHS
;; scheme/racket doesn't seem to offer it

(define (complement pred)
  (lambda rest-args (not (apply pred rest-args))))

;; this version of dot-name will not convert '24 to "24"
;; racket does not recognize '24 as a valid scheme symbol

(define (dot-name exp)
  (substitute-if #\_ (complement char-alphanumeric?) (symbol->string exp)))



