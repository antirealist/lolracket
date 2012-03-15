#lang racket

;;; Racket translation of Graphviz utils from Land Of Lisp
;;; Steve Austin 
;;; March 2012

(require srfi/13)       ;; For string-map
(require mzlib/string)  ;; provides expr->string

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


;; could also use regexp-replace* and \w for this

(define (char-alphanumeric? chr)
  (or (char-alphabetic? chr) (char-numeric? chr))) 

(define (substitute-if chr a-test? str)
  (define (sub-test any-c)
    (if (a-test? any-c) chr
        any-c))
  (string-map sub-test str))

;; translated this from the CLHS
;; scheme/racket doesn't seem to offer it
;; UPDATE: yes, it does: negate
;; i'll keep this here anyway

;(define (complement pred)
;  (lambda rest-args (not (apply pred rest-args))))

;; this version of dot-name will not convert '24 to "24"
;; racket does not recognize '24 as a valid scheme symbol

(define (dot-name exp)
  (substitute-if #\_ (negate char-alphanumeric?) (symbol->string exp)))

(define *max-label-length* 30)

;; symlist->charlist assumes that the list to be tweaked consists of symbols
;; it does not handle embedded strings!

(define (symlist->string symlist)
  (string-join (map symbol->string symlist) " "))

;; I don't think we need to worry about pretty printing

(define (dot-label exp)
  (let ([s (expr->string exp)])
    (if (> (string-length s) *max-label-length*)
        (string-append (substring s 0 (- *max-label-length* 3)) "...")
        s)))

(define (nodes->dot nodes)
  (for-each (lambda (node)
              (newline)
              (display (dot-name (car node)))
              (display "[label=\"")
              (display (dot-label node))
              (display "\"];"))
            nodes))

(define (edges->dot edges)
  (for-each (lambda (node)
              (for-each (lambda (edge)
                          (newline)
                          (display (dot-name (car node)))
                          (display "->")
                          (display (dot-name (car edge)))
                          (display "[label=\"")              
                          (display (dot-label (cdr edge)))
                          (display "\"];"))
                        (cdr node)))
            edges))

(define (graph->dot nodes edges)
  (display "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (display "}"))

;; I can't believe this actually works!!!
;; But it does!!!

(define (dot->png fname mythunk)
  (let ([out (open-output-file fname #:exists 'truncate)])
    (parameterize ([current-output-port out])
      (mythunk)
      (close-output-port out)))
  (system (string-append "/usr/local/bin/dot -Tpng -O " fname)))

(define (graph->png fname nodes edges)
  (dot->png fname
            (lambda ()
              (graph->dot nodes edges))))





