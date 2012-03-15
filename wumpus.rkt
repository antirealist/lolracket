#lang racket

(require "graphviz.rkt")
(require srfi/1)  ;; for lset-difference

(define *congestion-city-nodes* null)
(define *congestion-city-edges* null)
(define *visited-nodes* null)
(define *node-num* 30)
(define *edge-num* 45)
(define *worm-num* 3)
(define *cop-odds* 15)

(define (random-node)
  (add1 (random *node-num*)))

;; can't use unless here - result is #<void> if test is true

(define (edge-pair a b)
  (if (not (eq? a b))
    (list (cons a b) (cons b a))
    null))

;; (loop repeat n collect l) = (for/list ([i (in-range 1 n)]) l)

(define (make-edge-list)
  (apply append (for/list ([i (in-range 1 *edge-num*)])
                  (edge-pair (random-node) (random-node)))))

(define (direct-edges node edge-list)
  (filter (lambda (x)
            (eq? (car x) node))
          edge-list))

(define-syntax-rule (push! el lst)
  (set! lst (cons el lst)))

;(define test-el '((1 . 2) (2 . 1) (3 . 4) (4 . 3) (2 . 7) (7 . 2) (7 . 9) (9 . 7) (8 . 3) (3 . 8)))
;(define test-nodes '(1 2 3 4 5 6 7 8 9))


(define (get-connected node edge-list)
  (let ([visited null])
    (define (traverse node)
      (unless (member node visited)
        (push! node visited)
        (for-each (lambda (edge)
                    (traverse (cdr edge)))
                  (direct-edges node edge-list))))
    (traverse node)
    visited))

(define (find-islands nodes edge-list)
  (let ([islands null])
    (define (find-island nodes)
      (let* ([connected (get-connected (car nodes) edge-list)]
             [unconnected (lset-difference eq? nodes connected)])
        (push! connected islands)
        (when (not (empty? unconnected))
          (find-island unconnected))))
    (find-island nodes)
    islands))



(define (connect-with-bridges islands)
  (if (not (empty? (cdr islands)))
    (append (edge-pair (caar islands) (caadr islands))
            (connect-with-bridges (cdr islands)))
    null))

(define (connect-all-islands nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

;(connect-all-islands test-nodes test-el)








