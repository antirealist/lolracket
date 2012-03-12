#lang racket

;;; Racket translation of adventure game from Land Of Lisp
;;; Steve Austin 
;;; March 2012

(require mzlib/string)  ;; provides read-from-string-all


(define-namespace-anchor a)
(define *ns* (namespace-anchor->namespace a))   ;; create a namespace for eval


(define *nodes* '((living-room (you are in the living room. 
                                    a wizard is snoring loudly on the couch.))
                  (garden (you are in a beautiful garden. 
                               there is a well in front of you.))
                  (attic (you are in the attic. 
                              there is a giant welding torch in the corner.))))

(define *edges* '((living-room (garden west door)
                               (attic upstairs ladder))                             
                  (garden (living-room east door))
                  (attic (living-room downstairs ladder))))

(define *objects* '(whiskey bucket frog chain))

(define *object-locations* '((whiskey living-room)
                             (bucket living-room)
                             (chain garden)
                             (frog garden)))

(define *location* 'living-room)


(define (describe-path edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(define (describe-paths location edges)
  (apply append (map describe-path (cdr (assoc location edges)))))


(define (describe-location location nodes)
  (cadr (assoc location nodes)))

(define (objects-at loc objs obj-locs)
  (filter (lambda (obj)
            (eq? (cadr (assoc obj obj-locs)) loc))
          objs))

(define (describe-objects loc objs obj-loc)
  (apply append (map (lambda (obj)
                       `(you see a ,obj on the floor.)) 
                     (objects-at loc objs obj-loc))))

(define (look)
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

(define (walk direction)
  (let ((next (findf (lambda (arg) (eq? direction (cadr arg))) 
                     (cdr (assoc *location* *edges*)))))
    (if next
        (begin (set! *location* (car next))
               (look))
        '(you cannot go that way.))))

(define-syntax-rule (push! el lst)
  (set! lst (cons el lst)))

(define (pickup object)
  (cond [(member object
                 (objects-at *location* *objects* *object-locations*))
         (push! (list object 'body) *object-locations*)
         `(you are now carrying the ,object)]
        [else '(you cannot get that.)]))

(define (inventory)
  (cons 'items- (objects-at 'body *objects* *object-locations*)))


(define (game-read)
  (let ([cmd (read-from-string-all (read-line))])
    (define (quote-it x)
      (list 'quote x))
    (cons (car cmd) (map quote-it (cdr cmd)))))

(define *allowed-commands* '(look walk pickup inventory))

(define (game-eval sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp *ns*)
      '(i do not know that command.)))

;; symlist->charlist assumes that the list to be tweaked consists of symbols
;; it does not handle embedded strings!

(define (symlist->charlist symlist)
  (string->list (string-join (map symbol->string symlist) " ")))


(define (tweak-text lst caps lit)
  (if (null? lst) '()
      (begin
        (let ([item (car lst)]
              [rest (cdr lst)])
          (cond [(eq? item #\space) (cons item (tweak-text rest caps lit))]
                [(member item '(#\! #\? #\.)) (cons item (tweak-text rest #t lit))]
                [(eq? item #\") (tweak-text rest caps (not lit))]
                [lit (cons item (tweak-text rest null lit))]
                [caps (cons (char-upcase item) (tweak-text rest #f lit))]
                [#t (cons item (tweak-text rest #f #f))])))))


(define (game-print symlist)
  (let ([charlist (symlist->charlist symlist)])
    (begin
      (display (list->string (tweak-text charlist #t #f)))
      (newline))))
  
(define (game-repl)
    (let ([cmd (game-read)])
      (unless (equal? (car cmd) 'quit)
        (game-print (game-eval cmd))
        (game-repl))))

(game-repl)









  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  