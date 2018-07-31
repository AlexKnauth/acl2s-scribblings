#lang racket/base

(provide branch-conditions
         non-trivial-branch-conditions
         ;; ---
         branches)

(require syntax/parse
         syntax/stx
         "stx.rkt"
         (for-template racket/base))
(module+ test
  (require rackunit)
  (define-binary-check (check-stx-datum=? a b)
    (datum=? a b)))

;; ---------------------------------------------------------

;; A Branches is a [Listof Branch]
;; A Branch is a [List Conditions Result]
;; A Conditions is a [Listof Condition]
;; A Condition is a Syntax object that should produce a
;;   boolean.
;; A Result is a Syntax object that might be a result.
(define-syntax-class branches
  #:attributes [[b 1] [b.c 2] [b.r 1]]
  [pattern (b:branch ...)])
(define-syntax-class branch
  #:attributes [[c 1] [r 0]]
  [pattern [:conditions r:expr]])
(define-syntax-class conditions
  #:attributes [[c 1]]
  [pattern (c:expr ...)])

;; A branch is "trivial" if it leads to #true.
;; For instance in this expression:
;; (if a
;;     (if b
;;         (f x)
;;         (if c
;;             #true
;;             (g y))
;;     (if d
;;         (h z)
;;         #true))
;; The branches [(a ~b c) #t] and [(~a ~d) #t] are trivial.

(define (any-result? x) #true)
(define (non-trivial-result? x)
  (syntax-parse x [{~not #true} #true] [_ #false]))

(define (branch-conditions stx [r? any-result?])
  (syntax-parse stx
    [{~var e (expr/bs r?)}
     (attribute e.b)]))

(define (non-trivial-branch-conditions stx)
  (branch-conditions stx non-trivial-result?))

;; bcs stands for branch conditions
(define-syntax-class (expr/bs r?)
  #:attributes [[b 1] [b.c 2] [b.r 1]]
  #:literals [if]
  [pattern (if #true t e) #:with {~var || (expr/bs r?)} #'t]
  [pattern (if #false t e) #:with {~var || (expr/bs r?)} #'e]
  [pattern (if1:if (if2:if cc ct ce) t e)
    #:with {~var || (expr/bs r?)}
    #'(if2 cc
           (if1 ct t e)
           (if1 ce t e))]
  [pattern (if c {~var t (expr/bs r?)} {~var e (expr/bs r?)})
    #:with :branches
    #'([(c t.b.c ...) t.b.r] ...
       [((#%plain-app not c) e.b.c ...) e.b.r] ...)]
  [pattern e
    #:with :branches
    (if (r? #'e)
        #'([() e])
        #'())])

;; ---------------------------------------------------------

(module+ test
  (check-stx-datum=? (branch-conditions
                      #'a)
                     '([() a]))
  (check-stx-datum=? (branch-conditions
                      #'(if a b c))
                     '([(a) b]
                       [((#%plain-app not a)) c]))
  (check-stx-datum=? (branch-conditions
                      #'(if a b #true))
                     '([(a) b]
                       [((#%plain-app not a)) #true]))
  (check-stx-datum=? (branch-conditions
                      #'(if a #true c))
                     '([(a) #true]
                       [((#%plain-app not a)) c]))

  (check-stx-datum=? (branch-conditions
                      #'(if a (if b x y) (if c y z)))
                     '([(a b) x]
                       [(a (#%plain-app not b)) y]
                       [((#%plain-app not a) c) y]
                       [((#%plain-app not a) (#%plain-app not c)) z]))

  (check-stx-datum=? (branch-conditions
                      #'(if (if a b #false) x #true))
                     '([(a b) x]
                       [(a (#%plain-app not b)) #true]
                       [((#%plain-app not a)) #true]))
  (check-stx-datum=? (branch-conditions
                      #'(if (if a #true b) x #true))
                     '([(a) x]
                       [((#%plain-app not a) b) x]
                       [((#%plain-app not a) (#%plain-app not b)) #true]))

  (check-stx-datum=? (non-trivial-branch-conditions
                      #'(if (if a b #false) x #true))
                     '([(a b) x]))
  (check-stx-datum=? (non-trivial-branch-conditions
                      #'(if (if a #true b) x #true))
                     '([(a) x]
                       [((#%plain-app not a) b) x]))
  )

