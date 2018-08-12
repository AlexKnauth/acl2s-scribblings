#lang racket/base

(provide branch-conditions
         non-trivial-branch-conditions
         ;; ---
         branches)

(require syntax/parse
         syntax/parse/experimental/reflect
         syntax/stx
         syntax/parse/syntax-class-or
         "stx.rkt"
         (for-template racket/base))
(module+ test
  (require rackunit)
  (define-binary-check (check-stx-datum=? a b)
    (datum=? a b)))

;; ---------------------------------------------------------

;; A BranchSpec is a syntax class that matches a syntax
;; object for a "core-form", and binds these attributes:
;;  * b.c with depth 2, conditions
;;  * b.r with depth 1, result
(define-syntax-class branch-if
  #:attributes [[b 1] [b.c 2] [b.r 1]]
  #:literals [if #%plain-app not]
  [pattern (if #true thn els)
    #:with :branches #'([() thn])]
  [pattern (if #false thn els)
    #:with :branches #'([() els])]
  [pattern (if {~and pos (#%plain-app not neg)} thn els)
    #:with :branches #'([(pos) thn]
                        [(neg) els])]
  [pattern (if cnd thn els)
    #:with :branches #'([(cnd) thn]
                        [((#%plain-app not cnd)) els])])

(define default-branch-specs
  (list (reify-syntax-class branch-if)))

(define-syntax-class/or* (branch-expr spcs)
  #:attributes [[b 1] [b.c 2] [b.r 1]]
  spcs)

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

(define (branch-conditions stx [spcs default-branch-specs] [r? any-result?])
  (syntax-parse stx
    [{~var e (expr/bs spcs r?)}
     (attribute e.b)]))

(define (non-trivial-branch-conditions stx [spcs default-branch-specs])
  (branch-conditions stx spcs non-trivial-result?))

;; spcs stands for branch specs
(define-syntax-class (expr/bs spcs r?)
  #:attributes [[b 1] [b.c 2] [b.r 1]]
  [pattern {~var e (branch-expr spcs)}
    #:with {~var || (branches-extend spcs r?)} #'(e.b ...)]
  [pattern e
    #:with :branches
    (if (r? #'e)
        #'([() e])
        #'())])

(define-syntax-class (branch-extend spcs r?)
  #:attributes [[b 1] [b.c 2] [b.r 1]]
  [pattern [() e]
    #:with {~var || (expr/bs spcs r?)} #'e]
  [pattern [(x ... {~var c (condition/b spcs)} y ...) r]
    #:with {~var || (branches-extend spcs r?)}
    #'([(x ... c.t.b.c ... y ...) r] ...)]
  [pattern [(c ...) {~var r (expr/bs spcs r?)}]
    #:with :branches #'([(c ... r.b.c ...) r.b.r] ...)])

(define-syntax-class (branches-extend spcs r?)
  #:attributes [[b 1] [b.c 2] [b.r 1]]
  [pattern ({~var br (branch-extend spcs r?)} ...)
    #:with :branches #'(br.b ... ...)])

(define-syntax-class (branch->condition spcs)
  #:attributes [[t.b 1] [t.b.c 2] [t.b.r 1]
                [f.b 1] [f.b.c 2] [f.b.r 1]]
  [pattern [(c ...) {~and e #true}]
    #:with t:branches #'([(c ...) e])
    #:with f:branches #'()]
  [pattern [(c ...) {~and e #false}]
    #:with t:branches #'()
    #:with f:branches #'([(c ...) e])]
  [pattern [(c ...) e]
    #:with t:branches #'([(c ... e) #true])
    #:with f:branches #'([(c ... (#%plain-app not e)) #false])])

(define-syntax-class (branches->condition spcs)
  #:attributes [[t.b 1] [t.b.c 2] [t.b.r 1]
                [f.b 1] [f.b.c 2] [f.b.r 1]]
  [pattern ({~var br (branch->condition spcs)} ...)
    #:with t:branches #'(br.t.b ... ...)
    #:with f:branches #'(br.f.b ... ...)])

(define-syntax-class (condition/b spcs)
  #:attributes [[t.b 1] [t.b.c 2] [t.b.r 1]
                [f.b 1] [f.b.c 2] [f.b.r 1]]
  #:literals [#%plain-app not]
  [pattern {~and e #true}
    #:with t:branches #'([() e])
    #:with f:branches #'()]
  [pattern {~and e #false}
    #:with t:branches #'()
    #:with f:branches #'([() e])]
  [pattern {~and e (#%plain-app not {~var neg (condition/b spcs)})}
    #:with t:branches #'(neg.f.b ...)
    #:with f:branches #'(neg.t.b ...)]
  [pattern {~var e (branch-expr spcs)}
    #:with {~var || (branches->condition spcs)} #'(e.b ...)])

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

