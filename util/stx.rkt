#lang racket/base

(provide datum=?
         subst)

(require racket/list)

;; datum=? : Stx Stx -> Boolean
(define (datum=? a b)
  (equal? (syntax->datum #`#,a) (syntax->datum #`#,b)))

;; subst : Stx Stx Stx [Stx Stx -> Boolean] -> Stx
(define (subst stx old new =?)
  (define e (stx-e stx))
  (restore
   stx
   (cond
     [(=? stx old) new]
     [(atomic-literal? e) stx]
     [(cons? e)
      (cons (subst (car e) old new =?)
            (subst (cdr e) old new =?))]
     [else stx])))

;; stx-e : Stx -> E
(define (stx-e stx)
  (if (syntax? stx) (syntax-e stx) stx))

;; restore : Stx E -> Stx
(define (restore stx e)
  (if (syntax? stx) (datum->syntax stx e stx stx) e))

;; atomic-literal? : E -> Boolean
(define (atomic-literal? e)
  (or (null? e) (boolean? e) (number? e) (symbol? e)
      (string? e) (bytes? e)
      (regexp? e) (byte-regexp? e)))

