#lang racket/base

(provide acl2s-print)

(require racket/list
         racket/match
         racket/pretty
         racket/struct)

(define ((acl2s-print old-print) v)
  (parameterize ([current-print old-print])
    (cond
      [(void? v) (void)]
      [else (pretty-write (convert v))])))

;; converts a value into an s-expr for printing
;; similar to print-convert from mzlib/pconvert,
;; but for ACL2s-style printing.
(define (convert v)
  (match v

    ;; base cases
    ['t 't]
    ['nil 'nil]
    [(? symbol? s) `(quote ,s)]
    [(? number? n) n]
    [(? string? s) s]

    ;; recursive cases:
    ;; list
    [(list-rest xs ... 'nil)
     `(list ,@(map convert xs))]
    ;; cons
    [(cons x (and y (not (cons _ _))))
     `(cons ,(convert x) ,(convert y))]
    ;; list*
    [(list-rest xs ..1 (and y (not (cons _ _))))
     `(list* ,@(map convert xs) ,(convert y))]

    ;; records
    [(? struct? s)
     (define-values [struct-type skipped?] (struct-info s))
     (define-values [name N auto acc mut imm sup sk?]
       (struct-type-info struct-type))
     (define lst (struct->list s))
     (unless (= N (length lst)) (error 'print "bad struct"))
     `(,name ,@(map convert lst))]
    ))

