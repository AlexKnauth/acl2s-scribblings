#lang info

;; --------------------------------------------------------------

;; Pkg Info

(define collection "acl2s-scribblings")

(define deps
  '("base"
    "rackunit-lib"
    "syntax-classes-lib"
    ))

(define build-deps
  '("racket-doc"
    "scribble-lib"
    ))

;; --------------------------------------------------------------

;; Collection Info

(define scribblings
  '(["scribblings/acl2s.scrbl" ()]))

;; --------------------------------------------------------------

