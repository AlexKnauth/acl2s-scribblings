#lang racket/base

(provide check-generator-predicate
         contract-generator)

(require racket/contract
         racket/list
         racket/stream
         racket/splicing
         rackunit)

;; ---------------------------------------------------------

;; Check that every value produced by the generator passes
;; the predicate. If there is a failure, collect up to three
;; failures and display them all as counterexamples.
;; TODO: should it be a time-out instead of a set number of
;;       tries?
(define-check (check-generator-predicate gen pred timeout)
  (define t0 (current-seconds))
  (define t10 (+ t0 timeout))
  (define (stop? t) (< t10 t))
  (define-values [fails success-n]
    (for/fold ([fails '()]
               [success-n 0])
              ([i (in-range 10000)]
               [t (in-producer current-seconds stop?)]
               [v (in-stream gen)])
      #:break (<= 3 (length fails))
      (cond [(pred v)
             ;; success: don't add to fails
             (values fails (+ success-n 1))]
            [else
             ;; failure: add it to fails
             (values (cons v fails) success-n)])))
  (cond [(empty? fails)
         (printf "check-generator-predicate: ~v successes\n" success-n)
         (void)]
        [else (with-check-info
               (['counterexamples fails])
               (fail-check))]))

;; ---------------------------------------------------------

(splicing-local
    [(define failure-sym (gensym 'failure-sym))
     (define (fail-proc no-gen?)
       (when no-gen?
         (error 'contract-generator "no generator exists"))
       failure-sym)]

  (define (contract-generator ctc [fuel 5])
    (define v
      (contract-random-generate ctc fuel fail-proc))
    (cond
      [(eq? v failure-sym) empty-stream]
      [else (stream-cons v (contract-generator ctc fuel))])))

;; ---------------------------------------------------------

(module+ test
  (check-generator-predicate
   (contract-generator exact-nonnegative-integer? 1)
   (λ (x)
     (let ([n (modulo x 100000)])
       (= (for/sum ([i (in-range (add1 n))]) i)
          (/ (* n (add1 n)) 2))))
   10))

