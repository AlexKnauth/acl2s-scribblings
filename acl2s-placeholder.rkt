#lang racket/base

(require (prefix-in rkt: racket/base)
         racket/list
         racket/match
         rackunit
         syntax/parse/define
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse/class/local-value
                     "util/stx.rkt"))

;; -------------------------------------------------------

(provide #%datum quote t nil list)

(define-syntax-parser #%datum
  [(_ . x:number)
   (unless (exact? (syntax-e #'x))
     (raise-syntax-error
      #f "only exact rational numbers allowed" #'x))
   #'(rkt:quote x)]
  [(_ . s:str)
   #'(rkt:quote s)]
  [(_ . c:char)
   #'(rkt:quote c)]
  [(_ . b:boolean)
   (if (syntax-e #'b)
       (raise-syntax-error
        #f "true is written as `t`" #'b)
       (raise-syntax-error
        #f "false is written as `nil`" #'b))])

(define-syntax-parser quote
  [(_ s:id) #'(rkt:quote s)]
  [(_ x:number) #'(#%datum . x)]
  [(_ s:str) #'(#%datum . s)]
  [(_ c:char) #'(#%datum . c)]
  [(_ b:boolean) #'(#%datum . b)]
  [(_ ()) #'(rkt:quote nil)]
  [(_ (a ...)) #'(list (quote a) ...)]
  [(_ (a ... . b)) #'(list* (quote a) ... (quote b))])

(define t 't)
(define nil 'nil)
(define (list . args)
  (foldr cons nil args))

;; -------------------------------------------------------

(provide equal check=)

(define (equal a b) (rkt->bool (equal? a b)))

(define-binary-check (check= a b) (equal? a b))

(define (bool->rkt v)
  (case v
    [(t) (rkt:quote #true)]
    [(nil) (rkt:quote #false)]
    [else (error 'boolean "should be `t` or `nil`, given ~v" v)]))

(define (rkt->bool v)
  (rkt:if v 't 'nil))

;; -------------------------------------------------------

;; If

(provide if)

(define-syntax-parser if
  [(_ condition:expr then:expr else:expr)
   #'(rkt:if (bool->rkt condition) then else)])

;; -------------------------------------------------------

(provide defunc let let*)

(define-syntax-parser defunc
  #:datum-literals [:input-contract :output-contract]
  [(_ f:id (x:id ...)
     :input-contract in-ctc:expr
     :output-contract out-ctc:expr
     body:expr)
   #:with result:id (generate-temporary #'result)
   #:with out-ctc*:expr (subst #'out-ctc #'(f x ...) #'result datum=?)
   #:fail-when (and
                (datum=? #'out-ctc #'out-ctc*)
                (not (datum=? #'out-ctc #'t))
                #'out-ctc)
   (format "output contract should refer to ~s"
           (syntax->datum #'(f x ...)))
   #'(define (f x ...)
       (if in-ctc
           (let ([result body])
             (if out-ctc*
                 result
                 (error 'f "output contract violation")))
           (error 'f "input contract violation")))])

;; -------------------------------------------------------

;; Conditionals

(provide cond)

(define-syntax-parser cond
  #:literals [t]
  ;; technically, this should return `nil`. But in my
  ;; version, that fall-through behavior is deprecated.
  [(_) #'(error 'cond "nil fall-through is deprecated")]
  ;; else is called `t` in this language
  [(_ (t a)) #'a]
  [(_ (q a) (q* a*) ...)
   #'(if q a (cond (q* a*) ...))])

;; -------------------------------------------------------

;; Basic Boolean Operations

(provide and or implies not booleanp)

(define-syntax-parser and
  [(_) #''t]
  [(_ a:expr b:expr ...)
   #'(if a (and b ...) 'nil)])

(define-syntax-parser or
  [(_) #''nil]
  [(_ a:expr b:expr ...)
   #'(if a 't (or b ...))])

(define-syntax-parser implies
  [(_ a:expr b:expr)
   #'(if a b 't)])

(define (booleanp v)
  (case v
    [(t) 't]
    [(nil) 't]
    [else 'nil]))

(defunc not (b)
  :input-contract (booleanp b)
  :output-contract (booleanp (not b))
  (case b [(t) 'nil] [(nil) 't]))

;; -------------------------------------------------------

;; Basic Pair and List Operations

(provide cons consp atom first rest listp endp)

(define (consp v) (rkt->bool (cons? v)))
(define (atom v) (rkt->bool (rkt:not (cons? v))))

(defunc first (p)
  :input-contract (consp p)
  :output-contract t
  (car p))

(defunc rest (p)
  :input-contract (consp p)
  :output-contract t
  (cdr p))

(define (listp v)
  (if (consp v)
      (listp (rest v))
      (equal v nil)))

(defunc endp (l)
  :input-contract (listp l)
  :output-contract (booleanp (endp l))
  (rkt->bool (rkt:not (cons? l))))

;; -------------------------------------------------------

;; Numbers

(provide integerp rationalp natp posp
         + * unary-- unary-/ - /
         < > <= >=
         numerator denominator)

(define (integerp v)
  (rkt->bool (exact-integer? v)))

(define (rationalp v)
  (rkt->bool (rkt:and (rational? v) (exact? v))))

(define (natp v)
  (rkt->bool (exact-nonnegative-integer? v)))

(define (posp v)
  (rkt->bool (exact-positive-integer? v)))

(defunc < (a b)
  :input-contract (and (rationalp a) (rationalp b))
  :output-contract (booleanp (< a b))
  (rkt->bool (rkt:< a b)))

(defunc > (a b)
  :input-contract (and (rationalp a) (rationalp b))
  :output-contract (booleanp (> a b))
  (rkt->bool (rkt:> a b)))

(defunc <= (a b)
  :input-contract (and (rationalp a) (rationalp b))
  :output-contract (booleanp (<= a b))
  (rkt->bool (rkt:<= a b)))

(defunc >= (a b)
  :input-contract (and (rationalp a) (rationalp b))
  :output-contract (booleanp (>= a b))
  (rkt->bool (rkt:>= a b)))

(defunc unary-- (a)
  :input-contract (rationalp a)
  :output-contract (rationalp (unary-- a))
  (rkt:- a))

(defunc unary-/ (a)
  :input-contract (and (rationalp a) (not (equal a 0)))
  :output-contract (rationalp (unary-/ a))
  (rkt:/ a))

(defunc - (a b)
  :input-contract (and (rationalp a) (rationalp b))
  :output-contract (rationalp (- a b))
  (rkt:- a b))

(defunc / (a b)
  :input-contract (and (rationalp a)
                       (rationalp b)
                       (not (equal b 0)))
  :output-contract (rationalp (/ a b))
  (rkt:/ a b))

;; -------------------------------------------------------

;; Symbols and Strings

(provide symbolp stringp)

(define (symbolp v)
  (rkt->bool (symbol? v)))

(define (stringp v)
  (rkt->bool (rkt:and (string? v) (immutable? v))))

;; -------------------------------------------------------

;; More List Operations

(provide len app rev in)

(defunc len (l)
  :input-contract (listp l)
  :output-contract (natp (len l))
  (cond ((endp l) 0)
        (t ;else
         (+ 1 (len (rest l))))))

(defunc app (x y)
  :input-contract (and (listp x) (listp y))
  :output-contract (listp (app x y))
  (cond ((endp x) y)
        (t ;else
         (cons (first x) (app (rest x) y)))))

(defunc rev (x)
  :input-contract (listp x)
  :output-contract (listp (rev x))
  (cond ((endp x) '())
        (t ;else
         (app (rev (rest x)) (list (first x))))))

(defunc in (a l)
  :input-contract (listp l)
  :output-contract (booleanp (in a l))
  (match l
    [(list-rest elems ... (== 'nil))
     (rkt->bool (member a elems))]))

;; -------------------------------------------------------

;; Defdata

(provide defdata
         all
         boolean
         rational integer nat pos
         symbol string
         enum oneof
         range
         record
         listof)

(define-for-syntax (err-outside-defdata stx)
  (raise-syntax-error #f "used outside of defdata" stx))

(define-syntax enum err-outside-defdata)
(define-syntax oneof err-outside-defdata)
(define-syntax range err-outside-defdata)
(define-syntax record err-outside-defdata)
(define-syntax listof err-outside-defdata)

(begin-for-syntax
  ;; data description ids for defdata
  (struct data-desc [pred-id] #:transparent)

  (define-syntax-class lt
    #:literals [< <=]
    [pattern <]
    [pattern <=]))

(define (allp v) 't)

(define-syntax all (data-desc #'allp))

(define-syntax boolean (data-desc #'booleanp))

(define-syntax rational (data-desc #'rationalp))
(define-syntax integer (data-desc #'integerp))
(define-syntax nat (data-desc #'natp))
(define-syntax pos (data-desc #'posp))

(define-syntax symbol (data-desc #'symbolp))
(define-syntax string (data-desc #'stringp))

(define-syntax-parser defdata
  [(_ name:id dd:expr)
   #:with namep
   (format-id #'name "~ap" #'name #:source #'name #:props #'name)
   #'(begin
       (define-syntax name
         (data-desc (quote-syntax namep)))
       (data-desc-defs name dd))])

(define-syntax-parser data-desc-defs
  #:literals [record]
  [(_ name:id (record (field:id . fld-desc:expr) ...))
   #:with name* (generate-temporary #'name)
   #:with name*? (format-id #'name* "~a?" #'name*)
   #:with [name*-field ...]
   (for/list ([fld (in-list (syntax->list #'[field ...]))])
     (format-id #'name* "~a-~a" #'name* fld))
   #:with namep
   (format-id #'name "~ap" #'name #:source #'name #:props #'name)
   #:with [name-field ...]
   (for/list ([fld (in-list (syntax->list #'[field ...]))])
     (format-id #'name "~a-~a" #'name fld
                #:source #'name #:props #'name))
   #:with v (generate-temporary 'v)
   #'(begin
       (struct name* [field ...] #:transparent
         #:reflection-name 'name)
       (define (namep v) (rkt->bool (name*? v)))
       (defunc name (field ...)
         :input-contract (and (data-desc-test field fld-desc)
                              ...)
         :output-contract (namep (name field ...))
         (name* field ...))
       (define (name-field v) (name*-field v))
       ...)]
  [(_ name:id dd:expr)
   #:with namep
   (format-id #'name "~ap" #'name #:source #'name #:props #'name)
   #:with v (generate-temporary 'v)
   #'(define (namep v)
       (data-desc-test v dd))])

(define-syntax-parser data-desc-test
  #:literals [t nil enum oneof range listof list cons record
              rational integer]
  [(_ v:id ddi)
   #:declare ddi (local-value data-desc?)
   #:with pred:id (data-desc-pred-id (attribute ddi.local-value))
   #'(pred v)]

  ;; singleton things
  [(_ v:id n:number) #'(equal v 'n)]
  [(_ v:id t) #'(equal v 't)]
  [(_ v:id nil) #'(equal v 'nil)]

  ;; numeric things
  [(_ v:id (range (~and dd:expr (~or rational integer))
                  range-constraints:expr))
   #'(and
      (data-desc-test v dd)
      (range-constraints-test v range-constraints))]

  ;; sum-type things
  [(_ v:id (enum lst:expr))
   #:with lst-id (syntax-local-lift-expression #'lst)
   #'(in v lst-id)]
  [(_ v:id (oneof dd:expr ...))
   #'(or (data-desc-test v dd) ...)]

  ;; product-type things
  [(_ v:id (list elem-dd:expr ...))
   #:with [elem-v ...]
   (generate-temporaries #'[elem-dd ...])
   #'(match v
       [(list-rest elem-v ... (== 'nil))
        (and (data-desc-test elem-v elem-dd) ...)]
       [_ 'nil])]
  [(_ v:id (cons a-dd:expr d-dd:expr))
   #:with [a-v d-v] (generate-temporaries '(a d))
   #'(match v
       [(cons a-v d-v)
        (and (data-desc-test a-v a-dd)
             (data-desc-test d-v d-dd))]
       [_ 'nil])]

  ;; other
  [(_ v:id (listof elem-dd:expr))
   #:with elems (generate-temporary 'elems)
   #:with ooo (quote-syntax ...)
   #:with elem-v (generate-temporary 'elem)
   #'(match v
       [(list-rest elems ooo (== 'nil))
        (rkt->bool
         (for/and ([elem-v (in-list elems)])
           (bool->rkt (data-desc-test elem-v elem-dd))))]
       [_ 'nil])]
  )

(define-syntax-parser range-constraints-test
  #:datum-literals [_]
  [(rct v:id (_ cmp:lt (~and high:expr (~not _))))
   #'(cmp v high)]
  [(rct v:id ((~and low:expr (~not _)) cmp:lt _))
   #'(cmp low v)]
  [(rct v:id ((~and low:expr (~not _))
              cmp1:lt
              _
              cmp2
              (~and high:expr (~not _))))
   #'(and (cmp1 low v) (cmp2 v high))])

;; -------------------------------------------------------

;; TODO: test?, thm

