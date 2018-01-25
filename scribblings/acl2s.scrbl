#lang scribble/manual

@(require scribble/example
          "../acl2s-printer.rkt"
          (for-label acl2s-scribblings/acl2s-placeholder))

@(define (make-ev)
   (define ev
     (parameterize ([current-print (acl2s-print (current-print))])
       (make-base-eval #:pretty-print? #false)))
   (ev '(require acl2s-scribblings/acl2s-placeholder))
   ev)

@(define-syntax-rule (ex #:eval ev-expr form ...)
   (examples #:eval ev-expr #:label #false form ...))

@title{ACL2s Reference}

@section{Beginner Mode}

@local-table-of-contents[]

@declare-exporting[acl2s-scribblings/acl2s-placeholder]

@subsection{Definitions and Checks}

@(define def/check-ev (make-ev))

@defform[#:literals [:input-contract :output-contract]
         (defunc fn-name (param-name ...)
           :input-contract in-ctc-expr
           :output-contract out-ctc-expr
           body-expr)]{
  Defines the function @racket[fn-name].

  @ex[#:eval def/check-ev
    (code:comment "sum-n : integer -> integer")
    (code:comment "Given integer n, return 0 + 1 + 2 + ... + n")
    (defunc sum-n (n)
      :input-contract (natp n)
      :output-contract (natp (sum-n n))
      (cond ((equal n 0) 0)
            (t (code:comment "else")
             (+ n (sum-n (- n 1))))))
  ]
}

@defform[(check= actual-expr expected-expr)]{
  Checks that @racket[actual-expr] and @racket[expected-expr] are
  equal.

  @ex[#:eval def/check-ev
    (check= (sum-n 5) (+ 1 2 3 4 5))
    (check= (sum-n 0) 0)
    (check= (sum-n 3) 6)
    (check= (sum-n 4) -32)
    (check= (sum-n 4) 10)
    (check= (sum-n 100) 5050)
  ]
}

@subsection{Booleans and Conditionals}

@defidform[#:kind "type" boolean]

@defthing[t boolean]{
  The boolean representing true.

  @ex[#:eval (make-ev)
    (if t 1 2)
    (cond (t "a") (t "b"))
  ]

  @bold{Warning}: @racket[t] happens to be both a boolean and a
  symbol, so be careful when mixing symbols with booleans.

  @ex[#:eval (make-ev)
    (booleanp t)
    (symbolp t)
  ]
}

@defthing[nil boolean]{
  The boolean representing false.

  @ex[#:eval (make-ev)
    (if nil 1 2)
    (cond (nil "a") (t "b"))
  ]

  @bold{Warning}: @racket[nil] happens to be a boolean, a list,
  and a symbol, so be careful when mixing lists with booleans,
  symbols with booleans, or lists with symbols.

  @ex[#:eval (make-ev)
    (booleanp nil)
    (symbolp nil)
    (listp nil)
  ]
}

@defproc[(booleanp [v all]) boolean]{
  Produces @racket[t] when @racket[v] is a boolean
  (@racket[t] or @racket[nil]), produces @racket[nil] if it's
  anything else.

  @ex[#:eval (make-ev)
    (booleanp t)
    (booleanp nil)
    (booleanp 5)
    (booleanp "watermelon")
  ]
}

@defform[(if question-expr then-expr else-expr)]{
  When @racket[question-expr] is @racket[t], @racket[if]
  evaluates and returns @racket[then-expr]. When
  @racket[question-expr] is @racket[nil], it evaluates and
  returns @racket[else-expr].

  @ex[#:eval (make-ev)
    (if t 1 2)
    (if nil 1 2)
  ]

  It evaluates the two branches @italic{lazily}, meaning it
  doesn't evaluate @racket[then-expr] unless the question really
  is @racket[t], and it doesn't evaluate @racket[else-expr]
  unless the question really is @racket[nil]. So this
  division-by-zero expression never gets evaluated.

  @ex[#:eval (make-ev)
    (if nil (/ 1 0) "the-other-thing")
    (if t "the-first-thing" (/ 1 0))
  ]

  This makes @racket[if] (and similar forms like @racket[cond])
  useful for guarding function calls against bad data.

  @ex[#:eval (make-ev)
    (defunc try-/ (x y)
      :input-contract (and (rationalp x) (rationalp y))
      :output-contract t
      (if (not (equal y 0))
          (code:comment "this division is only evaluated when y isn't 0")
          (/ x y)
          nil))
    (try-/ 2 6)
    (try-/ 2 0)
  ]

  This delay in evaluating the branches is also important for
  functions that use recursion, so it doesn't evaluate the
  recursive call when it should be on the base case.
}

@defform[#:literals [t]
         (cond (question-expr answer-expr) ... (t else-expr))]{
  Starts evaluating each @racket[question-expr] in order. If a
  question produces @racket[t], @racket[cond] evaluates and
  returns the @racket[answer-expr] associated with it. If a
  question produces @racket[nil], it moves on to the next
  question. If all of the questions are @racket[nil], it
  evaluates and returns @racket[else-expr].

  @ex[#:eval (make-ev)
    (cond (nil 1)
          (t 2)
          (t 3))
    (cond (nil 1)
          (nil 2)
          (t 3))
  ]

  Like @racket[if], @racket[cond] evaluates its answer
  expressions lazily, which means it can be used to guard against
  bad data, or to guard recursive calls.

  @ex[#:eval (make-ev)
    (cond (nil (/ 1 0)) (t "the-other-thing"))
    (cond (t "the-first-thing") (t (/ 1 0)))
    (defunc try-/ (x y)
      :input-contract (and (rationalp x) (rationalp y))
      :output-contract t
      (cond ((not (equal y 0))
             (code:comment "this division is only evaluated when y isn't 0")
             (/ x y))
            (t (code:comment "else")
             nil)))
    (try-/ 3 6)
    (try-/ 3 0)
  ]
}

@defform[(and conjunct-expr ...)]{
  Produces @racket[t] if all the @racket[conjunct-expr]s are
   @racket[t].

  @ex[#:eval (make-ev)
    (and t t)
    (and t nil)
    (and nil t)
    (and nil nil)
    (and t t t t t)
    (and t t t nil t)
  ]

  Like @racket[if] and @racket[cond], @racket[and] evaluates its
  arguments lazily, only when all of the previous arguments have
  praduced @racket[t]. So this division by zero expression never
  gets evaluated:

  @ex[#:eval (make-ev)
    (and nil (/ 1 0))
  ]
}

@defform[(or disjunct-expr ...)]{
  Produces @racket[t] if at least one of the
  @racket[conjunct-expr]s is @racket[t].

  @ex[#:eval (make-ev)
    (or t t)
    (or t nil)
    (or nil t)
    (or nil nil)
    (or nil nil nil nil nil)
    (or nil nil nil t nil)
  ]

  Like @racket[if] and @racket[cond], @racket[or] evaluates its
  arguments lazily, only when none of the previous arguments have
  praduced @racket[t]. So this division by zero expression never
  gets evaluated:

  @ex[#:eval (make-ev)
    (or t (/ 1 0))
  ]
}

@defproc[(not [b boolean]) boolean]{
  Produces @racket[nil] if @racket[b] is @racket[t], and
  @racket[t] if @racket[b] is @racket[nil].

  @ex[#:eval (make-ev)
    (not t)
    (not nil)
  ]
}

@subsection{Equality}

@defproc[(equal [a all] [b all]) boolean]{
  Produces @racket[t] if @racket[a] and @racket[b] are equal,
  @racket[nil] otherwise.

  @ex[#:eval (make-ev)
    (equal 8 8)
    (equal 8 9)
    (equal "eggplant" "eggplant")
    (equal "eggplant" 'eggplant)
    (equal (list 1 2 3) (list 1 2 3))
    (equal (list 1 2 3) (list 1 2 4))
    (equal (list "ice") (list "ice"))
    (equal (list "ice") "ice")
  ]
}

@subsection{Numbers}

@deftogether[[
  @defidform[#:kind "type" rational]
  @defidform[#:kind "type" integer]
  @defidform[#:kind "type" nat]
  @defidform[#:kind "type" pos]
]]

@defproc[(rationalp [v all]) boolean]{
  A recognizer for rational numbers.

  @ex[#:eval (make-ev)
    (rationalp 5/12)
    (rationalp 6)
    (rationalp -8)
  ]
}

@defproc[(integerp [v all]) boolean]{
  A recognizer for integers.

  @ex[#:eval (make-ev)
    (integerp 5/12)
    (integerp 42)
    (integerp -9)
  ]
}

@defproc[(natp [v all]) boolean]{
  A recognizer for natural numbers. The natural numbers are the
  integers starting from zero and counting up.

  @ex[#:eval (make-ev)
    (natp 0)
    (natp 1)
    (natp 14)
    (natp -2)
    (natp 5/3)
    (natp 1138)
  ]
}

@defproc[(posp [v all]) boolean]{
  A recognizer for positive integers. These are the integers
  starting from one and counting up.

  @ex[#:eval (make-ev)
    (posp 0)
    (posp 1)
    (posp 2)
    (posp 2187)
    (posp -47)
    (posp 13/31)
  ]
}

@deftogether[[
  @defproc[(< [a rational] [b rational]) boolean]
  @defproc[(> [a rational] [b rational]) boolean]
  @defproc[(<= [a rational] [b rational]) boolean]
  @defproc[(>= [a rational] [b rational]) boolean]
]]{
  Numeric comparison functions.

  @ex[#:eval (make-ev)
    (< 4 78)
    (< 99 2)
    (< 8 8)
    (<= 3 4)
    (<= 3 3)
    (<= 3 2)
    (> 8 -3)
    (> -8 3)
    (>= 6 6)
    (>= 7 6)
  ]
}

@deftogether[[
  @defproc[(+ [a rational] [b rational]) rational]
  @defproc[(- [a rational] [b rational]) rational]
  @defproc[(* [a rational] [b rational]) rational]
  @defproc[(/ [a rational] [b rational/non-zero]) rational]
  @defproc[(unary-- [a rational]) rational]
  @defproc[(unary-/ [a rational/non-zero]) rational]
]]{
  Basic arithmetic operations.

  @ex[#:eval (make-ev)
    (+ 2 2)
    (+ 1 (+ 2 3))
    (+ 6 1)
    (* 6 7)
    (* 9 3)
    (* 45/77 11)
    (- 15 8)
    (- 8 15)
    (/ 42 7)
    (/ 47 2)
    (/ 46 6)
    (unary-- 98)
    (unary-- -32)
    (unary-/ 54)
    (unary-/ 1/29)
  ]
}

@subsection{Lists and Pairs}

@defform[#:kind "type" (listof X)]

@defthing[#:link-target? #f
          nil (listof X)]{
  The empty list.

  @bold{Warning}: @racket[nil] happens to be an empty list, a
  boolean, and a symbol all at once, so be careful when mixing
  these types together.

  @ex[#:eval (make-ev)
    (listp nil)
    (booleanp nil)
    (symbolp nil)
  ]
}

@defproc*[([(cons [x X] [y (listof X)]) (listof X)]
           [(cons [x X] [y Y]) (cons X Y)])]{
  Constructs a pair. The @racket[x] argument is the
  @racket[first], and the @racket[y] argument is the
  @racket[rest].

  If you want to make a list, then @racket[y] should be a list.

  @ex[#:eval (make-ev)
    (cons 5 nil)
    (cons 2 (cons 4 (cons 8 nil)))
    (cons 3 (list 5 7 11 13))
  ]

  A @racket[cons] is not always a list. It's only a list when
  the rest is also a list.

  @ex[#:eval (make-ev)
    (listp (cons 1 (cons 2 (cons 3 (cons 4 nil)))))
    (code:comment "please never do this:")
    (listp (cons 1 (cons 2 (cons 3 4))))
  ]
}

@defproc[(endp [l (listof X)]) boolean]{
  Produces @racket[t] if the list @racket[l] is empty,
  @racket[nil] otherwise.

  @ex[#:eval (make-ev)
    (endp nil)
    (endp (cons 1 (cons 2 (cons 3 nil))))
  ]
}

@defproc[(consp [v all]) boolean]{
  Produces @racket[t] if the @racket[v] is a cons pair,
  @racket[nil] otherwise.

  @ex[#:eval (make-ev)
    (consp nil)
    (consp (cons 1 (cons 2 (cons 3 nil))))
    (consp "banana")
  ]
}

@defproc*[([(first [lst (listof X)]) X]
           [(first [pair (cons X Y)]) X])]{
  Gets the first element of a list or pair.

  @ex[#:eval (make-ev)
    (first (cons 5 nil))
    (first (cons 1 (cons 2 (cons 3 nil))))
    (first (list "apple" "banana" "cherry"))
  ]
}

@defproc*[([(rest [lst (listof X)]) (listof X)]
           [(rest [pair (cons X Y)]) Y])]{
  Gets the rest of a list or pair.

  @ex[#:eval (make-ev)
    (rest (cons 5 nil))
    (rest (cons 1 (cons 2 (cons 3 nil))))
    (rest (list "apple" "banana" "cherry"))
  ]
}

@defproc[(listp [v all]) boolean]{
  Produces @racket[t] if @racket[v] is a list, @racket[nil]
  otherwise. A list is either @racket[nil] or a @racket[cons]
  pair with a list in the rest position.

  @ex[#:eval (make-ev)
    (listp nil)
    (listp (cons 1 (cons 2 (cons 3 nil))))
    (listp "lime")
    (code:comment "if it doesn't end with nil, it's not a list:")
    (listp (cons 1 (cons 2 3)))
  ]
}

@defproc[(list [x X] ...) (listof X)]{
  Produces a list with all the @racket[x]s.

  @ex[#:eval (make-ev)
    (list)
    (list 1 2 3)
    (list "red" "orange" "yellow" "green" "blue" "purple")
  ]
}

@defproc[(len [l (listof X)]) nat]{
  Produces the length of the list.

  @ex[#:eval (make-ev)
    (len nil)
    (len (list 1 2 3))
    (len (list 'a 'b 'c 'd 'e 'f 'g))
  ]
}

@defproc[(app [a (listof X)] [b (listof X)]) (listof X)]{
  Appends the lists @racket[a] and @racket[b] together into a new
  list with the elements of @racket[a] first and the elements of
  @racket[b] after that.

  @ex[#:eval (make-ev)
    (app nil (list 3 5 8))
    (app (list 1 2 3) (list 4 5))
    (app (list 'a) (list 'b 'c))
    (app (app (list 'a) (list 'b 'c)) (list 'd 'e 'f 'g))
  ]
}

@defproc[(rev [l (listof X)]) (listof X)]{
  Reverses the list @racket[l].

  @ex[#:eval (make-ev)
    (rev nil)
    (rev (list 1 2 3))
    (rev (list 'a 'b 'c 'd 'e 'f 'g))
  ]
}

@defproc[(in [a all] [l (listof all)]) boolean]{
  Produces @racket[t] if the list @racket[l] contains the value
  @racket[a] somewhere in it (according to @racket[equal]).

  @ex[#:eval (make-ev)
    (in 2 (list 1 2 3))
    (in 2 (list 1 3 5))
    (in 'g (list 'c 'a 'b 'b 'a 'g 'e))
    (in 'g (list 'c 'a 'b 'b 'a 'j 'e))
  ]
}

@subsection{Symbols}

@defidform[#:kind "type" symbol]

@defproc[(symbolp [v all]) boolean]{
  Produces @racket[t] if @racket[v] is a symbol, @racket[nil]
  otherwise.

  @ex[#:eval (make-ev)
    (symbolp 'apple)
    (symbolp 'pear)
    (symbolp 'antigo)
    (symbolp 't)
    (symbolp 'nil)
    (symbolp 5)
    (symbolp "I'm a string")
    (symbolp (cons 'water (cons 'melon nil)))
    (symbolp 'watermelon)
  ]

  @bold{Warning}: @racket[t] and @racket[nil] happen to be both
  symbols and booleans, and @racket[nil] is a list as well. This
  means when you're using @racket[symbolp] on something that
  could be a boolean or a list, you might get @racket[t] even if
  you didn't @italic{intend} for it to be interpreted that way.
  Be careful when mixing symbols with booleans or lists.

  @ex[#:eval (make-ev)
    (symbolp nil)
    (booleanp nil)
    (listp nil)
    (symbolp t)
    (booleanp t)
  ]
}

@subsection{Strings}

@defidform[#:kind "type" string]

@defproc[(stringp [v all]) boolean]{
  Produces @racket[t] if @racket[v] is a string, @racket[nil]
  otherwise.

  @ex[#:eval (make-ev)
    (stringp "The silver fox bounded through the forest.")
    (stringp "watermelon")
    (stringp 'watermelon)
  ]
}

@subsection{Data Definitions}

@(define dd-ev (make-ev))
@(define-syntax-rule (ex/dd form ...)
   (ex #:eval dd-ev form ...))

@defform[#:literals [all
                     boolean
                     rational integer nat pos
                     symbol string
                     enum oneof
                     range
                     list cons listof
                     record
                     < <=]
         (defdata name type)
         #:grammar
         ([type
           singleton
           simple-type
           (enum lst-expr)
           (oneof type type)
           (range numeric-type range-constraints)
           (list type ...)
           (cons type type)
           (listof type)
           (record (field-name . type) ...)]
          [simple-type
           all boolean symbol string numeric-type]
          [numeric-type
           rational integer nat pos]
          [range-constraints
           (num-expr lt _ lt num-expr)
           (num-expr lt _)
           (_ lt num-expr)]
          [lt
           < <=])]{
  Defines the function @racket[namep] as a recognizer for the
  given @racket[type].

  Singletons:
  @ex/dd[
    (defdata one 1)
    (onep 1)
  ]

  List Types:
  @ex/dd[
    (defdata natlist (listof nat))
    (natlistp nil)
    (natlistp (list 2 3 5 8))
    (natlistp (list 2 3 -5 8))
  ]
}

@defidform[#:kind "type" all]

@defform[#:kind "type" (enum lst-expr)]{
  @ex/dd[
    (defdata traffic-light (enum (list 'red 'yellow 'green)))
    (traffic-lightp 'red)
    (traffic-lightp 'green)
    (traffic-lightp 'blue)
  ]
}

@defform[#:kind "type" (oneof type ...)]{
  @ex/dd[
    (defdata intstr (oneof integer string))
    (intstrp 5)
    (intstrp -2)
    (intstrp "oconto")
    (intstrp 'watermelon)
    (intstrp "watermelon")
    (defdata spiral (oneof nil (cons spiral spiral)))
    (spiralp nil)
    (spiralp (cons nil nil))
    (spiralp (cons (cons nil (cons nil nil)) nil))
    (spiralp (cons (cons (cons nil (cons nil nil))
                         (cons (cons nil nil) nil))
                   (cons nil
                         (cons (cons nil (cons nil nil)) nil))))
    (spiralp (cons (cons nil (cons "watermelon" nil)) nil))
  ]
}

@defform[#:kind "type" #:literals [< <=]
         (range numeric-type range-constraints)
         #:grammar
         ([numeric-type
           rational integer nat pos]
          [range-constraints
           (num-expr lt _ lt num-expr)
           (num-expr lt _)
           (_ lt num-expr)]
          [lt
           < <=])]{
  @ex/dd[
    (defdata probability (range rational (0 <= _ <= 1)))
    (probabilityp 1/2)
    (probabilityp 2)
    (probabilityp 0)
    (probabilityp 1)
    (defdata big-nat (range integer (24601 < _)))
    (big-natp 4)
    (big-natp 3827)
    (big-natp 13372462)
  ]
}

@defform[#:kind "type" (record (field-name . type) ...)]{
  @ex/dd[
    (defdata fullname
      (record (first . string) (last . string)))
    (define x (fullname "David" "Smith"))
    x
    (fullnamep x)
    (fullnamep "David")
    (fullname-first x)
    (fullname-last x)
    (equal x (fullname "David" "Smith"))
  ]
}

@subsection{The @racket[quote] form}

@defform*[[(quote name)
           (quote boolean)
           (quote number)
           (quote string)
           (quote (datum ...))]]{
  Quoting a name creates a symbol. Quoting a piece of literal
  data like a number, boolean, or string produces that same
  number, boolean, or string.

  It gets more complicitated when you use it with parentheses. It
  creates a list, but it also "distributes itself" over
  everything within it, including to every element of the list. So
  @racket[(@#,racket[quote] (datum ...))] is equivalent to
  @racket[(list (@#,racket[quote] datum) ...)].
}

@subsection{Miscellaneous}

@defproc[(atom [v all]) boolean]{
  Produces @racket[t] if @racket[v] is anything other than a
  cons. Produces @racket[nil] if @racket[v] is a cons.

  @ex[#:eval (make-ev)
    (atom 5)
    (atom "watermelon")
    (atom t)
    (atom nil)
    (atom (cons 5 "watermelon"))
    (atom (cons t nil))
  ]
}

