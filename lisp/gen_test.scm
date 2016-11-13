;;; Generative testing for GoLisp
;;; Copyright 2016 Dave Astels
;;; Inspired by Clojure's test.generative


;;; ----------------------------------------------------------------------------
;;;  Some support collections of characters

(define _list-of-chars_ (string->list " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\^_`abcdefghijklmnopqrstuvwxyz{|}~"))

(define _ascii-alpha_ (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(define _symbol-chars_ (string->list "!$%&*+-/.:<=>?@\\^_|~abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))


;;; ----------------------------------------------------------------------------
;;; Distributions

(define (gen/uniform . bounds)
  "Uniform distribution from lo (inclusive) to hi (exclusive). Defaults to range of 32 bit positive integers."
  (if (nil? bounds)
      (lambda () (random))
      (let ((lo (car bounds))
            (hi (cadr bounds)))
        (lambda () (integer (floor (+ lo (* (random 1.0) (- hi lo)))))))))

(define (gen/geometric p)
  "Geometric distribution with mean 1/p."
  (integer (ceiling (/ (log (random 1.0))
                       (log (- 1.0 p))))))


;;; ----------------------------------------------------------------------------
;;; Support functions

(define (gen/call-through maybe-func)
  "Recursively call x until it doesn't return a function."
  (let recur ((f maybe-func))
    (if (function? f)
        (recur (f))
        f)))

(define (gen/repeatedly count f)
  "Make a list of count invocations of f."
  (let loop ((n count)
             (result '()))
    (if (zero? n)
        result
        (loop (-1+ n) (cons (gen/call-through f) result)))))

(define (gen/repeat count x)
  "make a list of count copies of f."
  (make-list count x))

(define (gen/reps sizer f)
  "Returns sizer repetitions of f (or (f) if f is a fn)."
  (let ((count (gen/call-through sizer)))
    (if (function? f)
        (gen/repeatedly count f)
        (gen/repeat count f))))

(define (gen/reductions f initial coll)
  "Returns a list of the intermediate values of the reduction (as per reduce) of coll by f, starting with initial."
  (map (lambda (l)
         (reduce-left f initial l))
       (map (lambda (x)
              (take x coll))
            (interval (length coll)))))

(define (gen/vals alist)
  (map cdr alist))

(define (gen/keys alist)
  (map car alist))

(define (gen/default-sizer)
  (gen/geometric 0.02))


;;; ----------------------------------------------------------------------------
;;; Generators

(define (gen/int)
  "Returns a int in the 32 bit int range."
  (* ((gen/uniform)) (if (gen/boolean) 1 -1)))

(define (gen/uint)
  "Returns a int in the 32 bit positive int range."
  ((gen/uniform)))

(define (gen/byte)
  "Returns an int in the byte range."
  (gen/uniform 0 255))

(define (gen/float)
  "Generate a float between 0 (inclusive) and 1 (exclusive)"
  (random 1.0))

(define (gen/boolean)
  "Returns a bool."
  (eqv? (random 2) 1))

(define (gen/elements coll)
  "Randomly select an element of coll with equal probability."
  (nth (gen/uniform 0 (length coll)) coll))

(define (gen/char)
  (gen/elements _list-of-chars_))

(define (gen/tuple . generators)
  "Generate a tuple with one element from each generator."
  (map (lambda (g) (g)) generators))

(define (gen/weighted m)
  "Given a map of generators and weights, return a value from one of the generators, selecting generator based on weights."
  (let* ((weights (gen/reductions + 0 (gen/vals m)))
         (total (gen/last weights))
         (choices (reverse (pairlis (gen/keys m) weights))))
    (let ((choice (gen/uniform 0 total)))
      (let loop ((weighted-choices choices))
        (let ((c (caar weighted-choices))
              (w (cdar weighted-choices)))
          (when w
            (if (< choice w)
                (gen/call-through c)
                (loop (cdr weighted-choices))))))))))))

(define (gen/one-of . specs)
  "Generates one of the specs passed in, with equal probability."
  (gen/weighted (pairlis specs (make-list (length specs) 1))))

(define (gen/list f . maybe-sizer)
  "Create a list with elements from f and sized from sizer."
  (if (nil? maybe-sizer)
      (gen/list f gen/default-sizer)
      (gen/reps (car maybe-sizer) f)))

(define (gen/vector . args)
  "Create a vector with elements from f and sized from sizer."
  (list->vector (apply gen/list args)))

(define (gen/alist fk fv . maybe-sizer)
  "Create an association list with keys from fk, vals from fv, and sized from sizer."
  (if (nil? maybe-sizer)
      (gen/alist fk fv gen/default-sizer)
      (pairlis (gen/reps (car maybe-sizer) fk)
               (gen/reps (car maybe-sizer) fv))))

(define (gen/bytearray f . maybe-sizer)
  (if (nil? maybe-sizer)
      (gen/bytearray f gen/default-sizer)
      (list->bytearray (gen/list f (car maybe-sizer)))))

(define (gen/string . args)
  "Create a string with chars from f and sized from sizer."
  (cond ((nil? args)
         (gen/string gen/char))
        ((nil? (cdr args))
         (gen/string (car args) gen/default-sizer))
        (else
         (list->string (gen/reps (cadr args) (car args))))))

(define (**gen/name**)
  (gen/elements _symbol-chars_))

(define (gen/symbol . maybe-sizer)
  "Create a symbol sized from sizer."
  (if (nil? maybe-sizer)
      (gen/symbol gen/default-sizer)
      (intern (list->string (reps (car maybe-sizer) **gen/name**)))))

(define (gen/slotname . maybe-sizer)
  "Create a slotname sized from sizer."
  (if (nil? maybe-sizer)
      (gen/slotname gen/default-sizer)
      (intern (str (list->string (gen/reps (car maybe-sizer) **gen/name**)) ":"))))

(define (gen/return x)
  (lambda ()
    x))

(define (gen/such-that f generator)
  (lambda ()
    (let loop ((val (gen/call-through generator)))
      (if (f val)
          val
          (loop (gen/call-through generator))))))

(define (gen/not-empty generator)
  (gen/such-that (lambda (x) (> (length x) 0))
                 generator))

(define (gen/fmap f generator)
  (lambda ()
    (f (gen/call-through generator)))))

(define (gen/sample generator . maybe-size)
  (if (nil? maybe-size)
      (gen/sample generator 10)
      (gen/list generator (car maybe-size))))

;;; ----------------------------------------------------------------------------
;;; Property support

(define-macro (prop/for-all variable generator . declaration)
  `(let* ((,variable (gen/call-through ,generator))
          (result (begin ,@declaration)))
     (list ,variable result))))

;;; ----------------------------------------------------------------------------
;;; Checking


(define (check/find-simplest-list values)
  (reduce (lambda (a b)
            (if (< (length a)
                   (length b))
                a
                b))
          ()
          values))

(define (check/find-simplest-integer values)
  (reduce (lambda (a b)
            (if (< a b)
                a
                b))
          0
          values))

(define (check/find-simplest-float values)
  (reduce (lambda (a b)
            (if (< a b)
                a
                b))
          0.0
          values))

(define (check/find-simplest-boolean values)
  (not (any not values)))

(define (check/find-simplest-string values)
  (reduce (lambda (a b)
            (if (string<? a b)
                a
                b))
          ""
          values))

(define (check/find-simplest-symbol values)
  (reduce (lambda (a b)
            (if (string<? (str a)
                          (str b))
                a
                b))
          ""
          values))

(define (check/find-simplest-frame values)
  (reduce (lambda (a b)
            (if (< (length (frame-keys a))
                   (length (frame-keys b)))
                a
                b))
          {}
          values))

(define (check/find-simplest-vector values)
  (reduce (lambda (a b)
            (if (< (length a)
                   (length b))
                a
                b))
          #()
          values))

(define (check/find-simplest values)
  (let* ((comparison-type (type-of (car values)))
         (check-function-name (str "check/find-simplest-" comparison-type))
         (f (eval (intern check-function-name)))
         )
    (if (function? f)
        (f values)
        (error (format #f "Unsupported comparison requested: ~A" comparison-type)))))


(define (check/run count prop)
  (let loop ((n count)
             (val '(#t #t))
             (errors '())
             (result #t))
    (if (< n 0)
        (if result
            {result: #t
             num-tests: count}
            {result: #f
             num-tests: count
;;             error-cases: errors
             number-of-errors: (length errors)
             simplest-error: (check/find-simplest errors)})
        (begin
;;          (format #t "~A~%" (car val))
          (loop (-1+ n)
                (gen/call-through prop)
                (if (cadr val)
                    errors
                    (cons (car val) errors))
                (and (cadr val) result))))))


