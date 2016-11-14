;;; Generative testing for GoLisp
;;; Copyright 2016 Dave Astels
;;; Inspired by Clojure's test.generative


;;; ----------------------------------------------------------------------------
;;;  Some support collections of characters

(define _list-of-chars_ (string->list " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\^_`abcdefghijklmnopqrstuvwxyz{|}~"))

(define _ascii-alpha_ (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(define _ascii-alphanum_ (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))

(define _symbol-chars_ (string->list "!$%&*+-/.:<=>?@\\^_|~abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))


;;; ----------------------------------------------------------------------------
;;; Distributions

(define (gen/uniform . bounds)
  "Uniform distribution from lo (inclusive) to hi (exclusive). Defaults to range of 32 bit positive integers."
  (if (nil? bounds)
      (lambda () (random))
      (let ((lo (car bounds))
            (hi (cadr bounds)))
        (format #t "uniform (~A, ~A)~%" lo hi)
        (lambda () (integer (floor (+ lo (* (random 1.0) (- hi lo)))))))))

(define (gen/geometric p)
  "Geometric distribution with mean 1/p."
  (lambda ()
    (integer (ceiling (/ (log (random 1.0))
                         (log (- 1.0 p)))))))

(define (**gen/default-sizer**)
  (gen/geometric 0.02))

(define **gen/scale** nil)

(define (gen/square x) (* x x))

(define (gen/scale-function s)
  (-> s log gen/square (* 2) ceiling integer))

(define (gen/get-default-sizer)
  (format #t "scale: ~A~%" **gen/scale**)
  (if (nil? **gen/scale**)
      **gen/default-sizer**
      (lambda () (gen/uniform **gen/scale**  (* 2 **gen/scale**)))))

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
  (let ((old-scale **gen/scale**)
        (vals (reverse (let loop ((n 1)
                                  (result '()))
                         (set! **gen/scale** (scale-function n))
                         (if (> n count)
                             result
                             (loop (1+ n) (cons (gen/call-through f) result)))))))
    (set! **gen/scale** old-scale)
    vals))

(define (gen/repeat count x)
  "make a list of count copies of f."
  (make-list count x))

(define (gen/reps sizer f)
  "Returns sizer repetitions of f (or (f) if f is a fn)."
  (let ((count (gen/call-through sizer)))
    (format #t "reps sizer: ~A~%" (if (function? sizer) (definition-of sizer) sizer))
    (format #t "reps count: ~A~%" count)
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

;;; ----------------------------------------------------------------------------
;;; Generators

(define (gen/int)
  "Returns a int between -scale and scale."
  (if (nil? **gen/scale**)
      (lambda () (gen/uniform -2305843009213693952 2305843009213693952))
      (lambda ()
        (let ((bound (* 10 **gen/scale**)))
          (gen/uniform (- bound) bound)))))

(define (gen/uint)
  "Returns a int between 0 and scale."
  (if (nil? **gen/scale**)
      (lambda () (gen/uniform 0 2305843009213693952))
      (lambda () (gen/uniform 0 (* 10 **gen/scale**)))))

(define (gen/byte)
  "Returns an int in the byte range. Useful enough that it's a separate generator."
  (gen/uniform 0 255))

(define (gen/float)
  "Generate a float between 0 (inclusive) and 1 (exclusive)"
  (lambda ()
    (random 1.0)))

(define (gen/boolean)
  "Returns a bool."
  (lambda ()
    (eqv? (random 2) 1)))

(define (gen/choose lo hi)
  "Generate a number in the range [lo...hi]"
  (gen/uniform lo (1+ hi)))

(define (gen/elements coll)
  "Randomly select an element of coll with equal probability."
  (lambda ()
    (nth ((gen/uniform 0 (length coll)))
         coll)))

(define (gen/char)
  (gen/elements _list-of-chars_))

(define (gen/alpha-char)
  (gen/elements _ascii-alpha_))

(define (gen/alphanum-char)
  (gen/elements _ascii-alphanum_))

(define (gen/tuple . generators)
  "Generate a tuple with one element from each generator."
  (lambda ()
    (map (lambda (g) (gen/call-through g)) generators)))

(define (gen/weighted m)
  "Given a map of weights and generators, return a value from one of the generators, selecting generator based on weights."
  (let* ((weights (gen/reductions + 0 (gen/keys m)))
         (total (last weights))
         (choices (reverse (pairlis weights (gen/vals m)))))
    (lambda ()
      (let ((choice ((gen/uniform 0 total))))
        (let loop ((weighted-choices choices))
          (let ((c (cdar weighted-choices))
                (w (caar weighted-choices)))
            (when w
                  (if (< choice w)
                      (gen/call-through (eval c))
                      (loop (cdr weighted-choices))))))))))

(define (gen/one-of . specs)
  "Generates one of the specs passed in, with equal probability."
  (gen/weighted (pairlis specs (make-list (length specs) 1))))

(define (gen/list f . maybe-sizer)
  "Create a list with elements from f and sized from sizer."
  (if (nil? maybe-sizer)
      (gen/list f (lambda () (gen/get-default-sizer)))
      (let ((sizer (car maybe-sizer)))
        (format #t "gen/list sizer: ~A~%" (if (function? sizer) (definition-of sizer) sizer))

        (lambda ()
          (gen/reps sizer f))))))

(define (gen/vector . args)
  "Create a vector with elements from f and sized from sizer."
  (let ((list-generator (apply gen/list args)))
    (lambda ()
      (list->vector (list-generator)))))

(define (gen/alist fk fv . maybe-sizer)
  "Create an association list with keys from fk, vals from fv, and sized from sizer."
  (if (nil? maybe-sizer)
      (gen/alist fk fv (lambda () (gen/get-default-sizer)))
      (lambda ()
        (let ((count (gen/call-through (car maybe-sizer))))
          (pairlis (gen/reps count fk)
                   (gen/reps count fv))))))

(define (gen/bytearray . maybe-sizer)
  (if (nil? maybe-sizer)
      (gen/bytearray  (lambda () (gen/get-default-sizer)))
      (lambda ()
        (list->bytearray ((gen/list gen/byte (car maybe-sizer)))))))

(define (gen/string . args)
  "Create a string with chars from f and sized from sizer."
  (cond ((nil? args)
         (gen/string gen/char))
        ((nil? (cdr args))
         (gen/string (car args) (gen/get-default-sizer)))
        (else
         (list->string (gen/reps (cadr args) (car args))))))

(define (gen/**name**)
  (gen/elements _symbol-chars_))

(define (gen/symbol . maybe-sizer)
  "Create a symbol sized from sizer."
  (if (nil? maybe-sizer)
      (gen/symbol (lambda () (gen/get-default-sizer)))
      (lambda ()
        (let ((sym-name (cons (gen/call-through (gen/elements _ascii-alpha_))
                              (gen/reps (car maybe-sizer) gen/**name**))))
          (intern (list->string sym-name))))))

(define (gen/slotname . maybe-sizer)
  "Create a slotname sized from sizer."
  (if (nil? maybe-sizer)
      (gen/slotname (lambda () (gen/get-default-sizer)))
      (lambda ()
        (let ((sym-name (cons (gen/call-through (gen/elements _ascii-alpha_))
                              (gen/reps (car maybe-sizer) gen/**name**))))
          (intern (str (list->string sym-name) ":"))))))

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
    (f (gen/call-through generator))))

(define (gen/bind generator function)
  (lambda ()
    (let ((val (gen/call-through generator)))
      (function val))))

(define (gen/sample generator . maybe-size)
  (if (nil? maybe-size)
      (gen/sample generator 10)
      (let ((size (car maybe-size)))
        (set! **gen/scale** size)
        (let ((values ((gen/list generator size))))
          (set! **gen/scale** nil)
          values))))

;;; ----------------------------------------------------------------------------
;;; Property support

(define (prop/apply-gen function)
  (lambda (args)
    (let ((result (apply function args)))
      {result: result function: function args: args})))

(define (prop/for-all* args function)
  (lambda ()
    ((prop/apply-gen function) (gen/call-through (apply gen/tuple (if (vector? args)
                                                                      (map eval (vector->list args))
                                                                      args))))))

(define (prop/binding-vars bindings)
  (map first (partition 2 bindings)))

(define (prop/binding-gens bindings)
  (map second (partition 2 bindings)))

(define-macro (prop/for-all bindings . body)
  `(prop/for-all* ,(list->vector (prop/binding-gens bindings))
             (lambda (,@(prop/binding-vars bindings))
               ,@body)))


;;; ----------------------------------------------------------------------------
;;; Checking

(define type-ranks '((boolean . 1)
                    (integer . 2)
                    (float . 3)
                    (character . 4)
                    (string . 5)
                    (symbol . 6)
                    (list . 7)
                    (vector . 8)
                    (frame . 9)))

(define (check/compare-element a b)
  (cond ((neqv? (type-of a) (type-of b))
         (let ((ranka (cdr (assoc ta type-ranks)))
               (rankb (cdr (assoc tb type-ranks))))
           (cond ((< ranka rankb) -1)
                 ((> ranka rankb) 1)
                 (else 0))))
        ((number? a)
         (let ((absx (abs x))
               (absy (abs y)))
           (cond ((< absx absy) -1)
                 ((> absx absy) 1)
                 (else 0))))
        ((or (string? a) (symbol? a))
         (let ((stra (str a))
               (strb (str b)))
           (cond ((string<? stra strb) -1)
                 ((string>? stra strb) 1)
                 (else 0))))
        ((boolean? a)
         (cond ((and (not a) b) -1)
               ((and a (not b)) 1)
               0))
        ((or (list? a) (vector? a))
         (let ((lena (length a))
               (lenb (length b)))
           (cond ((< lena lenb) -1)
                 ((> lena lenb) 1)
                 (else 0))))
        ((frame? a)
         (let ((lena (length a))
               (lenb (length b)))
           (cond ((< lena lenb) -1)
                 ((> lena lenb) 1)
                 else 0)))))

(define (check/one-with-lesser-values a b)
  (cond ((nil? a)
         a)
        (else
         (if (negative? (reduce + 0 (map check/compare-element a b)))
             a
             b))
        ))

(define (check/find-simplest-list lists)
  (reduce (lambda (a b)
            (cond ((< (length a) (length b))
                   a)
                  ((> (length a) (length b))
                   b)
                  (else                 ; (eqv? (length a) (length b))
                   (check/one-with-lesser-values a b))))
          ()
          lists))

(define (check/find-simplest-vector vectors)
  (reduce (lambda (a b)
            (cond ((< (length a) (length b))
                   a)
                  ((> (length a) (length b))
                   b)
                  (else                 ; (eqv? (length a) (length b))
                   (check/one-with-lesser-values a b))))
          #()
          vectors))

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
            (if (< (length a)
                   (length b))
                a
                b))
          {}
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
             (val {result: #t})
             (errors '())
             (result #t))
;    (format #t "~A~%" val)
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
          (loop (-1+ n)
                (gen/call-through prop)
                (if (result: val)
                    errors
                    (cons (args: val) errors))
                (and (result: val) result))))))


