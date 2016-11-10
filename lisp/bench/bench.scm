
;;; ================================================================================
;;; Symbolic derivation


(define (deriv a)
  (cond ((not (pair? a))
         (if (eq? a 'x) 1 0))
        ((eq? (car a) '+)
         (cons '+
               (map deriv (cdr a))))
        ((eq? (car a) '-)
         (cons '-
               (map deriv (cdr a))))
        ((eq? (car a) '*)
         (list '*
                a
                (cons '+
                      (map (lambda (a) (list '/ (deriv a) a)) (cdr a)))))
        ((eq? (car a) '/)
         (list '-
               (list '/
                     (deriv (cadr a))
                     (caddr a))
               (list '/
                     (cadr a)
                     (list '*
                           (caddr a)
                           (caddr a)
                           (deriv (caddr a))))))
        (else
         (fatal-error "No derivation method available"))))

;;; ================================================================================
;;; Ackerman

(define (ack m n)
  (cond ((== m 0) (+ n 1))
        ((== n 0) (ack (- m 1) 1))
        (else (ack (- m 1) (ack m (- n 1))))))


;;; ================================================================================
;;; DESTRUC -- Destructive operation benchmark.

(define (append-to-tail! x y)
  (if (null? x)
    y
    (let loop ((a x) (b (cdr x)))
      (if (null? b)
        (begin
          (set-cdr! a y)
          x)
        (loop b (cdr b))))))

(define (destructive n m)
  (let ((l (do ((i 10 (- i 1)) (a '() (cons '() a)))
               ((== i 0) a))))
    (do ((i n (- i 1)))
        ((== i 0) l)
      (cond ((null? (car l))
             (do ((l l (cdr l)))
                 ((null? l))
               (if (null? (car l)) (set-car! l (cons '() '())))
               (append-to-tail! (car l)
                                (do ((j m (- j 1)) (a '() (cons '() a)))
                                  ((== j 0) a)))))
            (else
             (do ((l1 l (cdr l1)) (l2 (cdr l) (cdr l2)))
                 ((null? l2))
               (set-cdr! (do ((j (quotient (length (car l2)) 2) (- j 1))
                              (a (car l2) (cdr a)))
                             ((zero? j) a)
                           (set-car! a i))
                         (let ((n (quotient (length (car l1)) 2)))
                           (cond ((== n 0)
                                  (set-car! l1 '())
                                  (car l1))
                                 (else
                                  (do ((j n (- j 1)) (a (car l1) (cdr a)))
                                      ((== j 1)
                                       (let ((x (cdr a)))
                                         (set-cdr! a '())
                                         x))
                                    (set-car! a i))))))))))))


;;; ================================================================================
;;; DIVITER -- Benchmark which divides by 2 using lists of n ()'s.

(define (create-n n)
  (do ((n n (- n 1))
       (a '() (cons '() a)))
      ((eq? n 0) a)))
 
(define *ll* (create-n 2000))

(define (iterative-div2 l)
  (do ((l l (cddr l))
       (a '() (cons (car l) a)))
      ((null? l) a)))

;;; ================================================================================
;;; FIB -- A classic benchmark, computes fib(35) inefficiently.

(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))


;;; ================================================================================
;;; One of the Kernighan and Van Wyk benchmarks.

(define (create-x n)
  (define result (make-vector n))
  (do ((i 0 (+ i 1)))
      ((>= i n) result)
    (vector-set! result i i)))

(define (create-y x)
  (let* ((n (vector-length x))
         (result (make-vector n)))
    (do ((i (- n 1) (- i 1)))
        ((< i 0) result)
      (vector-set! result i (vector-ref x i)))))

(define (my-try n)
  (vector-length (create-y (create-x n))))

(define (go-array n)
  (let loop ((repeat 100)
             (result '()))
    (if (> repeat 0)
        (loop (- repeat 1) (my-try n))
        result)))


;;; ================================================================================
;;; GRAPHS -- Obtained from Andrew Wright.

(load "lisp/bench/graphs_bench.lsp")

;;; ================================================================================
;;; Running benchmarks

(define (display string)
    (format #t "~A" string))

(define (run-bench name count run)
    (let loop ((i count)
               (result '(undefined)))
      (if (< 0 i)
          (loop (- i 1) (run))
          result)))

(define (run-benchmark name count run-maker . args)
  (format #t "~A~%" name)
  (let* ((loop-count 10)
         (run (apply run-maker args))
         (results (map (lambda (ignored)
                         (time (run-bench name count run)))
                       (interval loop-count))))
     (list name count loop-count (min results) (max results) (/ (apply + results) loop-count))))

(define (fatal-error . args)
    (for-each (lambda (x) (format #t "~A " x)) args)
  (newline)
  (exit 1))

;;; ================================================================================
;;; Controlling

(define **DERIV-ITERS** 2000)
(define **ACK-ITERS** 1)
(define **DESTRUC-ITERS** 1)
(define **DIVITER-ITERS** 100)
(define **FIB-ITERS** 1)
(define **ARRAY1-ITERS** 1)

(define **BENCHMARKS**
    (list (lambda ()
            (run-benchmark "deriv"
                           **DERIV-ITERS**
                           (lambda (a) (lambda () (deriv a)))
                           '(+ (* 3 x x) (* a x x) (* b x) 5)))
          (lambda ()
            (run-benchmark "ackerman"
                           **ACK-ITERS**
                           (lambda (m n) (lambda () (ack m n)))
                           3
                           6))
          (lambda ()
            (run-benchmark "destruc"
                           **DESTRUC-ITERS**
                           (lambda (n m) (lambda () (destructive n m)))
                           600
                           50))
          (lambda ()
            (run-benchmark
             "diviter"
             **DIVITER-ITERS**
             (lambda (l) (lambda () (iterative-div2 l)))
             *ll*))
          (lambda ()
            (run-benchmark
             "fib"
             **FIB-ITERS**
             (lambda (n) (lambda () (fib n)))
             35))
          (lambda ()
            (run-benchmark
             "array1"
             **ARRAY1-ITERS**
             (lambda (n) (lambda () (go n)))
             20000))))


(define (run label)
  (let ((results (map (lambda (f)
                        (f))
                      **BENCHMARKS**))
        (name-width 20)
        (outer-width 5)
        (inner-width 5)
        (min-width 5)
        (max-width 5)
        (avg-width 5))
    (format #t "~%Benchmark for ~A~%~%" label)
    (format #t "~VA | ~VA | ~VA | ~V@A | ~V@A | ~VA~%" name-width "Name" outer-width "Outer" inner-width "Inner" min-width "Min" max-width "Max" avg-width "Avg")
    (format #t "~V~+~V~+~V~+~V~+~V~+~V~~%" (+ 1 name-width) (+ 2 outer-width) (+ 2 inner-width) (+ 2 min-width) (+ 2 max-width) (+ 1 avg-width))
    (for-each (lambda (d)
                (let ((name (first d))
                      (inner-count (second d))
                      (outer-count (third d))
                      (min (fourth d))
                      (max (fifth d))
                      (avg (sixth d)))
                  (format #t "~VA | ~VA | ~VA | ~VA | ~VA | ~VA~%" name-width name outer-width outer-count inner-width inner-count min-width min max-width max avg-width avg)))
              results)
    (format #t "~V~+~V~+~V~+~V~+~V~+~V~~%" (+ 1 name-width) (+ 2 outer-width) (+ 2 inner-width) (+ 2 min-width) (+ 2 max-width) (+ 1 avg-width))))
