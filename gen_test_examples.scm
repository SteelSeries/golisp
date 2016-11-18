(define (ascending? coll)
  (every (lambda (pair) (<= (car pair) (cadr pair)))
          (partition 2 1 coll)))

(define property
  (prop/for-all (v (gen/list gen/int 10))
    (let ((s (sort v <)))
      (and (= (length v) (length s))
           (ascending? s)))))

(define bad-property
  (prop/for-all (v (gen/list gen/int 5))
                (ascending? v)))
