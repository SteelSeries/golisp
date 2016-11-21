(define (not-empty x)
  (cond ((string? x)
         (> (string-length x) 0))
        (else
         (> (length x) 0))))

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


(define-record User user-name: user-id: email: active?:)

(define domain (gen/elements '("gmail.com" "hotmail.com" "computer.org")))
(define email-gen
  (gen/fmap (lambda (name-and-domain-name)
              (let ((name (car name-and-domain-name))
                    (domain-name (cadr name-and-domain-name)))
                (str name "@" domain-name)))
            (gen/tuple (gen/string gen/alphanum-char 10) domain)))


(define user-gen
  (gen/fmap (lambda (args) (new:> User args))
            (gen/tuple (gen/string gen/alphanum-char 10)
                       gen/uint
                       email-gen
                       gen/boolean)))

