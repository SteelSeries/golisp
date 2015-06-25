;;; -*- mode: Scheme -*-


(describe frame-rendering
          (assert-eq (str (make-frame a: 1))
                     "{a: 1}"))

(describe naked-symbols
          (assert-eq a:
                     'a:))

(describe frame-access
          (assert-eq (get-slot {a: 1 b: 2 c: 3} a:)
                     1)
          (assert-eq (get-slot {a: 1 b: 2 c: 3} b:)
                     2))

(describe frame-mutation
          (let ((f {a: 1 b: 2 c: 3}))
            (assert-eq (set-slot! f a: 5)
                       5)
            (assert-eq (get-slot f a:)
                       5)))

(describe frame-method
          (let ((f {a: 5
                       b: 2
                       foo: (lambda (x)
                              (+ x a))}))
            (assert-eq (send f foo: 1)
                       6))
          (let ((f {a: 5
                       b: 2
                       foo: (lambda (x)
                              (set! b (+ x a)))}))
            (assert-eq (send f foo: 1)
                       6)
            (assert-eq (get-slot f b:)
                       6)))

(describe prototypes
          (let* ((f {a: 2
                        b: 1})
                 (g {parent*: f
                              a: 3}))
            (assert-eq (get-slot g b:)
                       1)
            (assert-eq (get-slot g a:)
                       3)
            (set-slot! g a: 1)
            (assert-eq (get-slot g a:)
                       1)
            (set-slot! g b: 10)
            (assert-eq (get-slot g b:)
                       10))
          (let* ((adder {add: (lambda (x)
                                (+ x a))})
                 (incrementor {parent*: adder
                                        a: 1}))
            (assert-eq (send incrementor add: 3)
                       4)))

(describe new-slots
          (let ((f {a: 1}))
            (assert-eq (set-slot! f b: 5)
                       5)
            (assert-eq (get-slot f b:)
                       5)))

(describe function-slot-use
          (let ((f {a: 5
                       b: 2
                       foo: (lambda (x)
                              (+ x a))
                       bar: (lambda ()
                              (foo b))}))
            (assert-eq (send f bar:)
                       7)
            (assert-eq (bar:> f)
                       7)))

(describe inherited-function-slot-use
          (let* ((f {a: 5
                        foo: (lambda (x)
                               (+ x a))})
                 (g {parent*: f
                              b: 2
                              bar: (lambda ()
                                     (foo b))}))
            (assert-eq (send g bar:)
                       7)))

(describe multiple-parents
          (let* ((e {a: 5})
                 (f {b: 2})
                 (g {parent-e*: e
                                parent-f*: f
                                foo: (lambda (x)
                                       (+ x a))
                                bar: (lambda ()
                                       (foo b))}))
            (assert-eq (send g bar:)
                       7)
            (set-slot! g a: 10)
            (assert-eq (get-slot g a:)
                       10)
            (assert-eq (get-slot e a:)
                       5)))

(describe calling-super
          (let* ((f {foo: (lambda () 42)})
                 (g {parent*: f  foo: (lambda () (+ 1 (send-super foo:)))}))
            (assert-eq (send g foo:)
                       43)))

(describe calling-super-sugar
          (let* ((f {foo: (lambda () 42)})
                 (g {parent*: f  foo: (lambda () (+ 1 (foo:^)))}))
            (assert-eq (foo:> g)
                       43)))

(describe locals-override-slots
          (let* ((f {a: 42})
                 (g {parent*: f  foo: (lambda () (let ((a 10)) (+ 1 a)))}))
            (assert-eq (send g foo:)
                       11)))

(describe cloning
          (let* ((f {a: 1 b: 2})
                 (g (clone f)))
            (assert-eq f
                       g)
            (set-slot! f a: 42)
            (assert-eq (get-slot f a:)
                       42)
            (assert-eq (get-slot g a:)
                       1)))

(describe has-slot
          (let ((f {a: 1 b: 2}))
            (assert-true (has-slot? f a:))
            (assert-true (has-slot? f b:))
            (assert-false (has-slot? f c:))))

(describe remove-slots
          (let* ((e {a: 5})
                 (f {b: 2})
                 (g {parent-e*: e
                                parent-f*: f
                                foo: 3
                                bar: 10}))
            (assert-true (remove-slot! g foo:))
            (assert-false (has-slot? g foo:))
            (assert-false (remove-slot! g a:))
            (assert-true (has-slot? e a:))
            (assert-false (remove-slot! nil a:))))

(describe shortcuts
          (let ((f {a: 1 b: 2}))
            (assert-true (a:? f))
            (assert-false (c:? f))
            (assert-eq (a: f)
                       1)
            (assert-eq (b: f)
                       2)
            (a:! f 42)
            (assert-eq (a: f)
                       42)))

(describe non-function-slots-dont-override-functions
          (let ((f {map: 42
                         foo: (lambda ()
                                (map (lambda (x)
                                       (+ x 1))
                                     '(1 2 3)))}))
            (assert-eq (send f foo:)
                       '(2 3 4))))

(describe function-slots-override-functions
          (let ((f {map: (lambda (x y) 42)
                         foo: (lambda ()
                                (map (lambda (x)
                                       (+ x 1))
                                     '(1 2 3)))}))
            (assert-eq (send f foo:)
                       42)))

(describe keys_values
          (let* ((f {a: 1 b: 2 c: 3})
                 (ks (frame-keys f))
                 (vs (frame-values f)))
            (assert-false (not (memq a: ks)))
            (assert-false (not (memq b: ks)))
            (assert-false (not (memq c: ks)))
            (assert-false (not (memq 1 vs)))
            (assert-false (not (memq 2 vs)))
            (assert-false (not (memq 3 vs)))))
