
(describe frame-rendering
          (== (str (make-frame a: 1)) "{a: 1}"))

(describe naked-symbols
          (== a: 'a:))

(describe frame-access
          (== (get-slot {a: 1 b: 2 c: 3} a:) 1)
          (== (get-slot {a: 1 b: 2 c: 3} b:) 2))

(describe frame-mutation
          (let ((f {a: 1 b: 2 c: 3}))
            (== (set-slot! f a: 5) 5)
            (== (get-slot f a:) 5)))

(describe frame-method
          (let ((f {a: 5
                    b: 2
                    foo: (lambda (x)
                           (+ x a))}))
            (== (send f foo: 1) 6))
          (let ((f {a: 5
                    b: 2
                    foo: (lambda (x)
                           (set! b (+ x a)))}))
            (== (send f foo: 1) 6)
            (== (get-slot f b:) 6)))

(describe prototypes
          (let* ((f {a: 2
                     b: 1})
                 (g {parent*: f
                     a: 3}))
            (== (get-slot g b:) 1)
            (== (get-slot g a:) 3)
            (set-slot! g a: 1)
            (== (get-slot g a:) 1)
            (set-slot! g b: 10)
            (== (get-slot g b:) 10))
          (let* ((adder {add: (lambda (x)
                                (+ x a))})
                 (incrementor {parent*: adder
                               a: 1}))
            (== (send incrementor add: 3) 4)))

(describe new-slots
          (let ((f {a: 1}))
            (== (set-slot! f b: 5) 5)
            (== (get-slot f b:) 5)))

(describe function-slot-use
          (let ((f {a: 5
                    b: 2
                    foo: (lambda (x)
                           (+ x a))
                    bar: (lambda ()
                           (foo b))}))
            (== (send f bar:) 7)
            (== (bar:> f) 7)))

(describe inherited-function-slot-use
          (let* ((f {a: 5
                     foo: (lambda (x)
                            (+ x a))})
                 (g {parent*: f
                     b: 2
                     bar: (lambda ()
                            (foo b))}))
            (== (send g bar:) 7)))

(describe multiple-parents
          (let* ((e {a: 5})
                 (f {b: 2})
                 (g {parent-e*: e
                     parent-f*: f
                     foo: (lambda (x)
                            (+ x a))
                     bar: (lambda ()
                            (foo b))}))
            (== (send g bar:) 7)
            (set-slot! g a: 10)
            (== (get-slot g a:) 10)
            (== (get-slot e a:) 5)))

(describe calling-super
          (let* ((f {foo: (lambda () 42)})
                 (g {parent*: f  foo: (lambda () (+ 1 (send-super foo:)))}))
            (== (send g foo:) 43)))

(describe calling-super-sugar
          (let ((f {foo: (lambda () 42)})
                (g {parent*: f  foo: (lambda () (+ 1 (foo:^)))}))
            (== (foo:> g) 43)))

(describe locals-override-slots
          (let* ((f {a: 42})
                 (g {parent*: f  foo: (lambda () (let ((a 10)) (+ 1 a)))}))
            (== (send g foo:) 11)))

(describe cloning
          (let* ((f {a: 1 b: 2})
                 (g (clone f)))
            (== f g)
            (set-slot! f a: 42)
            (== (get-slot f a:) 42)
            (== (get-slot g a:) 1)))

(describe has-slot
          (let ((f {a: 1 b: 2}))
            (== (has-slot? f a:) #t)
            (== (has-slot? f b:) #t)
            (== (has-slot? f c:) #f)))

(describe remove-slots
          (let* ((e {a: 5})
                 (f {b: 2})
                 (g {parent-e*: e
                     parent-f*: f
                     foo: 3
                     bar: 10}))
            (== (remove-slot! g foo:) #t)
            (== (has-slot? g foo:) #f)
            (== (remove-slot! g a:) #f)
            (== (has-slot? e a:) #t)
            (== (remove-slot! nil a:) #f)))

(describe shortcuts
          (let ((f {a: 1 b: 2}))
            (== (a:? f) #t)
            (== (c:? f) #f)
            (== (a: f) 1)
            (== (b: f) 2)
            (a:! f 42)
            (== (a: f) 42)))

(describe non-function-slots-dont-override-functions
          (let ((f {map: 42
                    foo: (lambda ()
                           (map (lambda (x)
                                  (+ x 1))
                                '(1 2 3)))}))
            (== (send f foo:) '(2 3 4))))

(describe function-slots-override-functions
          (let ((f {map: (lambda (x y) 42)
                    foo: (lambda ()
                           (map (lambda (x)
                                  (+ x 1))
                                '(1 2 3)))}))
            (== (send f foo:) 42)))

(describe keys_values
          (let* ((f {a: 1 b: 2 c: 3})
                 (ks (frame-keys f))
                 (vs (frame-values f)))
            (== (not (memq a: ks)) #f)
            (== (not (memq b: ks)) #f)
            (== (not (memq c: ks)) #f)
            (== (not (memq 1 vs)) #f)
            (== (not (memq 2 vs)) #f)
            (== (not (memq 3 vs)) #f)))
