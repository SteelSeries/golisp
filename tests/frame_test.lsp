;;; -*- mode: Scheme -*-


(describe frame-rendering
          (assert-eq (str (make-frame a: 1))
                     "{a: 1}")
          (assert-error (make-frame a: 1 b:)) ;must have an even number of args
          (assert-error (make-frame a: 1 'a 2)) ;keys must be naked symbols
          (assert-error (make-frame a: 1 "b" 2)) ;keys must be naked symbols
          (assert-error (make-frame a: 1 3 2))) ;keys must be naked symbols

(describe naked-symbols
          (assert-eq a:
                     'a:))

(describe get-slot
          (assert-eq (get-slot {a: 1 b: 2 c: 3} a:)
                     1)
          (assert-eq (get-slot {a: 1 b: 2 c: 3} b:)
                     2)
          (assert-error (get-slot '() a:))
          (assert-error (get-slot nil a:))
          (assert-error (get-slot {} "a"))
          (assert-error (get-slot {} 'a))
          (assert-error (get-slot {a: 1} b:)))

(describe get-slot-or-nil
          (assert-eq (get-slot-or-nil {a: 1 b: 2 c: 3} a:)
                     1)
          (assert-eq (get-slot-or-nil {a: 1 b: 2 c: 3} b:)
                     2)
          (assert-nil (get-slot-or-nil {a: 1} b:))
          (assert-error (get-slot-or-nil '() a:))
          (assert-error (get-slot-or-nil nil a:))
          (assert-error (get-slot-or-nil {} "a"))
          (assert-error (get-slot-or-nil {} 'a)))

(describe set-slot!
          (let ((f {a: 1 b: 2 c: 3}))
            (assert-eq (set-slot! f a: 5)
                       5)
            (assert-eq (get-slot f a:)
                       5))
            (assert-error (set-slot! '() a: 1)) ;1st arg must be a frame
            (assert-error (set-slot! f "a" 1)))) ;2nd arg must be a naked symbol

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
                       6))

          (assert-error (send '(1 2) foo:)) ;1st arg must be a frame
          (assert-error (send {a: 1} 'a)) ;selector must be a naked symbol
          (assert-error (send {a: 1} 1)) ;selector must be a naked symbol
          (assert-error (send {a: 1} b:)) ;selector must be a key in the frame
          (assert-error (send {a: 1} a:))) ;slot value must be a function

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
                       43))
          (assert-error (send-super foo:)) ;only usable in a frame

          (let* ((f {foo: (lambda () 42)})
                 (g {parent*: f  foo: (lambda () (+ 1 (send-super 'foo)))}))
            (assert-error (send g foo:))) ;selector must be a naked symbol

          (let* ((f {foo: 42})
                 (g {parent*: f  foo: (lambda () (+ 1 (send-super 'foo)))}))
            (assert-error (send g foo:)))) ;parent's slot value must be a function

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

(describe has-slot?
          (let ((f {a: 1 b: 2}))
            (assert-true (has-slot? f a:))
            (assert-true (has-slot? f b:))
            (assert-false (has-slot? f c:))
            (assert-error (has-slot? '() a:)) ;1st arg must be a frame
            (assert-error (has-slot? f 'a)) ;2nd arg must be a naked symbol
            (assert-error (has-slot? f "a")))) ;2nd arg must be a naked symbol

(describe remove-slot!
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
            (assert-false (remove-slot! nil a:))
            (assert-error (remove-slot! '(1 2) a:)) ;1st arg must be a frame
            (assert-error (remove-slot! f 'a)) ;2nd arg must be a naked symbol
            (assert-error (remove-slot! f "a"))))) ;2nd arg must be a naked symbol

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
            (assert-false (not (memq 3 vs))))

          (assert-error (frame-keys '()))
          (assert-error (frame-keys ""))
          (assert-error (frame-keys 4))
          (assert-error (frame-values '()))
          (assert-error (frame-values ""))
          (assert-error (frame-values 4)))
