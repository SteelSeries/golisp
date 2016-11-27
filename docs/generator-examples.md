# Generator Examples

This file is ported directly from Clojure's test.check

The following examples assume you have the following file loaded (which is loaded at startup in the standard distribution):

```
lisp/gen_test.scm
```

For the most part, these are in order of simplest to most complex. They also
skip over some of the built-in, basic generators.

## A simple example

First, let's start with an example, suppose we want to test a sort function.
It's easy to come up with some trivial properties for our function, namely that
the output should be in ascending order. We also might want to make sure that
the count of the input is preserved. Our test might look like:

```
(define (ascending? coll)
  (every (lambda (pair) (<= (car pair) (cadr pair)))
          (partition 2 1 coll)))
    
(define property
  (prop/for-all (v (gen/list gen/int 10))
    (let ((s (sort v <)))
      (and (= (length v) (length s))
           (ascending? s)))))
    
;; test our property
(check/run 100 property)
==> {result: true, num-tests: 100}
```

What if we were to forget to actually sort our list? The test will fail, and
then check/run will find the simplest example that caused a failure.
<!-- will try and find 'smaller' inputs that still cause the test -->
<!-- to fail. For example, the function might originally fail with input: -->
<!-- `[5 4 2 2 2]`, but check/run will shrink this down to `[0 -1]` (or `[1 0]`). -->

## Generators

In order to write our property, we'll use generators. A generator knows how to
generate random values for a specific type. The `gen_test `framework
 has many built-in generators, as well as combinators for creating
your own new generators. You can write sophisticated generators just by
combining the existing generators with the given combinators. As we write
generators, we can see them in practice with the `sample` function:

```
(gen/sample gen/int)
==> (0 1 -1 0 -1 4 4 2 7 1)
```

we can ask for more samples:

```
(gen/sample gen/int 20)
==> (0 1 1 0 2 -4 0 5 -7 -8 4 5 3 11 -9 -4 6 -5 -3 0)
```

You may notice that as you ask for more values, the 'size' of the generated
values increases. As check/run generates more values, it increases the
'size' of the generated values. This allows tests to fail early, for simple
values, and only increase the size as the test continues to pass.

### Compound generators
how above we're providing a function that returns a `gen/tuple` generator? The
decision of which to use depends on whether you want to simply transform the
_value_ of a generator (sort it, multiply it by two, etc.), or create an
entirely new generator out of it.

