golisp
======

GoLisp is a simple Lisp language and runtime implemented in Google’s
Go programming language. It’s main purpose is integration into a Go
application to provide runtime extension and scripting. The core of a
basic Lisp is provided, but with limited special forms and primitive
functions. More of these will be added as required without having to
modify the GoLisp core. Also, a REPL is provided and GoLisp can be
used as-is to enter and evaluate code.

It is heavily inspired and influenced by Scheme (MIT/GNU scheme in
particular). GoLisp 1.0 is much more in line with Scheme, but it is at
it's heart a language for our purposes internally. It's growth and
evolution reflect that.

GoLisp evolves along 4 axis:

* becoming more compatible with MIT/GNU Scheme (GoLisp v1.0 is a huge
  move forward in this regard) 
* supporting more capabilities of the underlying Go implementation
  language (e.g. recent work supporting concurrency and channels)
* our particular needs (e.g. byte array support)
* useful additions (e.g. frames)

If GoLisp has a form or function that is also in MIT/GNU scheme, we
strive to make it work the same way.

As much as possible the internals echo those in MIT/GNU Scheme. For
example, the lexical environment structure/handling.

So, while it is intentionally very much like MIT/GNU Scheme, it is not
constrained to conform the specification. That said, unless our
extensions are used, code will usually be indistinguishable.

A complete language reference and other material is available at
[http://techblog.steelseries.com/golisp](http://techblog.steelseries.com/golisp).

A sample application is also available at [https://github.com/SteelSeries/golisp-example-app](https://github.com/SteelSeries/golisp-example-app).

GoLisp has the following dependencies:

* [gopkg.in/fatih/set.v0](gopkg.in/fatih/set.v0)
* [github.com/SteelSeries/bufrr](github.com/SteelSeries/bufrr)
* [gopkg.in/check.v1](gopkg.in/check.v1)

To install GoLisp, use:

```bash
go get -u github.com/SteelSeries/golisp 
```
