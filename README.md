golisp
======

GoLisp is a simple Lisp language and runtime implemented in Google’s Go
programming language. It’s original purpose is integration into a Go application
to provide runtime extension and scripting, but I've been using it as a primary
language on my personal projects. The core of a basic Lisp is provided, but with
limited special forms and primitive functions. More of these will be added as
required without having to modify the GoLisp core. Also, a REPL is provided and
GoLisp can be used as-is to enter and evaluate code.

It is heavily inspired and influenced by Scheme (MIT/GNU scheme in particular).

GoLisp evolves along 4 axis:

* becoming more compatible with MIT/GNU Scheme (GoLisp v1.0 was a huge
  move forward in this regard, and v1.1 takes it even further) 
* supporting more capabilities of the underlying Go implementation
  language (e.g. recent work supporting concurrency and channels)
* our particular needs (e.g. byte array support)
* useful additions (e.g. frames and type checking)

If GoLisp has a form or function that is also in MIT/GNU scheme, we
strive to make it work the same way.

As much as possible the internals echo those in MIT/GNU Scheme. For example, the
lexical environment structure/handling. So, while it is intentionally very much
like MIT/GNU Scheme, it is not constrained to conform the specification. The
language spec is in the docs directory (both tex source as well as pdf output).

GoLisp has the following dependencies:

go get gopkg.in/fatih/set.v0
go get github.com/SteelSeries/bufrr
go get gopkg.in/check.v1
