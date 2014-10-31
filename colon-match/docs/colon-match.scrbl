#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          racket/require
          (for-label colon-match
                     (subtract-in
                      racket
                      colon-match)))

@title{:match}

@defmodule[colon-match]

@defform[#:literals (=>)
         (:match val-expr clause ...)
         #:grammar ([clause [pat body ...+]
                            [pat (=> id) body ...+]
                            [pat #:when cond-expr body ...+]])]{
like @racket[match], except that it allowes identifiers with colons in patterns to specify types.  

For example the pattern @racket[x:num] is equivalent to the pattern @racket[(? number? x)].  

@racket[(:match val-expr [pat body-stuff ...] ...)] expands to
@racket[(match val-expr [(:pat pat) body-stuff ...] ...)].  

@examples[
  (require colon-match)
  (:match 1 [n:num n])
  (:match 'x [n:num n] [_ #f])
  (:match '(1 2 3) [(list a:num b:num c:num) (+ a b c)])
  (:match '(sym "str" 3) [(list-no-order n:num str:str sym:sym) (list n str sym)])
]}

@defform[#:kind "match expander"
         (:pat pat)]{
a match expander that rewrites @racket[pat] to change all of the identifiers written in colon notation
to a pattern that checks the type.  

A pattern of the form @racket[id:type] is rewritten to @racket[(type id)], and then @racket[type] is
is a match expander that expands to something like @racket[(? type? id)].  This means that you can
define your own ":match classes" by defining them as match expanders.  

@examples[
  (require colon-match racket/match racket/contract/base (for-syntax racket/base))
  (match '(1 2 3) [(:pat (list a:num b:num c:num)) (+ a b c)])
  (match '(1 2 3) [(list (:pat a:num) _ _) a])
  (define-match-expander pos-num
    (syntax-rules ()
      [(pos-num id) (? (and/c number? positive?) id)]))
  (match 1 [(:pat x:pos-num) x])
  (match -1 [(:pat x:pos-num) x] [_ #f])
  (:match 1 [x:pos-num x])
]}

@defform[(define-:match-class id pred-expr)]{
defines @racket[id] as a match expander that checks that the pattern satisfies @racket[pred-expr].
@racket[id] can then be used in a colon-notation pattern.

@examples[
  (require colon-match racket/contract/base)
  (define-:match-class pos-num (and/c number? positive?))
  (:match 1 [x:pos-num x])
]}

