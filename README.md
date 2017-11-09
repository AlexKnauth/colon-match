colon-match [![Build Status](https://travis-ci.org/AlexKnauth/colon-match.png?branch=master)](https://travis-ci.org/AlexKnauth/colon-match)
===

match with colon notation, based on discussion with @soegaard at http://lists.racket-lang.org/users/archive/2013-December/060826.html

documentation: http://pkg-build.racket-lang.org/doc/colon-match/index.html

The [`:match`](http://docs.racket-lang.org/colon-match/index.html#%28form._%28%28lib._colon-match%2Fmain..rkt%29._~3amatch%29%29) form is like [`match`](http://docs.racket-lang.org/reference/match.html#%28form._%28%28lib._racket%2Fmatch..rkt%29._match%29%29), except that it allowes identifiers with colons in patterns to specify different types of values. The pattern `x:num` matches a number and bind it at `x`, the pattern `v:str` matches a string and binds it to `v`, and so on.

Examples:

```racket
> (require colon-match)
> (:match 1 [n:num n])
1
> (:match 'x [n:num n] [_ #f])
#f
> (:match '(1 2 3) [(list a:num b:num c:num) (+ a b c)])
6
> (:match '(sym "str" 3)
    [(list-no-order n:num str:str sym:sym)
     (list n str sym)])
'(3 "str" sym)
```
