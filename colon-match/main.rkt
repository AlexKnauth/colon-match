#lang racket/base

(provide :match
         :pat
         define-:match-class
         )

(require racket/match
         (prefix-in rkt: racket/bool)
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     syntax/stx
                     racket/match
                     racket/string
                     ))

(define-syntax :match
  (lambda (stx)
    (syntax-parse stx
      [(match_ val-expr:expr
               [pat:expr body ...+] ...)
       (syntax-property
        #'(match val-expr
            [(:pat pat) body ...] ...)
        disappeared-use (list (syntax-local-introduce #'match_)))])))

(define-match-expander :pat
  (lambda (stx)
    (syntax-parse stx
      [(_ pat:expr) (rewrite #'pat)])))

(begin-for-syntax
  (define disappeared-use 'disappeared-use)
  
  (define (rewrite stx)
    (syntax-parse stx #:literals (quote quasiquote)
      [pat:id (rewrite-id #'pat)]
      [(quote dat) stx]
      [((~and qq quasiquote) qp)
       (with-syntax ([qp (rewrite-qp #'qp)])
         (syntax/loc stx (qq qp)))]
      [(id:id pat ...)
       (with-syntax ([(p ...) (map rewrite (syntax->list #'(pat ...)))])
         (syntax/loc stx (id p ...)))]
      [(pat ...)
       (with-syntax ([(p ...) (map rewrite (syntax->list #'(pat ...)))])
         (syntax/loc stx (p ...)))]
      [#(pat ...) stx]
      [#&pat stx]
      [pat #:when (prefab-struct-key (syntax-e #'pat))
           stx]
      [_ stx]))
  
  (define (rewrite-id stx)
    (define sym (syntax-e stx))
    (define str (symbol->string sym))
    (match (split str)
      [#f stx]
      [(list pat-str type-str)
       (let ([pat-sym (string->symbol pat-str)]
             [type-sym (string->symbol type-str)]
             [stx.source (syntax-source stx)]
             [stx.line (syntax-line stx)]
             [stx.column (syntax-column stx)]
             [stx.position (syntax-position stx)]
             [stx.span (syntax-span stx)])
         (define pat-str.length (string-length pat-str))
         (define type-str.length (string-length type-str))
         (define pat-id
           (datum->syntax stx pat-sym
                          (list stx.source
                                stx.line
                                stx.column
                                stx.position
                                pat-str.length)
                          (orig stx)))
         (define type-id
           (orig
            (datum->syntax stx type-sym
                           (list stx.source
                                 stx.line
                                 (+ stx.column pat-str.length 1)
                                 (+ stx.position pat-str.length 1)
                                 type-str.length))))
         (with-syntax ([pat pat-id]
                       [type type-id])
           (syntax/loc stx
             (type pat))))]))
  
  (define (split str)
    (match (string-split str ":" #:trim? #f)
      [(list) (match str
                ["" #f]
                [":" (list "" "")])]
      [(list s) #f]
      [(list pat-strs ..1 ty-str)
       (list (match (string-join pat-strs ":")
               ["" "_"]
               [s s])
             ty-str)]
      ))
  
  (define (orig stx)
    (syntax-property stx 'original-for-check-syntax #t))
    
  
  (define (rewrite-qp stx)
    (syntax-parse stx #:literals (unquote unquote-splicing)
      [sym:id stx]
      [((~and uq unquote) pat) 
       (with-syntax ([p (rewrite #'pat)])
         (syntax/loc stx (uq p)))]
      [((~and uqs unquote-splicing) pat)
       (with-syntax ([p (rewrite #'pat)])
         (syntax/loc stx (uqs p)))]
      [(qp ...) (rewrite-qp-list stx)]
      [#(qp ...) (rewrite-qp-vector stx)]
      [#&qp (rewrite-qp-box stx)]
      [qp #:when (prefab-struct-key (syntax-e #'qp))
          (rewrite-qp-prefab stx)]
      [_ stx]
      ))
  
  (define (rewrite-qp-list stx)
    (syntax-parse stx
      [(qp ...)
       (with-syntax ([(qp ...) (stx-map rewrite-qp #'(qp ...))])
         (syntax/loc stx (qp ...)))]))
  
  (define (rewrite-qp-vector stx)
    (syntax-parse stx
      [#(qp ...)
       (with-syntax ([(qp ...) (rewrite-qp-list #'(qp ...))])
         (syntax/loc stx #(qp ...)))]))
  
  (define (rewrite-qp-box stx)
    (syntax-parse stx
      [#&qp
       (with-syntax ([qp (rewrite-qp #'qp)])
         (syntax/loc stx #&qp))]))
  
  (define (rewrite-qp-prefab stx)
    (let ([key-datum (prefab-struct-key (syntax-e stx))])
      (match (struct->vector (syntax-e stx))
        [(vector struct:key-datum subqps ...)
         (define dat
           (apply make-prefab-struct key-datum
                  (map rewrite-qp subqps)))
         (datum->syntax stx dat)])))
  
  )

(define-syntax define-:match-class
  (syntax-parser 
    [(define-match-class id:id pred:expr)
     #'(begin
         (define id? pred)
         (define-match-expander id
           (lambda (stx)
             (syntax-parse stx
               [(_ pat:expr) (syntax/loc stx (? id? pat))]))))]
    ))

(define-syntax-rule (defprov-:match-classes [id pred] ...)
  (begin (provide id ...)
         (define-:match-class id pred) ...))

(define (true? x)
  (if x #t #f))

(define (any? x)
  #t)

(defprov-:match-classes
  [num number?]
  [int integer?]
  [nat exact-nonnegative-integer?]
  [str string?]
  [sym symbol?]
  [lst list?]
  [vec vector?]
  [proc procedure?]
  [hsh hash?]
  [char char?]
  [box box?]
  [bool boolean?]
  [truth true?]
  [true true?]
  [|#t| true?]
  [false rkt:false?]
  [|#f| rkt:false?]
  [any any?]
  [|| any?]
  )


(module+ test
  (require rackunit
           (only-in racket/contract/base and/c))
  
  (check-equal? (:match 1 [n:num n]) 1)
  (check-equal? (:match 'x [n:num n] [_ 2]) 2)
  
  (check-equal? (:match "string" [s:str s]) "string")
  (check-equal? (:match 'x [s:str s] [_ 2]) 2)
  
  (check-equal? (:match (list 1 2 3) [l:lst l]) (list 1 2 3))
  (check-equal? (:match 'x [l:lst l] [_ 2]) 2)
  
  (check-equal? (:match (vector 1 2 3) [v:vec v]) (vector 1 2 3))
  (check-equal? (:match 'x [v:vec v] [_ 2]) 2)
  
  (check-equal? (:match #t [b:bool b]) #t)
  (check-equal? (:match #f [b:bool b]) #f)
  (check-equal? (:match 'x [b:bool b] [_ 2]) 2)
  
  (check-equal? (:match 'x [l l]) 'x)
  
  (check-equal? (:match '(2 x "foo" (3 4)) [(list n s f l) (list n s f l)]) '(2 x "foo" (3 4)))
  (check-equal? (:match '(42 x) [(list n:num s:sym) (list n s)]) '(42 x))
  
  (check-equal? (:match (list 1 "2" '|3|)
                        [(list a:1:num b:2:str c:3:sym)
                         (list a:1 (string->number b:2) (string->number (symbol->string c:3)))])
                (list 1 2 3))
  
  (check-equal? (:match '(1 2 #(1 2 3 (1 2 3 4)))
                        [(list a:num b:num (vector c:num d:num e:num (list f:num g:num h:num i:num)))
                         (list a b c d e f g h i)])
                (list 1 2 1 2 3 1 2 3 4))
  
  (check-equal? (:match '(1 2 #(1 2 3 (1 2 3 #&4)))
                        [`(,a:num ,b:num #(,c:num ,d:num ,e:num (,f:num ,g:num ,h:num #&,i:num)))
                         (list a b c d e f g h i)])
                (list 1 2 1 2 3 1 2 3 4))
  
  (check-equal? (:match #s(key-datum 1 2 3)
                        [`#s(key-datum ,a:num ,b:num ,c:num) (list a b c)])
                (list 1 2 3))
  
  (check-equal? (:match #s(key-datum_0 1 2 3)
                        [`#s(key-datum_0 ,a:num ,b:num ,c:num) (list a b c)])
                (list 1 2 3))
  
  (check-equal? (:match #hash((1 . "1") (2 . "2") (3 . "3"))
                        [(hash-table [1 a:str] [2 b:str] [3 c:str])
                         (list a b c)])
                (list "1" "2" "3"))
  
  (check-equal? (:match #(struct:num 1 2 3)
                        ['#(struct:num 1 2 3)
                         "yay!"])
                "yay!")
  
  (define-:match-class +num (and/c number? positive?))
  
  (check-equal? (:match 1 [x:+num x "yay!"])
                "yay!")
  
  (check-equal? (:match -1 [x:+num x #t] [_ #f])
                #f)
  
  (begin-for-syntax
    (require rackunit)
    (check-equal? (split "x") #f)
    (check-equal? (split "x:y") '("x" "y"))
    (check-equal? (split "x:y:z") '("x:y" "z"))
    (check-equal? (split ":x") '("_" "x"))
    (check-equal? (split "x:") '("x" ""))
    (check-equal? (split ":") '("_" ""))
    (check-equal? (split ":x:y") '(":x" "y"))
    (check-equal? (split "x:y:") '("x:y" ""))
    (check-equal? (split "x:y::") '("x:y:" ""))
    (check-equal? (split "::") '(":" "")))
  
  )
