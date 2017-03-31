# SICP

## what is data 
P123
> In general, we can think of data as defined by some collection of selectors and constructors, together with specified conditions that these procedures must fulfill in order to be a valid representation.

## Church numerals
P126
> (define zero (lambda (f) (lambda (x) x))) (define (add-1 n)(lambda (f) (lambda (x) (f ((n f) x)))))
this representation is known as Church numerals, after its inventor, Alonzo Church, the logician who invented the λ- calculus.

翻译成 js 是
```
const zero = 
    (f) => 
      (x) => x

const addOne = 
  (n) =>
    (f) =>
      (x) =>
        f(n(f)(x))
```

## dotted-tail
(define (f x y . z) ⟨body⟩)
(define (g . w) ⟨body⟩)

(define f (lambda (x y . z) ⟨body⟩))
(define g (lambda w ⟨body⟩))
值得注意的是 `lambda` 的形式有一点不一样

## fold
ex 2.38
满足结合律左右折叠才一样
