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

## 2.4 数据表示
### Tagged data 代标志的数据
给每一种数据带上标志，调用函数的时候通过标志来确定调用什么样的方法。缺点是函数需要知道所有的标志，添加新的标志需要修改所有的函数。编写新的数据类型的时候要注意不能重名。

### Data-Directed 数据导向
通过维护一个表格（数据类型 -> 方法），来决定函数分配(dispatch)，类似于 C++ 的虚表。称这个表为函数分配表。

### 消息传递 Message passing
所需要的操作通过字符作为`消息`传入, 通过构建一个`智能操作结构`，智能体现在可以根据消息分配函数。`智能操作结构` 决定了函数分配表中的列.

## 模块化、对象和状态

### 引用透明
p 315
> A language that supports the concept that “equals can be substituted for equals” in an expression without changing the value of the expres- sion is said to be referentially transparent. 

# let let* letrec


