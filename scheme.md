# core language
```
<program>             -->   <form>*
<form>                -->   <definition> | <expression>
<definition>          -->   <variable definition> | (begin <definition>*)
<variable definition> -->   (define <variable> <expression>) 
<expression>          -->   <constant>
                        | <variable>
                        | (quote<datum>)
                        | (lambda <formals> <expression> <expression>*)
                        | (if <expression> <expression> <expression>)
                        | (set! <variable> <expression>)
                        | <application>
<constant>            -->  <boolean> | <number> | <character> | <string> <formals>             -->  <variable>
                        | <variable*>
                        | (<variable> <variable>*. <variable>)
<application>         --> (<expression><expression>*)
```
note: `*` 意思是 0 次以上， `datum` 是 scheme object 

# quote
`quote` 并不是函数调用，它是一种特殊的语法。`quote` 的作用是将代码视作一种程序，便于操作语法树。

对于数字和字符串，在任何情况下都是常量，`quote` 不起任何作用
```
'2 ;;=> 2

'2/3 ;;=> 2/3

(quote "Hi Mom!") ;;=> "Hi Mom!"
```

对于 list，得到相应符号的 list
```
(quote (a b c d)) ;;=> (a b c d)
```
```
(quote (quote cons)) ;;=> (quote cons)
(car (quote (quote cons))) ;;=> quote
```
在这个例子中, 第二个 `quote` 并不起任何作用, 和任何的 symbol 相同, 因为外层的 `quote` 并不会执行后面的代码.

相应的`(list (a b c d))` 在 a,b,c,d 未定义的情况下会报错


# list
若 `(cdr a)` 得到的是一个 `list` 那么 `a` 被称作 *proper list*, 空 list `()` 也是 * proper list * 虽然 `(cdr '())` 会报错. 反之被称作 *improper list*, 例如 `(cons 'a 'b)` 就是一个 *improper list*, 用点号标记(dotted-pair notation) `(a . b)`, `(cdr '(a . b)) => b`

两个元素的 *list* 被称作 *pair*

```
(pair?  '(a . b)) ;;=> #t
(pair?  '(a b)) ;;=> #f
```

注意, 点号标记在后一个元素是 `list` 的时候会自动取消,例如

```
'(a . (b . (c . ()))) ;;=> (a b c)
```

## car cdr
`car` Contents of the Address part of Register number 取列表中的第一个

`cdr` Contents of the Decrement part of Register number 取列表中的剩下的

注意, a, d 可以组合使用, 组合方式先右后左
```
(caar '(( 1 2 3) 4 5 6)) ;;=> 1
(cddr '(( 1 2 3) 4 5 6)) ;;=> (5, 6)
(cadr '(( 1 2 3) 4 5 6)) ;;=> 4 ;; 先 d 后 a
(cdar '(( 1 2 3) 4 5 6)) ;;=> (2 3) ;; 先 a 后 d
```

# procedure
1. 参数的处理顺序是不确定的，甚至对解释器对于不同的应用有不同的顺序。
2. procedure 也会被求值: `((car (list + -)) 1 2)`
 
# lambda
## 参数
有三种形式：
1. 单个元素： `var`
2. *proper list* `(var1 ... varn)`
3. *improper list* `(var1 ... varn . varr)`

第一种可以个任意个参数，会被会被放到一个 `list` 中，如：
```
(let ([f (lambda x x)]) (f 1 2 3 4)) ;;=> (1 2 3 4)
```

第二种给定参数数量

第三种为前两种的复合，前面给定数量，后面放进 `list`，若前面的数量就不足，则报错。
```
(let ([g (lambda (x . y) (list x y))]) (g 1 2 3 4)) ;;=> (1 (2 3 4))
(let ([g (lambda (x y z. w) (list x y z w))]) (g 1 2 )) ;;=> error g: arity mismatch;
```

# let let* letrec
*一些*解释器支持方括号提供可读性 `(let ([f +]) (f 2 3)) ;;=> 5`
对于不支持的解释器，请将方括号替换为圆括号。

## shadow
```
(let ([x 1])
  
  (let ([x (+ x 1)])
(+ x x))) ;;=> 4
```
`let` 第一个参数不属于 `body` 不会被覆盖掉（shadow），第二个参数会被覆盖掉。

```
(let ((<var1> <exp1>) ... (<varn><expn>))
  <body>
)
```
等价于
((lambda (<var1>...<varn>)
  <body>
) <exp1>...<exp2>)

(let* ((<var1> <exp1>) ... (<varn><expn>))
  <body>
)
等价于
```
((lambda (<var1>)
  .... 
  ((lambda (<varn>)
      <body>
    )<expn>
  )
)<exp1>)
```
使用 `define-syntax` 定义如下：

```
(define-syntax mylet
  (syntax-rules ()
    [(_ () b1 b2 ...) ((lambda () b1 b2 ...))]
    [(_ ((x e)) b1 b2 ...)
     ((lambda (x) b1 b2 ...) e)]
    [(_ ((x e) (x1 e1) ... ) b1 b2 ...)
     ((lambda (x) (mylet ((x1 e1) ...) b1 b2 ...)) e)]))
```

letrec 创建的词法变量不仅可以在letrec执行体中可见而且在初始化中也可见。letrec是专门为局部的递归和互递归过程而设置的。 ex 4.20
```
(letrec
    ((fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1)))))))
    (fact 10)
)
```

named let
```
(let name ((var expr) ...) body1 body2 ...)
```

等价于
```
(letrec ((name (lambda (var ...) body1 body2 ...))) (name expr ...))
```

## tips
有循环引用的时候需要使用 `-rec`，需要按顺序求值的时候使用 `-*`


# define
针对 lambda 的三种形式，define 也有三种形式
1. `(define var0 (lambda var e1 e2 ...))` 等价于 `(define (var0 . varr) e1 e2 ...)`
2. `(define var0 (lambda (var1 ... varn) e1 e2 ...)) ` 等价于 `(define (var0 . varr) e1 e2 ...)`
3. `(define var0 (lambda (var1 ... varn . varr) e1 e2 ...))` 等价于 `(define (var0 var1 ... varn . varr) e1 e2 ...)`

# if
`if`  是一个关键字而不是一个普通的过程, 原因在于 `lisp` 不是 *applicative order language*，如果 `if` 是一个普通的过程, 那么参数会被求值, 这会造成问题, 如下:
```
(define reciprocal
  (lambda (n)
    (if (= n 0)
        "oops!"
        (/ 1 n))))
```
会先求`(/ 1 n)` 的值, 这在 `n = 0` 的时候会出错. 因此, 需要将 `if`  变成一个关键字, 惰性求值. 对于 `or` `and` 等需要短路的语义类似, 也需要特殊处理.

# Continuations
 "what to do with the value" 
在计算的时候（不考虑并行），代码是顺序执行的，这就是说，每一句程序都有 *接下来* 的程序要运行，例如
`(if (null? x) (quote ()) (cdr x))` 在 `x` 是 `(a b c)` 的执行过程如下：

```
1. the value of (if (null? x) (quote ()) (cdr x)), 2. the value of (null? x),
3. the value of null?,
4. the value of x,
5. the value of cdr,
6. the value of x (again).
```

每一行程序之后的程序就是 *Continuations* 
continuation 是当前环境的一个打包, 记录了这个程序此时的所有状态, 以及接下来要运行什么。
Scheme 中 continuation 是 first-class的， 这意味着它可以被当做参数和返回值，也可以通过调用 continuation 执行后续程序。

## call/cc
call/cc 全称是 call-with-current-continuation，用于获取 `Continuation`

它接受一个参数 `receiver`, `receiver` 也是一个函数，它接受一个 `continuation` 参数,
```
(call/cc (lambda (continuation) (;; do with continuation)))
```
`continuation`, 便是调用 `call/cc` 处接下来要运行的程序。

例子：
```
(define (receiver continuation) (
  (continuation "magic")
))

(display (call/cc receiver)) ;; magic
```

例子中的 `continuation` 代表了接下来要运行的 `display`, 并将 "magic" 传进去。

更多的例子
```
(call/cc
  (lambda (k)
    (* 5 4))) ;;=> 20, 没有用 k， 简单返回了 20

(call/cc
  (lambda (k)
    (* 5 (k 4)))) ;;=>4， 将 4 传给了 'continuation' (REPL 的输出)， 故输出 4

(+ 2 (call/cc
  (lambda (k)
    (* 5 (k 4))))) ;;=> 6, 将 4 传给了 'continuation' ('(+ 2 blabla )'), 故输出 6

;; non-local exit 
(define product
  (lambda (ls)
    (call/cc
      (lambda (break)
        (let f ([ls ls])
          (cond
            [(null? ls) 1]
            [(= (car ls) 0) (break 0)]
            [else (* (car ls) (f (cdr ls)))]))))))
(product '(1 2 3 4 5))  ;;=> 120  在 receiver 中完成运算
(product '(7 3 8 0 1 9 5)) ;;=> 0 在 遇到 0 时直接跳到 'continuation' （REPL 的输出） 节省了函数返回的计算
```

一个复杂的例子：
```
(let ([x (call/cc (lambda (k) (k (lambda (first) (first 3)))))]) 
  (x (lambda (second) (+ second 2))))
```

`call/cc` 获取的 'continuation' 是 `(let ([x blabla]) (x (lambda (second) (+ second 2))))` 把 `(lambda (first) (first 3))` 作为返回给 `continuation` 得到 

```
(let ([x (lambda (first) (first 3))]) 
  (x (lambda (second) (+ second 2))))
```
因此计算结果是 `5`

我们还可以把 'continuation' 保存下来

```
(define cc #f)
(+ (call/cc (lambda (return)
                (set! cc return)
                1))
   1)  ;; => 2
(cc 1) ;; => 2
(cc 10) ;; => 11
```

这可以用来做
1. 断点跟踪，观察当前变量并可以继续执行
2. 多任务(multi-tasking) 的任务切换
3. 协程(coroutine)

和数理逻辑的关系还有待学习啦，引一段知乎的回答：
> Curry-Howard 同构的层面，call/cc 对应皮尔士定律，它代表着排中律，这条定律是 Lambda 演算所对应的直觉逻辑里没有的
> 作者：Belleve
> 链接：https://www.zhihu.com/question/21954238/answer/23855834
> 来源：知乎
> 著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。