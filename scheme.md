# quote
`quote` 并不是函数调用，它是一种特殊的语法。`quote` 的作用是将代码视作一种程序，便于操作语法树。

对于数字和字符串，在任何情况下都是常量，`quote` 不起任何作用
```
'2 => 2
'2/3 => 2/3

(quote "Hi Mom!") => "Hi Mom!"
```

对于 list，得到相应符号的 list
```
(quote (a b c d)) => (a b c d)
```
```
(quote (quote cons)) => (quote cons)
(car (quote (quote cons))) => quote
```
在这个例子中, 第二个 `quote` 并不起任何作用, 和任何的 symbol 相同, 因为外层的 `quote` 并不会执行后面的代码.

相应的`(list (a b c d))` 在 a,b,c,d 未定义的情况下会报错


# list
若 `(cdr a)` 得到的是一个 `list` 那么 `a` 被称作 * proper list*, 空 list `()` 也是 * proper list * 虽然 `(cdr '())` 会报错. 反正被称作 * improper list *,例如 `(cons 'a 'b)` 就是一个 * improper list *, 用点号标记(dotted-pair notation) `(a . b)`, `(cdr '(a . b)) => b`. 

注意, 点号标记在后一个元素是 `list` 的时候会自动取消,例如

```
'(a . (b . (c . ()))) => (a b c)
```

## car cdr
`car` Contents of the Address part of Register number 取列表中的第一个

`cdr` Contents of the Decrement part of Register number 取列表中的剩下的

注意, a, d 可以组合使用, 组合方式先右后左
```
(caar '(( 1 2 3) 4 5 6)) => 1
(cddr '(( 1 2 3) 4 5 6)) => (5, 6)
(cadr '(( 1 2 3) 4 5 6)) => 4 ;; 先 d 后 a
(cdar '(( 1 2 3) 4 5 6)) => (2 3) ;; 先 a 后 d
```

# procedure
1. 参数的处理顺序是不确定的，甚至对解释器对于不同的应用有不同的顺序。
2. procedure 也会被求值: `((car (list + -)) 1 2)`
 

# let let* letrec
## tips
1. *一些*解释器支持方括号提供可读性 `(let ([f +]) (f 2 3)) => 5`
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
letrec创建的词法变量不仅可以在letrec执行体中可见而且在初始化中也可见。letrec是专门为局部的递归和互递归过程而设置的。 ex 4.20
```
(letrec
    ((fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1)))))))
    (fact 10)
)
```
