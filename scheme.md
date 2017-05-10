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
相应的`(list (a b c d))` 在 a,b,c,d 未定义的情况下会报错


# list
若 `(cdr a)` 得到的是一个 `list` 那么 `a` 被称作 * proper list*, 空 list `()` 也是 * proper list * 虽然 `(cdr '())` 会报错. 反正被称作 * improper list *,例如 `(cons 'a 'b)` 就是一个 * improper list *, 用点号标记(dotted-pair notation) `(a . b)`, `(cdr '(a . b)) => b`


# let let* letrec
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
