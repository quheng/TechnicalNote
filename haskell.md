# Haskell

## setup
you can add `:set promot "ghci> "` to `~/.ghci`

## function
1. 两个参数的函数可以使用中缀(infix)的方式调用: e.g. 10 \`div\` 5 等价于 div 10 5, 即 10 除以 5
2. 函数名字可以用单引号, 但不能用双引号
3. 函数名字不能以大写字母开头

## load file
:l filename

## if-statement
else part is mandatory, 因为 if 语句在 是一个表达式, 而表达式在Haskell 中一定有返回值