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

## list

### note
list 嵌套 list 时, 内部 list 元素的数量可以不一样, 但是数据类型必须一样.

### 字符串
"abcd" 等价于 ['a', 'b', 'c', 'd']

### ++
连接两个list, 需要注意的是 Haskell 会遍历左边的list, 尽可能的把短的 list 放在左边. e.g. "hello" + "world" 得到 "hello world"

### :
将一个元素放在一个list的开头, e.g. 'a' + "bcd" 得到 "abcd"

### !!
获取 list 中的元素, e.g. "abcdefg" !! 1 得到b. Haskell 也是从 0 开始数数的. 访问大于 list 大小的元素会Exception `index too large`.

### 比较
一个一个元素进行比较

```
[1, 2] < [2]
[1, 2, 3] > [1, 2]
[1, 2] == [1, 2]
```
