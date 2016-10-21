# Haskell

## ghci command
1. `:l` load module
2. `:t` 查看数据类型
3. `:info function` 查看函数信息
4. `:set +t` 自动输出数据类型 `:unset +t` 取消
5. `:m +Data.Ratio` :m 等价于 :module 用于导入模块

## setup
you can add `:set prompt "ghci> "` to `~/.ghci`

## function
1. 两个参数的函数可以使用中缀(infix)的方式调用: e.g. 10 \`div\` 5 等价于 div 10 5, 即 10 除以 5
2. 函数名字可以用单引号, 但不能用双引号
3. 函数名字不能以大写字母开头, 只有类型名可以以大写字母开头

## load file
:l filename

## if-statement
else part is mandatory, 因为 if 语句在 是一个表达式, 而表达式在Haskell 中一定有返回值

## list

### note
list 嵌套 list 时, 内部 list 元素的数量可以不一样, 但是数据类型必须一样, 因为它们的类型都是 `[a]`.

### 字符串
"abcd" 等价于 ['a', 'b', 'c', 'd']

### ++
连接两个list, 需要注意的是 Haskell 会遍历左边的list, 尽可能的把短的 list 放在左边. e.g. "hello" + "world" -> "hello world"

### :
将一个元素放在一个list的开头, e.g. 'a' + "bcd" -> "abcd"

### !!
获取 list 中的元素, e.g. "abcdefg" !! 1 -> b. Haskell 也是从 0 开始数数的. 访问大于 list 大小的元素会Exception `index too large`.

### 比较
一个一个元素进行比较, 不同类型元素不能比较
```
[1, 2] < [2]

[1, 2, 3] > [1, 2]

[1, 2] == [1, 2]

[1, 2] > ['a'] // error
```

### useful function
1. head 第一个元素 head [1,2,3] -> 1, head [] 报错
2. tail 除掉一个元素的剩余元素 tail [1,2,3] -> [2,3], tail [] 报错
3. init 除掉最后一个元素的剩余元素 init [1,2,3] -> [1,2], init [] 报错
4. last 最后一个元素 last [1,2,3] -> 3, last [] 报错
5. length list 长度, length [1,2,3] -> 3
6. null 判断 list 是否为空  null [] -> true
7. take 取前 n 个元素, take 4 [1,2,3] -> [1,2,3] 注意: 大于list 大小不会报错
8. maximum, minimum, drop, sum, product, elem等等

### ranges
1. [1,2..] -> [1,2,3,4,5.......]
2. [1..20] -> [1,2,3,4........20]
3. ['a'..'d'] -> "abcd"
4. [2,4..10] -> [2,4,6,8,10]
5. take 10 circle("123") -> "1231231231"  list 为参数
6. take 10 repeat('1') > "1111111111" 元素为参数
7. replicate 3, '1' ->  "111"

### list comprehensive
1. [x * 2 | x <- [1..3]]  -> [2,4,6]
2. [x * 2 | x <- [1..3], x > 2]   -> [6]

e.g.
1. 求 list 大小 length' li = sum( [1| _<-li] )

## tuple
1. 固定大小
2. 可存放不同类型元素
note: tuple 不能只有一个元素, e.g. (1) -> 1

### useful function
1. fst
2. snd
3. zip

## 类型

`_` 占位符, match everything

:t (==) -> (==) :: Eq a => a -> a -> bool
两个类型为 Eq 的变量, 返回 bool 类型的变量

:t fst -> fst :: (a, b) -> a
这里的a, b均是小写字母开头, 表面不是类型名. 这里更像是一种泛型, 省略类型名, 所有类型均可传入.

### 指定数据类型
2 :: float -> 2.0

### useful function
1. show
2. read


## switch-case like (guard)

### as-pattern
```
test all@(first:second) = all
test "test string"
```
输出为 "test string"

### 样例
test_guard a b | c == 1 = "1"
               | c == 2 = "2"
               | otherwise = "1 nor 2"
               where c = a/b
注意ghci里不支持换行