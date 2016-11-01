# Haskell

## ghci command
1. `:l` load module 等价于 `:load`
2. `:t` 查看数据类型
3. `:info function` 查看函数信息
4. `:set +t` 自动输出数据类型 `:unset +t` 取消
5. `:m +Data.Ratio` :m 等价于 :module 用于导入模块

## setup
you can add `:set prompt "ghci> "` to `~/.ghci`

## function
1. 两个参数的函数可以使用中缀(infix)的方式调用: e.g. 10 \`div\` 5 等价于 div 10 5, 即 10 除以 5
2. 函数名字可以用单引号, 但不能用双引号, 单引号表示和没有单引号的函数有一点微小的不同
3. 函数名字不能以大写字母开头, 只有类型名可以以大写字母开头
4. 函数中不能读取文件, 连接网络, 读取数据库, 读取时间, 使用全局变量等, 保证函数的执行是幂等的.
5. 只对合法输入的子集有合法输出的函数叫 partial function, 函数名应该以 unsafe 开头. 反之叫做 total function.
6. `->` 代表一个函数有一个参数并且返回一个值. 所以, 可以说 Haskell 所有的函数**只能有一个参数**. 多参数的函数实际上是生成了多个取其中参数的函数, 取了前面参数的函数作为返回值取了下一个参数.


### where
将 let 块放在最后, 提高阅读性

### fold
[reading](http://www.cs.nott.ac.uk/~pszgmh/fold.pdf)

**note**

1. 原始递归函数
2. 类似于 mapreduce 中的 reduce
3. 尽可能使用 Data.List 中的 foldl'

1. foldl

```
foldl :: (a -> b -> a) a [b] a
foldl step zero (x:xs) = foldl step (step zero x) xs 
foldl _ zero [] = zero
```

(a -> b -> a): step 函数, (初始值, 读取到的 b , 输出新的累计值)
a: step 函数最初的初始值
[b]: 作用的 list
a: 最后的累计值
用 fold 写 sum 可以表示成
```
foldlSum xs = foldl step 0 xs
    where step acc x = acc + x
```

2. foldr

```
foldr :: (a -> b -> b) -> b -> [a] -> b

foldr step zero (x:xs) = step x (foldr step zero xs) 
foldr _ zero [] = zero
```

3. 转换

```
myFoldl f z xs = foldr step id xs z 
     where step x g a = g (f a x)
```

### lambda
`\` 代表匿名函数

### currying

将有一部分参数的函数设计成新函数

```
niceSum :: [Integer] -> Integer
niceSum xs = foldl (+) 0 xs    -- 之前的写法
nicerSum = foldl (+) 0         -- currying
```

### sections

用()生成第二个参数作为已知的, 只保留第一个参数新函数

```
foo = (`elem` ['a'..'z'])
foo 'a' -- True
foo '1' -- False
```

### as-pattern
举例说明
```
suffixes :: [a] -> [[a]] 
 suffixes xs@(_:xs') = xs : suffixes xs' 
 suffixes _ = []
```
xs 代表了 (_:xs'), 提高了代码的可阅读性, 更重要的是, 我们不需要在生成一个新的 newXs = (_:xs') 提高了性能


### (.) 操作符
f . g  代表了 f (g x)

### seq 
迫使把第一个参数的值求出来, 避免空间泄露

## 类型

`_` wild card 占位符, match everything


:t (==) -> (==) :: Eq a => a -> a -> bool

两个类型为 Eq 的变量, 返回 bool 类型的变量

:t fst -> fst :: (a, b) -> a

这里的a, b均是小写字母开头, 表面不是类型名. 这里更像是一种泛型, 省略类型名, 所有类型均可传入.

### 自定义数据类型

```

data BookInfo (可选参数的数据类型)= Book Int String [String]

 deriving (Show)

```

1. BookInfo: 类型名, 必须要大写

2. Book: value constructor 或者叫做 data constructor 必须大小, 使用它来调用该类型.

3. deriving: 怎么打印该类型的数据

### typeclasses
```
class BasicEq a where
    isEqual :: a -> a -> Bool

instance BasicEq Bool where
    isEqual True True = True 
    isEqual False False = True           
    isEqual _ _ = False
```

定义名叫(必须以大写字母开头)的 typeclasses. 这种类型的实例必须实现定义好的函数. 类似注册的概念, 表面 Bool 也是一种 BasicEq, 有 isEqual 函数. Bool 类型必须事先写好.

在 Haskell 98 中, 待注册的数据类型(例子中的 Bool)不能是类型参数如 String([Char]). 若要使用的话在文件的开头加上`{-# LANGUAGE TypeSynonymInstances #-}`. 若同时注册多个相同的函数, 会发生 overlapping, 可以使用OverLappingInstances 解决, 这时会自动使用最接近的函数.

### useful function
1. Show 转成 String, 可以用 show 函数将 Show 转成 String
2. Read 读取 String, 可以用 read 函数将 String 转成一个 data

### type data newType
1. type 只是取一个别名
```
type NewInt = Int
(1 :: NewInt) + (1 :: NewInt)   -- 所有可以使用 NewInt 的都可以使用 Int
```
2. newType 只能定义一个构造函数且只能有一个 Field
```
newtype TooFew = TooFew -- error 没有 Field
newtype TooManyFields = Fields Int Int -- error 两个 Field
newtype TooManyCtors = Bad Int   -- error 两个构造函数
                      | Worse Int
```

### 指定数据类型

2 :: float -> 2.0

### 常用数字类型
real world haskell pdf P185, book 145


## module
Haskell 对应一个单一的 module, module 中表明了给外界提供了什么样的功能. module name 和数据类型一样必须以大写字母开头.
e.g.

```
module SimpleJSON
    ( 
      JValue(..)   -- .. 表示 JValue 所有的 constructors
    , getString
    , getInt
    , getDouble
    , getBool
    , getObject
    , getArray
    , isNull
    ) where

module ExportEverything where

module ExportNothing () where
```

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
5. length list 长度, 会将整个 list 遍历一遍 length [1,2,3] -> 3
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