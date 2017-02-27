# tips

## array is not pointer
> Go's arrays are values. An array variable denotes the entire array; it is not a pointer to the first array element (as would be the case in C). This means that when you assign or pass around an array value you will make a copy of its contents. (To avoid the copy you could pass a pointer to the array, but then that's a pointer to an array, not an array.) One way to think about arrays is as a sort of struct but with indexed rather than named fields: a fixed-size composite value.

```
a := [...]int{1}
b := a
b[0] = 0
fmt.Println(a)
// [1]
fmt.Println(b)
// [0]
```

## Redeclaration
```
f, err := os.Open(name)
d, err := f.Stat()
```
err 可以被定义两次, 只要
1. 两次定义在同一个范围内
2. 值被初始化给了对应变量
3. 第二次定义时, 同时定义了两个或以上变量.

## reference
1. https://blog.golang.org/go-slices-usage-and-internals
2. https://golang.org/doc/effective_go.html