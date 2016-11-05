#Nullable Object

在 OO 编程中, 常常要处理的是 object pointer 是空的时候应该怎么处理, 代码中有许多
```
if (obj == null)
 do something
else 
 do the other things
```
JAVA 8 和 Guava 都提供了 Optional 来更好的解决这一问题.

## 参数检查
当需要保证参数不可能是 null 的时候应该立即用 Optional.of(obj)



