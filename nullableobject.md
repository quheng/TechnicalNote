#Nullable Object

在 OO 编程中, 常常要处理的是 object pointer 是空的时候应该怎么处理, 代码中有许多
```
if (obj == null)
 do something
else 
 do the other things
```
在实际的变成中很可能去忘记考虑 null 的情况. 使用 Optional 迫使我们去主动的思考这里是空的情况应该怎么处理, 避免了一些潜在可能的 Bug.

JAVA 8 和 Guava 都提供了 Optional 来更好的解决这一问题. 下面的内容基于Java 8 中 java.util.Optional中的实现

## 参数检查
当需要保证参数不可能是 null 的时候应该立即用 Optional.of(obj); 若某个参数的值为空时需要特殊处理, 则应该使用 Optional.ofNullable() 来提醒自己要处理 null 的情况.
避免在参数中使用 Optional<T>.

## isPresent
尽量避免使用

