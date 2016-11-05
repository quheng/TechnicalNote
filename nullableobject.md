#Nullable Object

在 OO 编程中, 常常要处理的是 object pointer 是空的时候应该怎么处理, 代码中有许多
```
if (obj == null)
 do something
else 
 do the other things
```
JAVA 8 和 Guava 都提供了 Optional 来更好的解决这一问题.

