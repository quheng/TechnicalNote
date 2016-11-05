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

## isPresent, get
尽量**避免**使用isPresent().
可以看到 isPresent() 的实现是:

```
/**
 * Return {@code true} if there is a value present, otherwise {@code false}. 
 * 
 * @return {@code true} if there is a value present, otherwise {@code false} 
 */
public boolean isPresent() { 
    return value != null;
}
```

和之前用 if 的处理一模一样, 尽量使用其他 api 的简便形式, 如 orElse, ifPresent等.

## orElseGet, map, flatMap

```
有值则返回, 否则返回默认值
```

```
如果有值，则对其执行调用mapping函数得到返回值。如果返回值不为null，则创建包含mapping返回值的Optional作为map方法返回值，否则返回空Optional。
```

```
flatMap方法与map方法类似，区别在于mapping函数的返回值不同。map方法的mapping函数返回值可以是任何类型T，而flatMap方法的mapping函数必须是Optional。
```

which one is better?

## filter
```
如果有值并且满足断言条件返回包含该值的Optional，否则返回空Optional。
```
