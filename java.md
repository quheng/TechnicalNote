#Java 8
## functional interface
1. Predicate<T> 参数: T, 返回值: boolean
2. Consumer<T> 参数: T, 返回值: void
3. Function<T, R> 参数: T, 返回值: R
4. Supplier<T> 参数: None, 返回值: T
5. UnaryOperator<T> 参数: T, 返回值: T  单元操作符
6. BinaryOperator<T, R> 参数: (T, T), 返回值: R

## default 与 多重继承
为了扩展之前的类库使之支持一些函数式的用法, Java8 为之前的一些类库添加了一些方法(Collection.forEach()). 为了保证先前兼容性(第三方类库中为 Collection 实现了 forEach() 方法), Java8 提供了 static 和 default 两个关键字. 使得接口也可以拥有方法. 由于 class 可以继承多个 interface, 这在一定程度上实现了呗 Java 摒弃的多重继承.

Java8 的多重继承只能继承方法而不能继承变量(状态), 如果一个方法有充分的语义和某个概念相关, 那么就可以将该方法写在对应的 interface 之中.

接口静态方法的调用三定律

1. 类胜于接口: 类中定义的方法之后忽略接口中定义的方法
2. 子类胜于父类
3. 如果没有对应前两条, 要么实现该方法, 要么将该方法声明为抽象方法

第一条是为了向后兼容.

##Nullable Object

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