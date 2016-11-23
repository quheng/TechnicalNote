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
