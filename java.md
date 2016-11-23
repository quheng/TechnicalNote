#Java 8
## functional interface
1. Predicate<T> 参数: T, 返回值: boolean
2. Consumer<T> 参数: T, 返回值: void
3. Function<T, R> 参数: T, 返回值: R
4. Supplier<T> 参数: None, 返回值: T
5. UnaryOperator<T> 参数: T, 返回值: T  单元操作符
6. BinaryOperator<T, R> 参数: (T, T), 返回值: R

## default 与 多重继承
为了扩展之前的类库使之支持一些函数式的用法, Java8 为之前的一些类库添加了一些方法(Collection.forEach()). 为了保证先前兼容性(第三方类库中为 Collection 实现了 forEach() 方法), Java8 提供了 static 和 default 两个关键字. 使得
