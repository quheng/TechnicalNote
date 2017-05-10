#Java 8
## functional interface
1. Predicate<T> 参数: T, 返回值: boolean
2. Consumer<T> 参数: T, 返回值: void
3. Function<T, R> 参数: T, 返回值: R
4. Supplier<T> 参数: None, 返回值: T
5. UnaryOperator<T> 参数: T, 返回值: T  单元操作符
6. BinaryOperator<T, R> 参数: (T, T), 返回值: R
讲的挺好的[阅读材料](http://zh.lucida.me/blog/java-8-lambdas-insideout-language-features/): [原文](http://cr.openjdk.java.net/~briangoetz/lambda/lambda-state-final.html)

## default 与 多重继承
为了扩展之前的类库使之支持一些函数式的用法, Java8 为之前的一些类库添加了一些方法(Collection.forEach()). 为了保证先前兼容性(第三方类库中为 Collection 实现了 forEach() 方法), Java8 提供了 default 两个关键字. 使得接口也可以拥有方法. 由于 class 可以继承多个 interface, 这在一定程度上实现了被 Java 摒弃的多重继承, 但是 Java8 的多重继承只能继承方法而不能继承变量(状态).

默认方法的调用三定律

1. 类胜于接口: 类中定义的方法之后忽略接口中定义的方法
2. 子类胜于父类
3. 如果没有对应前两条, 要么实现该方法, 要么将该方法声明为抽象方法

第一条是为了向后兼容.

## interface static method
如果一个方法有充分的语义和某个概念相关, 那么就可以将该方法写在对应的 interface 之中.


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

### 参数检查

当需要保证参数不可能是 null 的时候应该立即用 Optional.of(obj); 若某个参数的值为空时需要特殊处理, 则应该使用 Optional.ofNullable() 来提醒自己要处理 null 的情况.

避免在参数中使用 Optional<T>.

### isPresent, get

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

### orElseGet, map, flatMap

有值则返回, 否则返回默认值
如果有值，则对其执行调用mapping函数得到返回值。如果返回值不为null，则创建包含mapping返回值的Optional作为map方法返回值，否则返回空Optional。

flatMap方法与map方法类似，区别在于mapping函数的返回值不同。map方法的mapping函数返回值可以是任何类型T，而flatMap方法的mapping函数必须是Optional。

which one is better?

## filter

如果有值并且满足断言条件返回包含该值的Optional，否则返回空Optional。

# 泛型
Java 泛型代码内部无法得到任何有关泛型的信息，泛型的最大作用是保证在容器中取出正确的对象，在编译器保证类型使用一致，但不保留信息，避免向下转型， 将类型检查提前到插入时以及一定的类型安全。数组在创建的时候需要类型信息，因此不能创建泛型数组。 有两种解决办法，一种是使用Object数组，在get时再转型为 T 还有一种是使用容器。
擦除(erasure)保证了在代码更新到泛型的版本后，依旧可以依赖未更新的代码.

## volatile
不会被缓存， 不会和其他代码重排序 但是并没有锁， 依旧可能会有多个进程冲突的情况（i++  read i; i+1; wirte i; 在i+1的时候i被更新 无法保证i的可见性）需满足以下条件才可以使用volatile
1. 对变量的写入操作不依赖变量的当前值 
2. 该变量不会与其它变量一起纳入不变性的条件中 
3. 在访问变量时不需要加锁


## GC
java8 永久代从堆中移出，PermSize 变为MetaspaceSize
动态类生成导致方法区溢出
GC 关注 方法区 和 堆

### 引用计数
1. ，pro:简单高效，con:循环引用(怎么解决)

### 可达性分析
gc root 虚拟机栈(局部变量) native栈(native 方法局部变量) 方法区 静态变量 常量
强引用: 和root 相连
软引用: 可能有用，但不是必须。系统实在没有空间时收回，软引用收回后依旧不够，OOM
弱引用：生存到下一次垃圾回收时
虚引用：很弱 尽量不要使用
pro 不会有循环引用 con 效率低下，每个对象会被分析两次，第一次标记，第二次回收。内存碎片
对象可在finalize()拯救自己。但是finalize只会执行一次，因此只能拯救一次。DO NOT USE finalize

在大量使用反射，动态代理，CGlib等框架时，要注意无用类回收，避免永久代溢出
## 复制算法
顺序使用一半的内存，pro 效率高，没有碎片。con 减小了可用内存，存活率较高时效率下降 用于处理新代。

#### 标记整理算法
标记清除并在清除时移向一段，减少内存碎片 老年代

### GC 停顿  
gc 时暂停用户线程 越低响应速度越快
吞吐 GC的时间  越大执行效率越高
gc 停顿时间越短， 吞吐越低，新生代空间越小

准确式GC，oopMap 安全点 safe point 减少标记的工作量，降低gc 停顿？？

并发和并行？

新生代
1. Serial 收集器 单线程，gc 停顿时间较长 简单高效
2. ParNew 收集器 Serial 多线程版 能和CMS 结合使用 单CPU不会比Serial 效果好
3. Parallel Scavenge：吞吐量高。吞吐量优先收集器

老年代
1. Serial Old：标记整理算法
2. Parallel Old
3. CMS(Concurrent Mark Sweep): 停顿时间最短。初始标记，并发标记，重新标记，并发清除第一个真正的并发收集器 Con CPU敏感 浮动垃圾(floating garbage清除时的垃圾)， 需要在清理时留下备用空间，和后备收集器。碎片

全代
G1：短且可预测停顿，新生代和老年代不再物理隔绝 初始标记 并发标记 最终标记 筛选回收

GC 日志？

新生代满了 minor gc
长期存活 进入老年代 minor gc 年龄加1
minor gc进行之前 判断老年区最大可用连续区域是否比新生代所有对象之和大。 不是的话，看是否可以冒险执行(老年区存不下)， or full gc  冒险失败， full gc。
大对象 直接进去老年代 避免短命的大对象


## tips
1. char 默认使用unicode
2. main 要在没有任何对象的情况下调用  因此要static

