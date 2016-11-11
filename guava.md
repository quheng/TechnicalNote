# Guava
## Ordering
### creation
1. allEqual()  所有元素都一样, 对于稳定排序算法不会改变顺序, 可序列化.
2. arbitrary()  任意顺序, 使用 objects.hashCode() 进行排序, 不可序列化,.
3. compound()  可以是一个 Comparator List, 用来处理相等的情况(即 ordering 返回 0), 直到其中一个 Comparator 返回值不是零.
4. explicit()  给定顺序排序, 若出现了为给定顺序的元素, 会抛出 IncomparableValueException
5. from() 从一个comparator 生成 Ordering
6. natural() 自然序
7. usingToString() 转成 String 之后按字典序
8. 新建

```
Ordering<String> byLengthOrdering = new Ordering<String>(){
 public int compare(String left, String right) {
     return Ints.compare(left.length(), right.length());
 }
};
```

### chaining 链式调用

reference: [Guava github wiki](https://github.com/google/guava/wiki)

