# Category theory

范畴论, 通俗的来讲, category theory 是忽略了集合内细节的集合论, 将集合视作一个整体, 着重于集合和集合之间的变换, 而不关心集合内具体的元素. 集合论中的术语多来自于拉丁语, 而范畴论中多来自于希腊语.

## morphism
映射, 因为多用箭头表示, 也常叫做 'arrow'.
集合论中称作 'function', 线性代数中的 '线性变换, linear transformations', 群论( group theory) 中的 'group homomorphisms', 拓扑学(topology) 中的 'continuous functions'. 

## words and notation
1. `g.f` 或者是圆圈读作 g after f, 表示g(f(x))
2. morphism 映射, arrow, function
3. isomorphism 同构. 若 `f::a->b`, 存在 `g::b->a` 使得 `g.f = id_a f.g = id_b` 则称 `f` is isomorphism
4. injective, 单射. 对于任意的 a, b. `f(a) != f(b)`. 希腊语 `monomorphism` or `monic morphism`
5. surjective (or onto), 满射. 对于 `f::a->b`, 任意的 y 属于 b, 存在 `f(x) = y`. 希腊语 `epimorphism` or `epic morphism`




