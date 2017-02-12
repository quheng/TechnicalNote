# Category Theory

范畴论, 通俗的来讲, category theory 是忽略了集合内细节的集合论, 将集合视作一个整体, 着重于集合和集合之间的变换, 而不关心集合内具体的元素. 集合论中的术语多来自于拉丁语, 而范畴论中多来自于希腊语. 因此需要注意的是在 category theory 中, 即使是小写字母, 也用来表示一个集合, 而不是在我们熟悉集合论中大写字母是集合, 小写字母是元素.

## morphism
映射, 因为多用箭头表示, 也常叫做 'arrow'. 下文多用 [Haskell](https://www.haskell.org) 的语法表示.
集合论中称作 'function', 线性代数中的 '线性变换, linear transformations', 群论( group theory) 中的 'group homomorphisms', 拓扑学(topology) 中的 'continuous functions'. 与这些术语在大的概念上一致, 在细节上却有很大不同.

以集合论中的 function 举例. 在集合论中, 我们常说 $$f(x) x\in S $$ 来表示对集合 S 中的某个元素进行变换. 而在范畴论中的 'morphism' 确不会关心集合 S 中的具体元素 x, 而只说 `f::a->b` 表示从集合 a 到 b 的一个变换. 这里说的集合, 翻译自 category, type, class 等等.

### 组合 composition
组合: `g.f` 或者是'f∘g', 读作 g after f, 表示g(f(x)). 满足一下两个原则, 
1. Identity: 存在 morphism `id_a::a->a`, 使得 `id_b . f = f = f . id_a`
2. Associativity: `(f.g).h = f.(g.h)`

### epimorphism
也可以称作'epic morphism'
对应集合论中的 surjective (or onto), 满射. 回顾一下满射 $$\forall y \in Y, \exists y \in Y, f(x)=y $$. 由于 category theory 不关心具体的元素, 我们不能用具体的 x 和 y 来形容 epimorphism. 



## words and notation
1. `g.f` 或者是圆圈读作 g after f, 表示g(f(x))
2. morphism 映射, arrow, function
3. isomorphism 同构. 若 `f::a->b`, 存在 `g::b->a` 使得 `g.f = id_a f.g = id_b` 则称 `f` is isomorphism
4. injective, 单射. 对于任意的 a, b. `f(a) != f(b)`. 希腊语 `monomorphism` or `monic morphism`
5. 




