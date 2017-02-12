# Category Theory

范畴论, 通俗的来讲, category theory 是忽略了集合内细节的集合论, 将集合视作一个整体, 着重于集合和集合之间的变换, 而不关心集合内具体的元素. 集合论中的术语多来自于拉丁语, 而范畴论中多来自于希腊语. 因此需要注意的是在 category theory 中, 即使是小写字母, 也用来表示一个集合, 而不是在我们熟悉集合论中大写字母是集合, 小写字母是元素.

## morphism
映射, 因为多用箭头表示, 也常叫做 'arrow'. 下文多用 [Haskell](https://www.haskell.org) 的语法表示.
集合论中称作 'function', 线性代数中的 '线性变换, linear transformations', 群论( group theory) 中的 'group homomorphisms', 拓扑学(topology) 中的 'continuous functions'. 与这些术语在大的概念上一致, 在细节上却有很大不同.

以集合论中的 function 举例. 在集合论中, 我们常说 $$f(x) x\in S $$ 来表示对集合 S 中的某个元素进行变换. 而在范畴论中的 'morphism' 确不会关心集合 S 中的具体元素 x, 而只说 `f::a->b` 表示从集合 a 到 b 的一个变换. 这里说的集合, 翻译自 category, type, class 等等.

### 组合 composition
组合: `g.f` 或者是'f∘g', 读作 g after f, 表示g(f(x)). 满足一下两个原则, 
1. Identity: 存在 morphism `id_a::a->a`, 使得 `id_b . f = f = f . id_a`, id_a 又叫做 'isomorphism'
2. Associativity: `(f.g).h = f.(g.h)`

### epimorphism
也可以称作'epic morphism'
对应集合论中的 surjective (or onto), 满射. 回顾一下满射 $$\forall y \in Y, \exists y \in Y, f(x)=y $$. 通俗来说就是目标充满了整个目标域. 由于 category theory 不关心具体的元素, 我们不能用具体的 x 和 y 来形容 epimorphism. 这里用一个小技巧来描述 epimorphism. 若一个 morphism 不是 epimorphism, 我们关注没被映射到的目标域. ![](/assets/QQ20170213-002703@2x.png)
由于不是 epimorphism, 那么消除了不同 morphism 在没被映射到的目标域中的差异, 使得`g_1.f = g_2.f, g_1 != g_2`. 因此我们可以说, 如果`f is epimorphism, g_1.f  = g_2.f if and only if g_1 = g_2`, 这里利用了非满射的取消性[cancellative](https://en.wikipedia.org/wiki/Cancellation_property)

### monomorphism
也可以称作'monic morphism'
对应集合论中的

## words and notation
1. `g.f` 或者是圆圈读作 g after f, 表示g(f(x))
2. morphism 映射, arrow, function
3. isomorphism 同构. 若 `f::a->b`, 存在 `g::b->a` 使得 `g.f = id_a f.g = id_b` 则称 `f` is isomorphism
4. injective, 单射. 对于任意的 a, b. `f(a) != f(b)`. 希腊语 `monomorphism` or `monic morphism`
5. 




