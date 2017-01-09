# Functional Program

## Monoids
半幺群
Monoids是一种元素的集合，它需要满足结合律和幺元（Identity，也称为单位元，这种元和其他元素结合时，不会改变那么元素）这些约束条件。
可以证明输入和输出类型相同函数是一种 Monoids

1. $$\exists f(x) = x $$
2. $$(f.g).h == f.(g.h) since (f(g))f(g(h(x)))$$