# JavaScript

## var and let
var 的作用域是最近的函数块，let 是最近的闭包

```js
let me = 'go';  // globally scoped
var i = 'able'; // globally scoped
```

但是 var 会加入到 `window` 中， let 不会

```js
console.log(window.me); // undefined
console.log(window.i); // 'able'
```