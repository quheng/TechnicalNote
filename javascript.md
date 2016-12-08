# JavaScript

## built-in type
1. `string`
2. `number`
3. `boolen`
4. `null` and `undefined`
5. `object`
6. `symbol` （es6）

```
var a;
typeof a;               // "undefined"

a = "hello world";
typeof a;               // "string"

a = 42;
typeof a;               // "number"

a = true;
typeof a;               // "boolean"

a = null;
typeof a;               // "object" -- weird, bug

a = undefined;
typeof a;               // "undefined"

a = { b: "c" };
typeof a;               // "object"
```

## Truthy & Falsy
### Truthy
1. "hello"
2. 42
3. true
4. [], [1, 2]  数组
5. {}, {a:42}  对象
6. function foo() {}  函数

### Falsy
1. "" 空字符串
2. 0， -0，Nan
3. null, undefine
4. false

## var and let
1.var 的作用域是最近的函数块，let 是最近的闭包

```js
function allyIlliterate() {
    //tuce is *not* visible out here

    for( let tuce = 0; tuce < 5; tuce++ ) {
        //tuce is only visible in here (and in the for() parentheses)
    }

    //tuce is *not* visible out here
}

function byE40() {
    //nish *is* visible out here

    for( var nish = 0; nish < 5; nish++ ) {
        //nish is visible to the whole function
    }

    //nish *is* visible out here
}
```
2.在最外层是都可以全局引用，但是在 var 会加入到 `window` 中， let 不会

```js

let me = 'go';  // globally scoped
var i = 'able'; // globally scoped

console.log(window.me); // undefined
console.log(window.i); // 'able'
```
3.重复定义
```js
'use strict';
let me = 'foo';
let me = 'bar'; // SyntaxError: Identifier 'me' has already been declared

'use strict';
var me = 'foo';
var me = 'bar'; // No problem, `me` is replaced.
```