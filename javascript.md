# JavaScript

## built-in type
1. `string`
2. `number`
3. `boolen`
4. `null` and `undefined`
5. `object`
6. `symbol` （es6）

note: 
除了 null 和 undefined
BuiltInType.\_\_proto\_\_ 为内建对象
BuiltInType.\_\_proto\_\_.\_\_proto\_\_ 为 Object
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

## 相等
`==` 会做类型转换， `===`不会 

```js
var a = "42";
var b = 42;

a == b;         // true
a === b;        // false
```

### Falsy
1. "" 空字符串
2. 0， -0，Nan
3. null, undefine
4. false

## var and let
1.var 的作用域是最近的函数块，let 是最近的代码块(包括 if 之类)

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

```js
function foo() {
    function bar(a) {
        i = 3; // changing the `i` in the enclosing scope's for-loop
        console.log( a + i );
    }

    for (let i=0; i<10; i++) {  // var 会造成死循环
        bar( i * 2 );
    }
}

foo();
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

## 闭包
保存运行时 lexical scope 内变量的引用

## this
函数调用时的最高最后一层的 `object`, 可以用 `call 和 `apply` 绑定 `this`. ES5 中可以用 `bind`
1. `var bar = new foo()` this 为新的对象
2. `var bar = foo.call( obj2 )` this 为绑定的对象
3. `var bar = obj1.foo()` this 为调用的对象
4. `var bar = foo()` 严格模式下为 `undefine`, 否则为 `global`

## new
1. 创造一个新的对象
2. 绑定 prototype
3. 绑定 this 到新创建的对象
4. 返回新创建的对象
我们可以这样实现 new：
```Javascript
function new(constructor) {
 var obj = {}
 Object.setPrototypeOf(obj, constructor.prototype)
 var argsArray = Array.from(arguments)
 return constructor.apply(obj, argsArray.slice(1)) || obj
}
```

new 和 call, apply 不能同时使用

## \_\_proto\_\_ 与 prototype
### \_\_proto\_\_
\_\_proto\_\_ 访问属于任意对象的[[prototype]], 可以使用 Object.setPrototypeOf() 来改变，用来实现 OOP 的类层级结构，它对应的是创建这个对象的函数(constructor)的 prototype
### prototype
prototype 属于 Function, 只用在 new foo() 中，用来实现 OOP 中的 class，作为返回值的 \_\_proto\_\_
![](/assets/21B10292-38FD-479E-AF9E-CB870B865974.png)

## Object.create()
用一个对象作为\_\_proto\_\_创建一个对象
我们可以这样实现 Object.create(), ----- by《JavaScript高级程序设计》P169
```Javascript
function create(obj) {
    function temF() {}
    temF.prototype = obj
    return new F();
}
```