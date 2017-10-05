# interesting c++
c++ 11

## 隐式转换

```
void f(int a) {}

int main(int argc, char const *argv[])
{
	int a = 1.1;  // warning
	double b = 2; 
	a = b;
	a = b + a;
	a = a + b;
	f(b);
	int c {1.1}; // error
	return 0;
}
```


## reference

```
	double a {1.0};  // OK
	int &b = a;  // non-const lvalue reference to type 'int' cannot bind to a value of unrelated type 'double'
	int &c = 1;  // non-const lvalue reference to type 'int' cannot bind to a temporary of type 'int'
	const int &d = a;  // OK bind to a temporary variabe
	const int &e = 10;  // OK
	const int &f = a * 2;  // OK bind to a temporary variabe
```

临时量是 *常量*， 不能绑到 non-const lvalue reference，好像合理。。。。。。

## constexpr
```
constexpr long long foo(int times) {
	long long count {0};
	for (int i = 0; i < times; ++i)
	{
		count++;

	}
	return count;
}

int main(int argc, char const *argv[])
{
	constexpr long long res = foo(500000); // OK
	// constexpr long long res = foo(5000000); // constexpr variable 'res' must be initialized by a constant expression 
	printf("%lld\n", res);
	return 0;
}
```

## typedef

```
char a = 'a';
char b = 'b';
typedef char *cs;  // 	using cs = char*;
const char *c = &a;
const cs d = &a;
c = &b;
d = &b;  // cannot assign to variable 'd' with const-qualified type 'const cs' (aka 'char *const')
*c = 'b';  // error: read-only variable is not assignable
*d = 'b';
```

typedef 会改变 *基本数据类型* 使得 const 的作用对象改变。
const char *c； 指向 const char 的指针， 基本类型是 char
const cs d； 指向 char 的 const 指针，基本类型是 char*


## array paramater

```
void foo(int a[10]) {
	cout << sizeof (a) << " " << sizeof a << endl;
	// sizeof on array function parameter will return size of
      'int *' instead of 'int [10]' [-Wsizeof-array-argument]
      // output is 8(depends on your computer)
	
	for (auto item : a) {
		cout << item << endl;
	}
	//annot build range expression with array function
      parameter 'a' since parameter with array type 'int [10]' is treated as
      pointer type 'int *'
}

int main(int argc, char const *argv[])
{
	int b[3] {1, 2, 3};
	cout << sizeof (b) << " " << sizeof b << endl;
	foo(b);

	return 0;
}
```
