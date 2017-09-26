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
	// constexpr long long res = foo(5000000); // 
	printf("%lld\n", res);
	return 0;
}
```

