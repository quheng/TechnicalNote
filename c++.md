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
void foo(int* a);
void foo(int a[]);
void foo(int a[10]);
的类型相同

## array and pointor

```
int a[4];

a == &a
```
a和&a转换为void\*类型，值确实一样，但是这个a和&a表示的意义不同。a本身是数组名，类型是int [4]，这种类型是可以用在需要指针的地方的，相当于int*。根据类型推导，&a的类型是int(*)[4]，表示"指向4个元素int数组的指针"，这个地址值与前面的a相同，但是a和&a不等价，例如a+1会增加4个字节，而&a+1会增加16字节(一个int[4]的长度)。&a[0]是int*类型，和a在大多数情况下等价(除了sizeof等)。

