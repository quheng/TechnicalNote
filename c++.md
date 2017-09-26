# interesting c++

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
