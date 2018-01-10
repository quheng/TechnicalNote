# Python

1. groupBy

```
from itertools import groupby
from operator import itemgetter

test_data = [(1,2), (1,3), (2,2), (2,3)]
for key, group in groupby(test_data, itemgetter(0)):
    print(key, list(group))
```
output:
1 [(1, 2), (1, 3)]
2 [(2, 2), (2, 3)]

but!!!

```
from itertools import groupby
from operator import itemgetter

test_data = [(1,2), (1,3), (2,2), (2,3)]
res = list(groupby(test_data, itemgetter(0)))
print([x[1] for x in res])
```

output:
[[], [(2, 3)]]

but!!!!

```
res = groupby(test_data, itemgetter(0))
print([list(x[1]) for x in res])
```

output:
[[(1, 2), (1, 3)], [(2, 2), (2, 3)]]


groupby 的结果共享一个 generator！
