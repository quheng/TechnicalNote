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

```