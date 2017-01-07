## Iterm2
### 使用 opt+delete 删除整个单词
Preferences > Profiles > Keyboard you can check "Use option key as +Esc."

### 在单词之间跳转
Preferences > Key > add opt-> as "send escape sequence". b 向左, f 向右

## cut
1. cut -b 1-4 filename 取文件每一行的前四个字节
2. cut -c 1-4 filename 取文件每一行的前四个字符
3. cut -f 1,2 -d ';' filename 取按照 ";" 分割的1, 2项
e.g.
```
-> echo "a;b;c;d;e;f;g" | cut -f 1,3 -d ';'
-> a;c
```

## comm

## uniq

## sort

## jq 
处理 json 文件
sample

```
cat s6.txt | cut -f 2 -d '|' | jq '.logistics_address_search_response.addresses.address_result[0] | .province + "," + .city + "," + .country'
```

## pbpaste
获取粘贴板内容

## lsof
1. lsof -i :8080 查看占用了 8080 端口的进程

## grep
1. -i 忽略大小写
2. -v 输出未选中

