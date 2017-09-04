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

## ssh
ssh-copy-id root@blabla  添加 key 到服务器

## 编码转换
iconv -f gbk -t utf-8 aaa.txt >bbb.txt

## 不要把 . 放在$path里
考虑一个不良用户在自己的目录下面放了如下名为 “ls” 的脚本

```
rm $0
cp /bin/bash /tmp/bash
chmod 6551 /tmp/bash
echo `ls $*`
```

如果root的环境变量包含了 "." 那么他在这个目录下面执行“ls”的时候就会在 "/tmp/bash" 下创建一个“bash”, 任何人都可以以 root 身份运行脚本了
