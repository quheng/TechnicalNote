# Hive 

## Install
1. `brew install hive`
2. 配置metadata store
    1. `brew install mysql`
    2. `mysqld` 启动mysql服务
    3. `mysql -u root` 登录mysql, 将root替换为合适的用户
    4. `CREATE DATABASE metastore`
    5. `USE metastore`
    6. CREATE USER 'hiveuser'@'localhost' IDENTIFIED BY 'password';
    7. GRANT SELECT,INSERT,UPDATE,DELETE,ALTER,CREATE on metastore.* TO 'hiveuser'@'localhost';
    8. source $HIVE_HOME/libexec/scripts/metastore/upgrade/mysql/hive-scheme-2.1.0.mysql.sql 
3. 将mysql-connect-java.jar 放入$HIVE_HOME/libexec/lib
4. 复制$HIVE_HOME/libexec/conf文件夹下的hive-default.xml 为hive-site.xml, 并根据需求进行配置
5. `hive`

## Command
1. `hive -e "select * from table";` 执行单行查询
2. `hive -f hql_file.hql`  执行文件内语句
3. `! pwd` 可以用!执行shell 命令
4. `SHOW FUNCTIONS` 当前可用函数
5. `DESCRIBE EXTENDED table_name` 展示详细表的信息

## Table
### managed table
又叫做internal table. Hive 自己管理数据的生命周期, 但是和其他工具结合起来不是很方便

### external table
Hive本身不管理数据, e.g.

```SQL
CREATE EXTERNAL TABLE IF NOT EXISTS stocks (
    name     STRING,
    describe STRING,
    price    INT
)
ROW FORMAT DELIMITED FIRLDS TERMINATED BY ','
LOCATION '/data/stocks';
``` 
drop external table 并不会真的删除数据, 而是删除metadata.


## File Format
`STORED AS format` 在创建表的时候指定数据格式

### TextFile
默认数据格式, 便于使用pig和Unix工具. 但是在空间占用上不如二进制文件.

### SequenceFile
二进制key-value. Hadoop 支持的标准格式, 便于使用Hadoop相关的工具处理, 但是Hadoop之外的工具使用起来就比较麻烦. 可以方便的压缩在 block 和 record 级别压缩, 并支持以 block 划分文件用于并行处理.

 


