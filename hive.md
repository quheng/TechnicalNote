# Hive 

## install
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