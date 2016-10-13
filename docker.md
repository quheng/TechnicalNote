# Docker 

## Image
1. from net `docker pull [REGISTRYHOST/][USERNAME/]NAME[:TAG]`
2. save image as files `docker save -o myfile.tar image_name:tag`
3. load image from files `dokcer load -i myfile.tar`
3. installing from a docker file `docker build - < docker file`


## volume

## bind mount
`-v host_address:mount_address`

直接和主机进行绑定

**note**: 

1. host_address 下的内容会将原始 image 中mount_address下的内容覆盖
2. `:ro` 表示只读
3. 若 host_address 或 mount_address 不存在, 均会自动创建
4. host_address 可以是一个单独的文件
5. -v 参数可多次指定
6. 多个 container 指定同一个 host_address 即可文件共享


## managed volume
`-v mount_address`

与 docker 管理的空间进行绑定

**note**

1. `dokcer inspect -f {{json .Config.Volumes}}` container_name 查看 volume 信息
