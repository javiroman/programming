```
$ docker run -it --privileged --rm -v $PWD:/opt erlang bash

root@204b250ab713:/# ip a
1: lo: <LOOPBACK,UP,LOWER_UP> mtu 65536 qdisc noqueue state UNKNOWN group
default qlen 1000
    link/loopback 00:00:00:00:00:00 brd 00:00:00:00:00:00
    inet 127.0.0.1/8 scope host lo
       valid_lft forever preferred_lft forever
    inet6 ::1/128 scope host 
       valid_lft forever preferred_lft forever
70: eth0@if71: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state
UP group default 
    link/ether 02:42:ac:11:00:02 brd ff:ff:ff:ff:ff:ff link-netnsid 0
    inet 172.17.0.2/16 scope global eth0
       valid_lft forever preferred_lft forever
    inet6 fe80::42:acff:fe11:2/64 scope link 
       valid_lft forever preferred_lft forever

root@204b250ab713:/# cd /opt/

root@204b250ab713:/opt# erl -name server@172.17.0.2 -setcookie test
Erlang/OTP 21 [erts-10.0.5] [source] [64-bit] [smp:4:4] [ds:4:4:10]
[async-threads:1] [hipe]

Eshell V10.0.5  (abort with ^G)
(server@172.17.0.2)1> c(server).
{ok,server}
(server@172.17.0.2)2> server:start().
```


```
$  docker run -it --privileged --rm -v $PWD:/opt erlang bash

root@19f331bc2cab:/# cd /opt

root@19f331bc2cab:/opt# ip a
1: lo: <LOOPBACK,UP,LOWER_UP> mtu 65536 qdisc noqueue state UNKNOWN group default qlen 1000
    link/loopback 00:00:00:00:00:00 brd 00:00:00:00:00:00
    inet 127.0.0.1/8 scope host lo
       valid_lft forever preferred_lft forever
    inet6 ::1/128 scope host 
       valid_lft forever preferred_lft forever
72: eth0@if73: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP group default 
    link/ether 02:42:ac:11:00:03 brd ff:ff:ff:ff:ff:ff link-netnsid 0
    inet 172.17.0.3/16 scope global eth0
       valid_lft forever preferred_lft forever
    inet6 fe80::42:acff:fe11:3/64 scope link 
       valid_lft forever preferred_lft forever

root@19f331bc2cab:/opt# erl -name client@172.17.0.3 -setcookie test
Erlang/OTP 21 [erts-10.0.5] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [hipe]

Eshell V10.0.5  (abort with ^G)
(client@172.17.0.3)1> net_adm:ping('server@172.17.0.2').
pong
(client@172.17.0.3)2> c(client).
{ok,client}
(client@172.17.0.3)3> client:allocate().
{ok,10}
(client@172.17.0.3)4> client:allocate().
{ok,11}
(client@172.17.0.3)5> client:deallocate(10).
ok
(client@172.17.0.3)6> 

```
