```
Erlang/OTP 19 [erts-8.3.5.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V8.3.5.1  (abort with ^G)
1> c(frequency).
{ok,frequency}
2> frequency:start().
Main server loop running ...
true
3> frequency:allocate().
Main server loop running ...
{ok,10}
4> frequency:show().    
Main server loop running ...
{[11,12,13,14,15],[{10,<0.57.0>}]}
5> frequency:allocate().
Main server loop running ...
{ok,11}
6> frequency:show().    
Main server loop running ...
{[12,13,14,15],[{11,<0.57.0>},{10,<0.57.0>}]}
7> frequency:deallocate(10).
Main server loop running ...
ok
8> frequency:show().        
Main server loop running ...
{[10,12,13,14,15],[{11,<0.57.0>}]}
9> 
```

