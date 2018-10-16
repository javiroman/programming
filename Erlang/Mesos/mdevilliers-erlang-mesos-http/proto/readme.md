Upgrading the {...}.proto files
--------------------------------


1. Copy proto files from /usr/include/mesos or from https://github.com/apache/mesos/tree/master/include/mesos/v1
2. In scheduler.proto and mesos.proto change

```
import "mesos/v1/mesos.proto";
```

to

```
import "mesos.proto";
```

3. Run make generate-proto-bindings
