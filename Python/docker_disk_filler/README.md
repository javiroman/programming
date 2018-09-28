# Build the Docker application

```
docker build . -t disk_filler
```

# Run the Docker application

```
docker run  -d -p 8080:8080 --name filler disk_filler
```

# Login into the Docker container

```
docker run --rm -it -p 8080:8080 --name filler disk_filler sh
or
docker exec -it filler sh
```

# Managing the application

```
export CONTAINER_IP=172.17.0.2

Fill disk: curl $CONTAINER_IP>:8080/fill?size=2
Stop filling: curl $CONTAINER_IP:8080/stop
```

# Copying manually the Docker image to Vagrant DC/OS nodes

```
$ docker save disk_filler | bzip2 | pv | ssh -i /home/user/.vagrant.d/insecure_private_key vagrant@192.168.121.219 'bunzip2 | sudo docker load'
```

# Working with DC/OS Marathon

```
$ dcos marathon app add filler-ephimeral-docker.json
$ dcos marathon app list
ID                             MEM  CPUS  TASKS  HEALTH  DEPLOYMENT  WAITING CONTAINER  CMD  
/disk-filler-ephimeral-docker   32  0.25   1/1    N/A       ---      False   DOCKER     N/A

$ curl filler-docker.marathon.l4lb.thisdcos.directory:1234/fill?size=2
JID-1538137537.680487.3040
$ curl filler-docker.marathon.l4lb.thisdcos.directory:1234/stop
JID-1538137548.5942774.2521
```

Notes about Mesos UCR containers with native Docker images:

```
# cat /opt/mesosphere/etc/mesos-slave-common
[...]
MESOS_DOCKER_REGISTRY=/opt/
[...]
```

If the --docker_registry agent flag points to a local directory (e.g.,
/tmp/mesos/images/docker), the provisioner will pull Docker images from local
filesystem, assuming Docker archives (result of docker save) are stored there
based on the image name and tag. For example, the operator can put a
busybox:latest.tar (the result of docker save -o busybox:latest.tar busybox)
under /tmp/mesos/images/docker and launch the agent by specifying
--docker_registry=/tmp/mesos/images/docker. Then the framework can launch a
Docker container by specifying busybox:latest as the name of the Docker image.

Note that this option won't change the default registry server for Docker
containerizer. (default: https://registry-1.docker.io)

# Working with PVs in Apache Mesos

```
curl -XPOST http://leader.mesos:5050/master/reserve -d slaveId="6e8240cd-ca35-427f-a55c-1b31e55eba4c-S3" -H "Content-Type: application/json" -d resources='[{"name": "cpus", "type": "SCALAR", "scalar": { "value": 0.5 }, "role": "test" }]' -vvv

curl -XPOST http://leader.mesos:5050/master/reserve -d slaveId="6e8240cd-ca35-427f-a55c-1b31e55eba4c-S3" -H "Content-Type: application/json" -d resources='[{"name": "cpus", "type": "SCALAR", "scalar": { "value": 0.5 }, "role": "test"}, "reservation": {"principal": "open"}]' -vvv

curl -XPOST http://leader.mesos:5050/master/reserve -d slaveId="6e8240cd-ca35-427f-a55c-1b31e55eba4c-S3" -H "Content-Type: application/json" -d resources='[{"name": "cpus", "type": "SCALAR", "scalar": { "value": 0.5 }, "role": "test", "reservation": {"principal": "open"}}]' -vvv

curl -d @quota-test-role.json -X POST http://leader.mesos:5050/quota -vvv

curl -XPOST http://leader.mesos:5050/master/unreserve -d slaveId="6e8240cd-ca35-427f-a55c-1b31e55eba4c-S3" -H "Content-Type: application/json" -d resources='[{"name": "cpus", "type": "SCALAR", "scalar": { "value": 0.5 }, "role": "test", "reservation": {"principal": "open"}}]' -vvv

curl -X DELETE http://leader.mesos:5050/quota/test -vvv

curl -XPOST http://leader.mesos:5050/master/unreserve -d slaveId="6e8240cd-ca35-427f-a55c-1b31e55eba4c-S3" -H "Content-Type: application/json" -d resources='[{"name": "cpus", "type": "SCALAR", "scalar": { "value": 0.5 }, "role": "test", "reservation": {"principal": "open"}}]' -vvv

curl -XPOST http://leader.mesos:5050/master/reserve -d slaveId="6e8240cd-ca35-427f-a55c-1b31e55eba4c-S3" -H "Content-Type: application/json" -d resources='[{"name": "cpus", "type": "SCALAR", "scalar": { "value": 0.5 }, "role": "test", "reservation": {"principal": "open"}}]' -vvv

curl -d @quota-test-role.json -X POST http://leader.mesos:5050/quota -vvv
curl -X DELETE http://leader.mesos:5050/quota/test -vvv
curl -XPOST http://leader.mesos:5050/master/unreserve -d slaveId="6e8240cd-ca35-427f-a55c-1b31e55eba4c-S3" -H "Content-Type: application/json" -d resources='[{"name": "cpus", "type": "SCALAR", "scalar": { "value": 0.5 }, "role": "test", "reservation": {"principal": "open"}}]' -vvv
```

