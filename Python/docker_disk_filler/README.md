# Build Docker application

docker build . -t disk_filler

# Run Docker application

docker run  -d -p 8080:8080 --name filler disk_filler

# Login into Docker application

docker run --rm -it -p 8080:8080 --name filler disk_filler sh
or
docker exec -it filler sh

# Manage application

export CONTAINER_IP=172.17.0.2

Fill disk: curl $CONTAINER_IP>:8080/fill?size=2
Stop filling: curl $CONTAINER_IP:8080/stop
