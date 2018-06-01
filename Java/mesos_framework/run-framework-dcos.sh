#MESOS_SOURCE_DIR=/opt/mesos
#MESOS_BUILD_DIR=${MESOS_SOURCE_DIR}/build
#PROTOBUF_JAR=${MESOS_BUILD_DIR}/src/java/target/protobuf-java-3.5.0.jar
#PROTOBUF_JAR=/opt/mesosphere/lib/mesos-1.5.0-shaded-protobuf.jar
PROTOBUF_JAR=/opt/mesosphere/lib/com.google.protobuf.protobuf-java-3.3.0.jar
#JAVA_HOME=/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.171-7.b10.el7.x86_64/
JAVA_HOME=/opt/mesosphere/packages/java--5e8781ced77cfcbada2912e209374e96a9edc2db/usr/java/
#JAVA=${JAVA-${JAVA_HOME}/bin/java}
JAVA=/bin/java
#MESOS_JAR=${MESOS_BUILD_DIR}/src/java/target/mesos-1.5.0.jar
MESOS_JAR=/opt/mesosphere/lib/mesos-1.5.0.jar
EXAMPLES_JAR=./build/libs/example-framework-1.0-SNAPSHOT.jar

export MESOS_AUTHENTICATE=1
export DEFAULT_PRINCIPAL=open
export DEFAULT_SECRET=DrE6o5r67qnf38H9K5KgGhoeRHqwU6sVNBVStHqw

# sh run-framework.sh mesos-m1 command-01
# sh run-framework.sh 10.200.1.84 command-01
exec ${JAVA} -cp ${PROTOBUF_JAR}:${MESOS_JAR}:${EXAMPLES_JAR} \
    -Djava.library.path=/opt/mesosphere/lib/ \
    org.opencredo.mesos.MainRunner \
    zk://${1}:2181/mesos $2
