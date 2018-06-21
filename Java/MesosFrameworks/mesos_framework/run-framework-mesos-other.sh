MESOS_SOURCE_DIR=/opt/mesos
MESOS_BUILD_DIR=${MESOS_SOURCE_DIR}/build
PROTOBUF_JAR=${MESOS_BUILD_DIR}/src/java/target/protobuf-java-3.5.0.jar
JAVA_HOME=/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.171-7.b10.el7.x86_64/
JAVA=${JAVA-${JAVA_HOME}/bin/java}
#### MESOS_JAR=${MESOS_BUILD_DIR}/src/java/target/mesos-1.5.0.jar
MESOS_JAR=/opt/hadoop/share/hadoop/yarn/lib/mesos-1.5.0.jar
EXAMPLES_JAR=./build/libs/example-framework-1.0-SNAPSHOT.jar

# sh run-framework.sh mesos-m1 launch-executor.cmd
# sh run-framework.sh 10.200.1.84 launch-executor.cmd
exec ${JAVA} -cp ${PROTOBUF_JAR}:${MESOS_JAR}:${EXAMPLES_JAR} \
    -Djava.library.path=${MESOS_BUILD_DIR}/src/.libs \
    org.opencredo.mesos.MainRunner \
    zk://${1}:2181/mesos $2

#    -agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005 \
