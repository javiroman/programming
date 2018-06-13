#!/usr/bin/env bash

MESOS_SOURCE_DIR=/opt/mesos
MESOS_BUILD_DIR=${MESOS_SOURCE_DIR}/build
JAVA_HOME=/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.171-7.b10.el7.x86_64/

JAVA=${JAVA-${JAVA_HOME}/bin/java}

# Use colors for errors.
. ${MESOS_SOURCE_DIR}/support/colors.sh

PROTOBUF_JAR=/opt/mesos/build/src/java/target/protobuf-java-3.5.0.jar

test ! -e ${PROTOBUF_JAR} && \
  echo "${RED}Failed to find ${PROTOBUF_JAR}${NORMAL}" && \
  exit 1

MESOS_JAR=${MESOS_BUILD_DIR}/src/java/target/mesos-1.5.0.jar

test ! -e ${MESOS_JAR} && \
  echo "${RED}Failed to find ${MESOS_JAR}${NORMAL}" && \
  exit 1

EXAMPLES_JAR=./build/libs/mesos_framework_examples-1.0-SNAPSHOT.jar

test ! -e ${EXAMPLES_JAR} && \
  echo "${RED}Failed to find ${EXAMPLES_JAR}${NORMAL}" && \
  exit 1

# Need to run in the directory containing this script so that the
# framework is able to find the executor.
cd `dirname ${0}`

# Example: sh test-framework.sh zk://10.200.1.84:2181/mesos
exec ${JAVA} -cp ${PROTOBUF_JAR}:${MESOS_JAR}:${EXAMPLES_JAR} \
  -Djava.library.path=${MESOS_BUILD_DIR}/src/.libs \
  TestFramework "${@}"
