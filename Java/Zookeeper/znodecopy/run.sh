java -cp "build/libs/*"  org.apache.zookeeper.server.auth.DigestAuthenticationProvider vault:jos

export JVMFLAGS=-Dzookeeper.DigestAuthenticationProvider.superDigest=vault:ooJ6dbCyFt6Zgr33kiLUoCCLiRM=

java -cp "build/libs/*"  org.example.zk.ZKCreate $1 $2
