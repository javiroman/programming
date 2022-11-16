package main

import (
	"fmt"
	"os"
	"strconv"
)

// working with raw string literals
var zkHosts = ``

func main() {
	replicas, _ := strconv.Atoi(os.Args[1])
	//hostname := `zookeeper-sample-`
	hostname := os.Args[2]
	//namespace := `default`
	namespace := os.Args[3]

	zkEnvPartial := `
export ZOOKEEPER__zoocfg__logLevel="INFO"
export ZOOKEEPER__zoocfg__4lw_commands_whitelist="*"
    `

	for i := 0; i < replicas; i++ {
		literal := `export ZOOKEEPER__zoocfg__server_` +
			strconv.Itoa(i+1) +
			`=` + `"` +
			hostname + `-` +
			strconv.Itoa(i) + `.zk-hs.` +
			namespace +
			`.svc.cluster.local:2888:3888` + `"`

		zkHosts = zkHosts + "\n" + literal
		//fmt.Println(literal)
	}

	zkEnv := zkEnvPartial + zkHosts
	fmt.Println(zkEnv)
}
