cat << EOF | sudo tee /etc/systemd/system/impala.service
[Unit]
Description=Apache Impala Daemon
Documentation=http://impala.apache.org

[Service]
EnvironmentFile=/opt/impala/conf/impala.env
ExecStart=/opt/impala/sbin/impalad --flagfile=/opt/impala/conf/impala.gflagfile
TimeoutStopSec=5
Restart=on-failure
User=impala

[Install]
WantedBy=multi-user.target
EOF

cat << EOF | sudo tee /etc/systemd/system/impala-catalog.service
[Unit]
Description=Apache Impala Catalog Daemon
Documentation=http://impala.apache.org

[Service]
EnvironmentFile=/opt/impala/conf/impala.env
ExecStart=/opt/impala/sbin/catalogd --flagfile=/opt/impala/conf/catalog.gflagfile
TimeoutStopSec=5
Restart=on-failure
User=impala

[Install]
WantedBy=multi-user.target
EOF

cat << EOF | sudo tee /etc/systemd/system/impala-statestore.service
[Unit]
Description=Apache Impala StateStore Daemon
Documentation=http://impala.apache.org

[Service]
EnvironmentFile=/opt/impala/conf/impala.env
ExecStart=/opt/impala/sbin/statestored --flagfile=/opt/impala/conf/statestore.gflagfile
TimeoutStopSec=5
Restart=on-failure
User=impala

[Install]
WantedBy=multi-user.target
EOF

cat << EOF | sudo tee /etc/systemd/system/impala-admission.service
[Unit]
Description=Apache Impala Admission Control Daemon
Documentation=http://impala.apache.org

[Service]
EnvironmentFile=/opt/impala/conf/impala.env
ExecStart=/opt/impala/sbin/admissiond --flagfile=/opt/impala/conf/admission.gflagfile
TimeoutStopSec=5
Restart=on-failure
User=impala

[Install]
WantedBy=multi-user.target
EOF

cat << EOF | sudo tee /opt/impala/conf/impala.env
IMPALA_HOME=/opt/impala
JAVA_HOME=/usr/lib/jvm/java/
CLASSPATH=/opt/impala/lib/*:/opt/hive/lib/*
HADOOP_HOME=/opt/hadoop
HIVE_HOME=/opt/hive
HIVE_CONF=/opt/hive/conf
EOF

sudo chown impala: /opt/impala/conf/impala.env

cat << EOF | sudo tee /opt/impala/conf/impala.gflagfile
--abort_on_config_error=false
--log_dir=/var/log/impala
--state_store_host=kudu-master1.node.keedio.cloud
--catalog_service_host=kudu-master1.node.keedio.cloud
--admission_service_host=kudu-master1.node.keedio.cloud
--kudu_master_hosts=kudu-master1.node.keedio.cloud
--enable_legacy_avx_support=true
EOF

sudo chown impala: /opt/impala/conf/impala.gflagfile

cat << EOF | sudo tee /opt/impala/conf/catalog.gflagfile
--kudu_master_hosts=kudu-master1.node.keedio.cloud
--log_dir=/var/log/impala
--enable_legacy_avx_support=true
EOF

sudo chown impala: /opt/impala/conf/catalog.gflagfile

cat << EOF | sudo tee /opt/impala/conf/statestore.gflagfile
--kudu_master_hosts=kudu-master1.node.keedio.cloud
--log_dir=/var/log/impala
--enable_legacy_avx_support=true
EOF

sudo chown impala: /opt/impala/conf/statestore.gflagfile

cat << EOF | sudo tee /opt/impala/conf/admission.gflagfile
--kudu_master_hosts=kudu-master1.node.keedio.cloud
--log_dir=/var/log/impala
--enable_legacy_avx_support=true
EOF

sudo chown impala: /opt/impala/conf/admission.gflagfile

cat << EOF | sudo tee /etc/systemd/system/metastore.service 
[Unit]
Description=Apache Hive Metastore - HSM
Documentation=http://hive.apache.org

[Service]
Type=simple
Environment=JAVA_HOME=/usr/lib/jvm/java/
Environment=HADOOP_HOME=/opt/hadoop
ExecStart=/opt/hive/bin/hive --service metastore --hiveconf hive.root.logger=DEBUG,console
TimeoutStopSec=5
Restart=on-failure
User=hive

[Install]
WantedBy=multi-user.target
EOF

cat << EOF | sudo tee /opt/hive/conf/hive-site.xml
<configuration>
  <property>
    <name>hive.metastore.uris</name>
    <value>thrift://kudu-master1.node.keedio.cloud:9083</value>
    <description>Thrift URI for the remote metastore. 
      Used by metastore client to connect to remote metastore.
    </description>
  </property>

  <property>
    <name>hive.metastore.warehouse.dir</name>
    <value>/var/lib/hive/warehouse</value>
    <description>location of default database for the warehouse</description>
  </property>

  <property>
    <name>metastore.task.threads.always</name>
    <value>org.apache.hadoop.hive.metastore.events.EventCleanerTask</value>
  </property>

  <property>
    <name>metastore.expression.proxy</name>
    <value>org.apache.hadoop.hive.metastore.DefaultPartitionExpressionProxy</value>
  </property>

  <property>
    <name>javax.jdo.option.ConnectionDriverName</name>
    <value>org.postgresql.Driver</value>
  </property>

  <property>
    <name>javax.jdo.option.ConnectionURL</name>
    <value>jdbc:postgresql://kudu-master1.node.keedio.cloud:5432/metastore</value>
  </property>

  <property>
    <name>javax.jdo.option.ConnectionUserName</name>
    <value>postgres</value>
  </property>

  <property>
    <name>javax.jdo.option.ConnectionPassword</name>
    <value>postgres</value>
  </property> 

  <property>
    <name>hive.metastore.transactional.event.listeners</name>
    <description>
      KEY connection with KUDU from (artifact from Kudu release): hms-plugin.jar
    </description>
    <value>      
      org.apache.hive.hcatalog.listener.DbNotificationListener,
      org.apache.kudu.hive.metastore.KuduMetastorePlugin
    </value> 
  </property> 

  <property>
    <name>hive.metastore.disallow.incompatible.col.type.changes</name>
    <value>false</value>
  </property>

  <property>
      <!-- Required for automatic metadata sync. -->
      <name>hive.metastore.dml.events</name>
      <value>true</value>
  </property>

  <property>
     <name>hive.metastore.event.db.notification.api.auth</name>
     <value>false</value>
     <description>
       Should metastore do authorization against database notification 
       related APIs such as get_next_notification.
       If set to true, then only the superusers in proxy settings have the permission
     </description>
  </property>
</configuration>
EOF

cat << EOF | sudo tee /opt/hadoop/etc/hadoop/core-site.xml
<configuration>
  <property>
    <name>dfs.namenode.name.dir</name>
    <value>/cluster/nn</value>
  </property>

  <property>
    <name>dfs.datanode.data.dir</name>
    <value>/cluster/1/dn,/cluster/2/dn</value>
  </property>

  <property>
    <name>dfs.replication</name>
    <value>3</value>
  </property>
</configuration>
EOF

cat << EOF | sudo tee /opt/hadoop/etc/hadoop/hdfs-site.xml
<configuration>
 <property>
    <name>fs.default.name</name>
    <value>file://var/lib/hive</value>
    <description>fake hdfs</description>
 </property> 
</configuration>
EOF

