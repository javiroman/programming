sudo mkdir /opt/kudu/conf

cat << EOF | sudo tee /opt/kudu/conf/tserver.gflagfile
--tserver_master_addrs=http://kudu-master1.node.keedio.cloud:7051
--webserver_doc_root=/opt/kudu/usr/local/www
--log_dir=/var/log/kudu
--fs_wal_dir=/var/lib/kudu/wal
--fs_data_dirs=/var/lib/kudu/data 
--rpc_encryption=optional
--rpc_authentication=optional
--rpc_negotiation_timeout_ms=5000
EOF

sudo chown kudu: /opt/kudu/conf/tserver.gflagfile
