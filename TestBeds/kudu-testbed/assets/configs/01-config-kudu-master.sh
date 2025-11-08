cat << EOF | sudo tee /opt/kudu/conf/master.gflagfile
--webserver_doc_root=/opt/kudu/usr/local/www
--log_dir=/var/log/kudu
--fs_wal_dir=/var/lib/kudu/wal
--fs_data_dirs=/var/lib/kudu/data
--rpc_encryption=optional
--rpc_authentication=optional
--rpc_negotiation_timeout_ms=5000
--hive_metastore_uris=thrift://kudu-master1.node.keedio.cloud:9083
EOF

sudo chown kudu: /opt/kudu/conf/master.gflagfile
