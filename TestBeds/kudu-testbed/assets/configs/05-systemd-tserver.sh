cat << EOF | sudo tee /etc/systemd/system/kudu-tserver.service 
[Unit]                                                                                                                                            
Description=Apache Kudu Tablet Server
Documentation=http://kudu.apache.org 

[Service]
Environment=KUDU_HOME=/var/lib/kudu
ExecStart=/opt/kudu/usr/local/sbin/kudu-tserver --flagfile=/opt/kudu/conf/tserver.gflagfile
TimeoutStopSec=5
Restart=on-failure
User=kudu

[Install]
WantedBy=multi-user.target
EOF
