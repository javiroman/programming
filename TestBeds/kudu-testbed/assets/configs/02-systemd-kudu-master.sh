cat << EOF | sudo tee /etc/systemd/system/kudu-master.service 
[Unit]                                                                                                                                            
Description=Apache Kudu Master Server
Documentation=http://kudu.apache.org 

[Service]
Environment=KUDU_HOME=/var/lib/kudu
ExecStart=/opt/kudu/usr/local/sbin/kudu-master --flagfile=/opt/kudu/conf/master.gflagfile
TimeoutStopSec=5
Restart=on-failure
User=kudu

[Install]
WantedBy=multi-user.target
EOF
