echo
echo "This Vagrant environment is ready for the following settings:"
echo

[ -z $ERLANG_CLUSTER ] || echo "- ERLANG_CLUSTER : $ERLANG_CLUSTER"

echo
echo "'vagrant up --provider=libvirt' and happy hacking!"
