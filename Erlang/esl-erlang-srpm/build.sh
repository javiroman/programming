#!/bin/bash

git clone https://github.com/erlang/otp
cd otp
git checkout 17.5
mkdir -p rpmbuild/{RPMS,SOURCES,SPECS,SRPMS,BUILD,BUILDROOT/esl-erlang-17.5-1.x86_64}
chmod 777 -R rpmbuild
cp ../erlang.spec rpmbuild/SPECS/erlang.spec

sed -i "s#@ARCH@#x86_64#" rpmbuild/SPECS/erlang.spec

sudo yum-builddep rpmbuild/SPECS/erlang.spec -y

./otp_build autoconf
./otp_build configure
./otp_build boot -a
./otp_build release -a

mkdir -p erlang/usr/lib/erlang erlang/usr/bin erlang/usr/etc erlang/usr/share
cp -r release/*/* erlang/usr/lib/erlang/
pushd erlang
LINKDIR="../lib/erlang/"
ln -s ${LINKDIR}/bin/{ct_run,dialyzer,epmd,erl,erlc,escript,run_erl,run_test,start_erl,to_erl,typer} usr/bin/
ln -s ${LINKDIR}/lib/{os_mon-*/priv/bin/cpu_sup,erl_interface-*/bin/erl_call} usr/bin/
ln -s ${LINKDIR}/lib/{observer-*/priv/bin/etop,observer-*/priv/bin/getop} usr/bin/
ln -s ${LINKDIR}/lib/{erlang/erts-*/bin/heart,os_mon-*/priv/bin/memsup,webtool-*/priv/bin/start_webtool} usr/bin/
popd

rm -rf erlang/usr/lib/erlang/lib/*/doc
mkdir -p erlang/usr/share/doc/esl-erlang/

set +e
for i in `find . -wholename '*/bin/*'`; do
    objdump -G ${i}
    RETVAL=$?
    [ $RETVAL -eq 0 ] && strip ${i}
done
set -e
find . -iname '*.so' -exec strip --strip-unneeded {} \;

sudo chrpath -d erlang/usr/lib/erlang/lib/crypto-*/priv/lib/crypto.so
sudo rm -rf erlang/usr/etc
sudo find erlang/ -name '.*' -delete
rm -rf erlang/usr/lib/erlang/man/cat*

export ERL_TOP=`pwd`

mv erlang/usr rpmbuild/BUILDROOT/esl-erlang-17.5-1.x86_64/usr

rpmbuild -bb rpmbuild/SPECS/erlang.spec
