#!/usr/bin/env python2
# -*- coding: utf-8 -*-

import yaml
import os
import sys
import paramiko
import socket
import time
import logging


def get_cluster_config_file():
    '''Returns the file used as ERLANG CLUSTER architecture'''
    try:
        ret = os.environ["ERLANG_CLUSTER"]
    except KeyError:
        logging.debug(
            "[Ansible] Please set environment variable ERLANG_CLUSTER")
        sys.exit(1)

    return "config/cluster-" + ret + ".yaml"


def get_cluster_config_yml():
    '''Returns a list with hostname and IP'''
    with open(get_cluster_config_file(), 'r') as f:
        ret = yaml.load(f)

    return ret


def check_ssh(ip, user, key_file, initial_wait=0, interval=0, retries=1):
    logging.debug("[Ansible] checking SSH availability for %s", ip)
    ssh = paramiko.SSHClient()
    ssh.set_missing_host_key_policy(paramiko.AutoAddPolicy())

    time.sleep(initial_wait)

    for x in range(retries):
        try:
            ssh.connect(ip, username=user, key_filename=key_file)
            return True
        except (paramiko.BadHostKeyException,
                paramiko.AuthenticationException,
                paramiko.SSHException,
                socket.error) as e:
            logging.debug(e)
            time.sleep(interval)

    return False


def check_ssh_available(cluster_yml):
    ssh_key_path = os.environ["HOME"] + "/.vagrant.d/insecure_private_key"

    # check if the hosts are ssh accesibles
    for item in cluster_yml:
        if check_ssh(item.get('ip'),
                     "vagrant",
                     ssh_key_path,
                     2, 2, 3):
            logging.debug(
                "[Ansible] %s: SSH is OK for provisioning", item)
        else:
            logging.debug("[Ansible] %s: SSH not ready", item)
            return False

    return True


logging.basicConfig(filename='.vagrant/inventory.log', level=logging.DEBUG)

all_vm_accesibles = False
logging.debug('[Ansible] getting host list from configuration')
cluster_yml = get_cluster_config_yml()


logging.debug("[Ansible] Sanity check loop for Ansible hosts")
while not all_vm_accesibles:
    logging.debug("[Ansible] Waiting for SSH to become available in all hosts")
    if check_ssh_available(cluster_yml):
        all_vm_accesibles = True


class InventoryTemplate:
    '''
    {
    "all": {
        "hosts": ["build", "erlang-n1","erlang-n2","erlang-n3"],
        "vars": {
                    "ansible_user": "vagrant",
                    "ansible_become": "true"
                }
    },
    "build": {
        "hosts": ["build"]
    },
    "erlang-nodes": {
        "hosts": ["erlang-n1", "erlang-n2", "erlang-n3"]
    },
    "_meta": {
        "hostvars": {
               "build": {"ansible_host": "192.168.121.248"},
               "erlang-n1": {"ansible_host": "192.168.121.248"},
               "erlang-n2": {"ansible_host": "192.168.121.112"},
               "erlang-n3": {"ansible_host": "192.168.121.80"}
            }
        }
    }
    '''

    _template = """
    {
        "all": {
            "hosts": [%(_get_all|_pattern_a)s],
            "vars": {
                "ansible_user": "vagrant",
                "ansible_become": "true"
            }
        },
        "build": {
            "hosts": ["build"]
        },
        "erlang-nodes": {
            "hosts": [%(_get_nodes|_pattern_a)s],
        },
        "_meta": {
            "hostvars": {
                %(_get_all_hostip|_pattern_b)s
            }
        }
    }
    """

    def __init__(self, dict={}):
        self.dict = dict

    def __str__(self):
        return self._template % self

    def __getitem__(self, key):
        return self._process(key.split("|"))

    def _process(self, l):
        arg = l[0]
        if len(l) == 1:
            if arg in self.dict:
                return self.dict[arg]
            elif hasattr(self, arg) and callable(getattr(self, arg)):
                return getattr(self, arg)()
            else:
                raise KeyError(arg)
        else:
            func = l[1]
            return getattr(self, func)(self._process([arg]))

    def _get_all(self):
        cad = []
        for i in cluster_yml:
            cad.append(i.get('name'))
        return cad

    def _get_nodes(self):
        cad = []
        for i in cluster_yml:
            if i.get('type') == 'erlang-node':
                cad.append(i.get('name'))
        return cad

    def _get_all_hostip(self):
        cad = []
        for i in cluster_yml:
            cad.append("\"%s\": {\"ansible_host\": \"%s\"}" % (i.get('name'),
                                                               i.get('ip')))
        return cad

    def _pattern_a(self, l):
        return ",".join(["\"%s\"" % x for x in l])

    def _pattern_b(self, l):
        return ",".join(["%s" % x for x in l])


print InventoryTemplate()
