#!/bin/env python3

import sys
import requests
import json
import os
import nss.io as io
import nss.nss as nss
import nss.ssl as ssl
from requests import HTTPError
from nss.error import NSPRError


# Fixed API endpoints
API_POINT_SLAVES = "/slaves"
API_POINT_UNRESERVE = "/master/unreserve"
API_POINT_DESTROY = "/master/destroy-volumes"
SLAVE_ID_STRING = "slaveId"


class Unreserve():

    def __init__(self, argv):
        self.argv = argv
        if argv[0] == "help":
            print("Usage: python3 curling_main.py ")
            print("<LEADER_IP> <FW_ROLE> <FW_PRINCIPAL> <FW_PRINCIPAL_SECRET>")
            print()
            print("Principal/secret locted at master:")
            print("/opt/mesosphere/etc/mesos/security/credentials.json")
            exit(0)

    def test_nss(self):
        '''References:
        github.com/tiran/python-nss/blob/master/doc/examples/ssl_example.py
        - Get certificates
        certutil -L -d sql:$HOME/.pki/nssdb/
        - Get one certificate by nickname
        certutil -L -d sql:$HOME/.pki/nssdb/ -a -n my-nickname
        - Rename nickname of certificate
        certutil --rename -n my-nicname --new-n my-new-nicname
            -d sql:/home/javierroman/.pki/nssdb/
        - Change NSS db password (new empty erase password)
        certutil -W -d sql:$HOME/.pki/nssdb/ -f old.txt -@new.txt
        '''
        print("called query")
        db_name = 'sql:' + os.getenv('HOME') + '/.pki/nssdb'
        nss.nss_init(db_name)

    def query_reserves(self):
        print("called query")

    def run_unreserve(self):
        '''
        Examples:
            leader_url = "http://10.200.0.21:5050"
            role = "postgrestls"
            principal = "open"
            secret = "DrE6o5r67qnf38H9K5KgGhoeRHqwU6sVNBVStHqw"
        '''
        leader_url = "http://" + args[0] + ":5050"
        role = args[1]
        principal = args[2]
        secret = args[3]
#
#        # Get offers through /slaves
#        slaves_res = requests.get(leader_url + API_POINT_SLAVES)
#        json_res = slaves_res.json()
#        print ("Step 1: Unreserve resources and destroy volumes " +
#               "for disks resources")
#        print ("Node & reserved resources for " + role + ":\n")
#
#        for x in json_res["slaves"]:
#            # Node ID
#            id = x["id"]
#            print("-------------------------------------------")
#            print(id)
#
#            if role in x["reserved_resources_full"]:
#                # Reserved resources associated to a specific role
#                role_resources = x["reserved_resources_full"][role]
#                # Uncomment to see JSON with reserved resources
#                # print (json.dumps(role_resources, indent=4, sort_keys=True))
#
#                for r in role_resources:
#                    # Curls needed to unreserve resources
#                    if r["name"] != "disk":
#                        # For resources not being "disk"
#                        # Following lines only print the curls
#                        print ("Resource: " + r["name"] +
#                               " -> Se puede dereservar con:")
#                        print ("curl -i -u " + "\"" + principal +
#                               "\":\"" + secret + "\" -d slaveID=" + "\"" +
#                               id + "\"" + " -d resources='[" + json.dumps(r) +
#                               "]' -X POST " + leader_url + api_point_unreserve)
#
#                        # Following lines launch the curls
#                        # Data containing Slave ID and resource to unreserve
#                        data = {slave_ID_string: id,
#                                "resources": '['+json.dumps(r)+']'}
#
#                        unreserve_res = \
#                            requests.post(leader_url
#                                          + api_point_unreserve,
#                                          auth=(principal, secret), data=(data))
#
#                        try:
#                            unreserve_res.raise_for_status()
#                        except HTTPError as err:
#                            print(err)
#                            print(unreserve_res.text)
#                        else:
#                            print (unreserve_res.status_code)
#                            print (unreserve_res.text)
#                            print()
#
#                    else:
#                        # For disk resources, we must first destroy volumes
#                        # Following lines only print the curls
#                        print("Resource: " + r["name"] +
#                              " -> NO se puede dereservar.")
#                        print("Primero hay que destruir volumenes con:")
#                        print("curl -i -u " +
#                              "\"principal\":\"secret\" -d slaveID=" + "\"" +
#                              id + "\"" + "-d volumes='[" + json.dumps(r) +
#                              "]' -X POST " + leader_url + api_point_destroy)
#
#                        # Following lines launch the curls
#                        # Data containing Slave ID and resource to unreserve
#                        data = {slave_ID_string: id, "volumes": '[' +
#                                json.dumps(r) + ']'}
#                        destroy_res = requests.post(leader_url +
#                                                    api_point_destroy,
#                                                    auth=(principal, secret),
#                                                    data=(data))
#
#                        try:
#                            destroy_res.raise_for_status()
#                        except HTTPError as err:
#                            print(err)
#                            print(destroy_res.text)
#                        else:
#                            print(destroy_res.status_code)
#                            print(destroy_res.text)
#
#                        # Ahora debemos volver a coger las reservas de /slaves!
#                        print()
#
#            else:
#                print ("No resources for " + role)
#                print ()
#
#        # Now we get the reservations again, as disk resources
#        # should be destroyed by the previous for
#        # Get offers through /slaves
#        slaves_res = requests.get(leader_url + api_point_slaves)
#        json_res = slaves_res.json()
#        print("***********************************")
#        print("Step 2: Unreserve the previously destroyed disk resources")
#        print("Node & reserved resources for " + role + ":\n")
#        for x in json_res["slaves"]:
#            # Node ID
#            id = x["id"]
#            print("-------------------------------------------")
#            print(id)
#
#            if role in x["reserved_resources_full"]:
#                # Reserved resources associated to a specific role
#                role_resources = x["reserved_resources_full"][role]
#                # ---> Uncomment to see JSON with reserved resources
#                # print (json.dumps(role_resources, indent=4, sort_keys=True))
#
#                for r in role_resources:
#
#                    if r["name"] != "disk":
#
#                        print("Something is going wrong, there shouldn't be any resources other than disk here")
#                        print()
#
#                    else:
#
#
#                        # Following lines only print the curls
#                        print("Y ahora de-reservar el disco:")
#                        print("curl -i -u " + "\"principal\":\"secret\" -d slaveID=" + "\"" +
#                            id + "\"" + "-d resources='[" + json.dumps(
#                                r) + "]' -X POST " + leader_url + api_point_unreserve)
#
#
#                        # Following lines launch the curls
#                        # Data containing Slave ID and resource to unreserve
#                        data = {slave_ID_string: id, "resources": '[' + json.dumps(r) + ']'}
#                        unreserve_res = requests.post(leader_url + api_point_unreserve, auth=(principal, secret), data=(data))
#
#                        try:
#                            unreserve_res.raise_for_status()
#                        except HTTPError as err:
#                            print(err)
#                            print(unreserve_res.text)
#            else:
#                print(unreserve_res.status_code)
#                print(unreserve_res.text)
#                print()
#
#            else:
#                print ("No resources for " + role)
#                print ()
#

    def run(self):
        # self.run_unreserve()
        self.test_nss()


def main(argv=sys.argv[1:]):
    u = Unreserve(argv)
    return u.run()


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
