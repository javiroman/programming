# coding: utf8
from atlassian import Jira
import os
import logging


log = logging.getLogger()
log.setLevel(logging.DEBUG)

JQL = 'project = PI AND resolution = Unresolved ORDER BY priority DESC, updated DESC'

JIRA_URL = os.environ.get('JIRA_URL', 'http://localhost:8080')
JIRA_USER = os.environ.get('JIRA_USER', 'user')
JIRA_PASSWORD = os.environ.get('JIRA_PASSWORD', 'password')

print(JIRA_URL)
print(JIRA_USER)
print(JIRA_PASSWORD)

jira = Jira(
    url=JIRA_URL,
    username=JIRA_USER,
    password=JIRA_PASSWORD,
    verify_ssl=False)

data = jira.jql(JQL)
print(data)