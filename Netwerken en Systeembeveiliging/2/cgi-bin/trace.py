#!/usr/bin/python

import os
import urlparse
import subprocess

try:
    query = dict(urlparse.parse_qsl(os.environ['QUERY_STRING']))
    print subprocess.check_output(['traceroute ' + query['ip']], shell=True)
except:
    print "Something went wrong. Did you specified an IP?"
