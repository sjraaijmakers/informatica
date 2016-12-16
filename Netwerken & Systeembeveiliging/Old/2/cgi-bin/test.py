#!/usr/bin/python

import os

# # Header
# print "Connection: closed\r\n",
# print "Content-Length: \r\n",
# print "Content-Type: text/plain\r\n\r\n"

# Environs
print "DOCUMENT_ROOT:", os.environ['DOCUMENT_ROOT']
print "REQUEST_METHOD:", os.environ['REQUEST_METHOD']
print "REQUEST_URI:", os.environ['REQUEST_URI']
print "QUERY_STRING:", os.environ['QUERY_STRING']
print "REMOTE_ADDR:", os.environ['REMOTE_ADDR']
print "PATH:", os.environ['PATH']
