# Steven Raaijmakers, 10804242
# Script parses emails to acceptable riak format

from riak import RiakClient, RiakNode

myClient = RiakClient(pb_port=10017, protocol='pbc')

import os

def p2r(p):
    print p
    # create new bucket for person
    myBucket = myClient.bucket(p)

    folders = os.listdir("maildir/" + str(p))
    # key
    for f in folders:
        # print "\t" + str(f)
        emails ="maildir/" + str(p) + "/" + str(f)

        emails_d = {}
        if os.path.isdir(emails):
            emails = os.listdir(emails)
            di = {}
            for e in emails:
                fl = "maildir/" + str(p) + "/" + str(f) + "/" + str(e)
                if not os.path.isdir(fl):
                    # print "\t\t" + str(e)
                    file = open("maildir/" + str(p) + "/" + str(f) + "/" + str(e), "r")
                    string = file.read()
                    try:
                        unicode(string, 'utf-8')
                        di[e] = string
                    except UnicodeError:
                        print "\t\t\t(skipped: " + str(e) + ")"


        key = myBucket.new(f, data=di)
        key.store()
    # print "added " + str(p)

persons = os.listdir("maildir")
for p in persons:
    p2r(p)
