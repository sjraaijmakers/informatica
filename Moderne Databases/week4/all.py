# Steven Raaijmakers, 10804242
# Script runs for loop over buckets over mr.sh

import subprocess
import sys


if __name__ == '__main__':
    if len(sys.argv) != 2:
        print "plz giv arg"
    else:
        word = sys.argv[1]

        p = subprocess.Popen(['curl', '-s', '-X', 'GET', "http://localhost:10018/riak?buckets=true"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        output = subprocess.check_output(['jq', '.buckets', '-c'], stdin=p.stdout)
        p.wait

        stripped = output.replace("\"", "").replace("\n", "").replace("[", "").replace("]", "").split(",")

        for i in stripped:
            p = subprocess.Popen(['sh', 'mr.sh', i, word], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            out, err = p.communicate()
            print out
