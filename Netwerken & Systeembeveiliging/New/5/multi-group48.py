# Networks & Network Security - Lab 5 (Group 48)
# NAME: Steven Raaijmakers & Rosco Kalis
# STUDENT ID: 10804242 & 10771603

import subprocess
import time

def main(nodes, r, steps):
    # Store processes.
    processes = {}

    # Opening processes
    for node in range(nodes):
        # Open a process.
        p = subprocess.Popen(['python', 'lab5-group48.py', '--pipe', '1'],
                             stdout=subprocess.PIPE,
                             stdin=subprocess.PIPE)
        processes[node] = p

    for node in list(reversed(range(nodes))):
        p = processes[node]
        p.communicate("ping")
        p.stdin.flush()

        time.sleep(1)
    #
    while True:
        command = sys.stdin.readline()
        if command == "quit":
            break
        for key, process in processes.iteritems():
            process.stdin.write(command)
            process.stdin.flush()

            response = process.stdout.readline()
            print str(key) + ": " + str(response)

    # Cleaning up
    for process in processes:
        process.kill()


if __name__ == '__main__':
    import sys
    import argparse
    p = argparse.ArgumentParser()
    p.add_argument('--nodes', help='number of nodes to spawn',
                   required=True, type=int)
    p.add_argument('--range', help='sensor range', default=50, type=int)
    p.add_argument('--steps', help='output graph info every step',
                   action="store_true")
    args = p.parse_args(sys.argv[1:])
    main(args.nodes, args.range, args.steps)
