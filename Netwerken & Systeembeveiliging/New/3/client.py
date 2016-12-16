"""
Lab 3 - Chat Room (Client)
NAME:
STUDENT ID:
DESCRIPTION:
"""

import socket
import time
import ssl

from gui import MainWindow


# Try to receive socket data
def try_receive(socket):
    try:
        return socket.recv(512)
    except:
        return None


# Loop is called from main
def loop(port, cert):
    """
    GUI loop.
    port: port to connect to.
    cert: public certificate (bonus task)
    """
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s = ssl.wrap_socket(s, ca_certs="server.crt")

    s.settimeout(10)
    s.connect(("localhost", port))
    s.setblocking(0)

    # The following code explains how to use the GUI.
    w = MainWindow()
    # update() returns false when the user quits or presses escape.
    while w.update():
        time.sleep(0.01)
        data = try_receive(s)
        # if the user entered a line getline() returns a string.
        if data:
            w.writeln(data)
        line = w.getline()
        if line:
            s.send(line)


# Command line parser.
if __name__ == '__main__':
    import sys
    import argparse
    p = argparse.ArgumentParser()
    p.add_argument('--port', help='port to connect to',
                   default=12345, type=int)
    p.add_argument('--cert', help='server public cert', default='')
    args = p.parse_args(sys.argv[1:])
    loop(args.port, args.cert)
