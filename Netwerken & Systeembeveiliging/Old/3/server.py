"""
Lab 3 - Chat Room (Server)
NAME: Steven Raaijmakers
STUDENT ID: 10804242
"""

# Lots of unneccessary commands had to be implemented which did not contributed
# to skills of networks in anyway.

import ssl
import socket
import select
from datetime import datetime

RECV = 512


# Message Class:
# Defines a Message with time, sender (socket) and content
class Message:
    def __init__(self, sender, content):
        self.time = datetime.now().strftime('%H:%M:%S')
        self.sender = sender
        self.content = content


# Chatserver Class:
# Sets up a chatserver
class Chatserver():
    # Initialize server
    def __init__(self, port):
        self.port = port

        # Create connection via socket
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)

        # Set encrypted version of "sock" as server
        self.server = ssl.wrap_socket(sock, keyfile="server.key",
                                      certfile="server.crt")

        # Connection list contains all sockets + nickname (including server)
        self.connection_list = {}

        self.filters = {}

        # Admin contains all sockets who have admin-rights
        self.admin = []

    # Returns corresponding nickname of socket
    def getnickname(self, socket):
        return self.connection_list[socket]

    # Sets nickname of socket
    def setnickname(self, socket, nickname):
        # Check wheter nicknames is not already taken
        if nickname in self.connection_list.itervalues():
            content = nickname + " is already taken."
            message = Message(self.server, content)
            self.pm(socket, message)
        # If username is available
        else:
            content = self.getnickname(socket) + " is now called " + nickname
            message = Message(self.server, content)
            self.broadcast(message)
            # First send broadcast, then change nickname (otherwise old
            # nickname will be unknown)
            # Where the magic happens:
            self.connection_list[socket] = nickname

    # Find socket instance by nickname
    def getsocket(self, search_nickname):
        for socket, nickname in self.connection_list.items():
            if nickname == search_nickname:
                return socket
        # When unknown nickname is requested
        raise ValueError("Value not found in list")

    def filter_add(self, socket, word):
        if socket in self.filters:
            self.filters[socket].append(word)
        else:
            self.filters[socket] = [word]

    def add_admin(self, socket):
        self.admin.append(socket)

    # Removes socket-connection
    def clean(self, socket):
        # Notify users via broadcast:
        content = self.getnickname(socket) + " is offline."
        message = Message(self.server, content)
        self.broadcast(message)

        # Remove socket from connection_list
        del self.connection_list[socket]

        # If removed socket was "admin", also remove from [admin]
        if socket in self.admin:
            self.admin.remove(socket)
            # If there are no admin left, make first socket in connection_list
            # an admin
            if len(self.admin) < 2:
                self.add_admin(self.admin.items()[1])

        # close socket
        socket.close()

    # Checks wheter one of the elements of word are in content
    def check_filter(self, words, content):
        if not words:
            return False
        for word in words:
            if word in content:
                return True
        return False

    def get_filter_words(self, receiver):
        if receiver in self.filters:
            return self.filters[receiver]
        return None

    # Send message to socket, hard way.
    def send(self, receiver, message):
        try:
            nickname = self.getnickname(message.sender)
            if receiver == message.sender:
                nickname = "You"
            time = message.time
            content = message.content
            # Output format
            words = self.get_filter_words(receiver)

            # Filter messages when sender is not server
            if message.sender != self.server:
                # Check wheter words are in content
                f = self.check_filter(words, content)
                # If not: send message
                if f is False:
                    receiver.send(time + " " + nickname + ": " + content)
            # If message is from server, display anyway
            else:
                receiver.send(time + " " + nickname + ": " + content)
        except:
            self.clean(receiver)

    # Personal message: send message to one receiver
    def pm(self, receiver, message):
        # Send message to only one socket, find the right on in connect_list
        sender = message.sender

        # If message is send by server, only display to "receiver"
        if sender == self.server:
            self.send(receiver, message)
        # If message is send by not-server, display to both sender and receiver
        else:
            self.send(receiver, message)
            self.send(sender, message)

    # Broadcast: send message to all sockets
    def broadcast(self, message):
        # Send message to all socket in connection list, except server
        for socket, _ in self.connection_list.items():
            if socket != self.server:
                self.send(socket, message)

    # Commands handles commands from the input line
    def commands(self, socket, command, args):
        try:
            # Changing nickname
            if command == "nick":
                # Only parse the first word of args
                new_nick = args[0]
                self.setnickname(socket, new_nick)

            # Say: broadcast message to everyone
            elif command == "say":
                # Cast "args" to a string
                content = " ".join(args)
                message = Message(socket, content)
                self.broadcast(message)

            # Whisper: send message to one socekt
            elif command == "whisper":
                receiver = self.getsocket(args[0])
                content = " ".join(args[1:])
                message = Message(socket, "*** [" +
                                  self.getnickname(receiver) + "] " + content)
                self.pm(receiver, message)

            # List outputs a list of all connected socket's nicknames
            elif command == "list":
                connected_socks = ""
                for s, n in self.connection_list.items():
                    if s != self.server:
                        connected_socks += "[" + n + "]"
                content = "These users are connected: " + connected_socks
                message = Message(self.server, content)
                self.pm(socket, message)

            # Help: prints available help commands
            elif command == "help" or command == "?":
                content = ("Available commands are: \n"
                           "/nick <user>\n/say <text>\n"
                           "/whisper <user> <text>\n"
                           "/list\n/help or /?\n/me <text>\n/whois <user>\n"
                           "/filter <word>")
                message = Message(self.server, content)
                self.pm(socket, message)

            # Filter?!@?!?/#JLHIOUO
            elif command == "filter":
                word = args[0]
                content = "You've added " + word + " to your filters"
                message = Message(self.server, content)
                self.pm(socket, message)
                self.filter_add(socket, word)

            # Me: replaces "me" by sockets nickname
            elif command == "me":
                content = self.getnickname(socket) + " " + " ".join(args)
                message = Message(socket, content)
                self.broadcast(message)

            # USERPORt?!?!?!?
            elif command == "whois":
                user_nickname = args[0]
                user_socket = self.getsocket(user_nickname)
                user_ip, user_port = user_socket.getsockname()
                content = ("User \"" + user_nickname +
                           "\" is connected from host " +
                           str(user_ip) + " on port " + str(user_port))
                message = Message(self.server, content)
                self.pm(socket, message)

            # Ban user
            elif command == "ban":
                if socket in self.admin:
                    to_ban = self.getsocket(args[0])
                    self.clean(to_ban)
        except Exception, e:
            message = Message(self.server, "Something went wrong.")
            self.pm(socket, message)
            print str(e)

    # Set server to listen
    def listen(self):
        self.server.bind(("localhost", self.port))
        self.server.listen(10)
        self.connection_list[self.server] = "Server"

        while True:
            # Create a list of all connected sockets
            # Can't be done in another way, and is needed for .select()
            connected = []
            for key in self.connection_list.keys():
                connected.append(key)
            read, _, _ = select.select(connected, [], [])
            for socket in read:
                # First time a connection is made
                if socket == self.server:
                    new_socket, new_address = self.server.accept()
                    # Add new socket to connection_list
                    self.connection_list[new_socket] = "%s:%s" % new_address
                    # Let the world know that a new socket has connected
                    content = self.getnickname(new_socket) + " is connected."
                    message = Message(self.server, content)
                    self.broadcast(message)
                    # Second socket in connection_list is the first not-server
                    if len(self.connection_list) == 2:
                        self.add_admin(new_socket)
                else:
                    try:
                        data = socket.recv(RECV)
                        if data:
                            # When command is used
                            if data.startswith("/"):
                                # Split input to commands and args
                                input = data.split()
                                command = input[0].replace("/", "")
                                args = input[1:]
                                self.commands(socket, command, args)
                            # No command is used: use "say"
                            else:
                                input = data.split()
                                self.commands(socket, "say", input)
                        # Client has disconnected
                        else:
                            self.clean(socket)
                    # Something went wrong, just clean the socket
                    except:
                        self.clean(socket)

    # Close the server socket
    def close(self):
        self.server.close()


# Run via main
def serve(port):
    # Create server
    c = Chatserver(port)
    # Listen to server
    c.listen()
    # Close the server
    c.close()


# Command line parser.
if __name__ == '__main__':
    import sys
    import argparse
    p = argparse.ArgumentParser()
    p.add_argument('--port', help='port to listen on', default=12345, type=int)
    args = p.parse_args(sys.argv[1:])
    serve(args.port)
