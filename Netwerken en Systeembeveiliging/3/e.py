"""
Lab 3 - Chat Room (Server)
NAME:
STUDENT ID: 10871993
DESCRIPTION: This program is a chatserver built in Python using sockets

Source: Used this tutorial as a guidline for server.py
http://bit.ly/2d1nEsQ

"""

import socket
import select
import string

usernames = {}
clientsockets = []
filter_words = []

def serve(port):
    datalen = 1024

    #Setup socket and it's binds
    serversocket = socket.socket()
    serversocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    serversocket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    serversocket.bind(('localhost', port))
    serversocket.listen(20)

    clientsockets.append(serversocket)

    while True:

        #Lists of clients which are ready to process information
        readable,writeable,excepted = select.select(clientsockets,[],[],2)

        for client in readable:
            if client is serversocket:
                #connection of new client and ready for writing
                connection, client_address = serversocket.accept()
                clientsockets.append(connection)
                name = "%s:%s"%connection.getpeername()

                print name
                #set socket in list
                connection.send("Welcome to Python chat server, " + name)
                setnick(connection, name)

            #message is ready to be recieved
            else:
                name_handler = usernames[client]
                try:
                    #recv data from a client
                    data = client.recv(datalen)
                    if data:
                        fuelup(serversocket, client, data)
                    else:
                        name_left = getuser(client)
                        #remove sockets that send garbage
                        if client in clientsockets:
                            usernames.pop(client)
                            clientsockets.remove(client)
                        say(serversocket, None,  name_handler + " has left", True)

                except Exception as e:
                    print e
                    if client in clientsockets:
                        usernames.pop(client)
                        clientsockets.remove(client)
                    say(serversocket, None,  name_handler + " has left", True)

    #no more chatserver so we close socket
    serversocket.close()

#broadcast to the whole group
def say(serversocket, client, message, servermessage):
    for outboundclient in clientsockets:
        #dont send to the server or itself
        banned_word = check_filter(outboundclient, message)
        if outboundclient != serversocket:
            try:
                if servermessage:
                    outboundclient.send("Server: " + message)
                else:
                    if not banned_word:
                        outboundclient.send(usernames[client] + ": " + message)
            except:
                outboundclient.close()
                if outboundclient in clientsockets:
                    outboundclient.remove(outboundclient)

def getuser(nickname_searched):
    return usernames.keys()[usernames.values().index(nickname_searched)]

def whisper(serversocket, client, message):
    try:
        nickname = message.split()[1]
        message = message.split()[2]
        sockettoo = getuser(nickname)

        for clients in clientsockets:
            #dont send to the server or itself
            if clients != serversocket and clients == sockettoo:
                try:
                    banned_word = check_filter(sockettoo, message)
                    if not banned_word:
                        sockettoo.send(usernames[client] + ": " + message)
                    banned_word2 = check_filter(client, message)
                    if not banned_word2:
                        client.send(usernames[client] + ": " + message)

                except Exception as e:
                    sockettoo.close()
                    if sockettoo in clientsockets:
                        clientsockets.remove(sockettoo)
    except:
        client.send("Something went wrong with your command")


def setnick(connection, name):
    if name in usernames.itervalues():
        return False
    else:
        usernames[connection] = name
        return True

def nick(serversocket, client, message):
    new_nickname = message.split()[1]
    currentnick =  usernames[client]
    setting = setnick(client, new_nickname)

    if setting:
        say(serversocket, client,  new_nickname + " is new username of " + currentnick, True)
    else:
        pm(serversocket, client, "Failed to set new nickname")


def list_c(serversocket, client, message):
    pm(serversocket, client, "Connected users are: " + str(usernames.values())[1:-1])

def help(serversocket, client, message):
    pm(serversocket, client, "Available commands are: \n /nick <user> \
     \n /say <text> or just <text> \
     \n /whisper <user> <text> \n /list \n /help or /? \
      \n /me <text> \n /whois <user> \n /filer <word> ")

def pm(serversocket, client, message):
    for clients in clientsockets:
        if clients != serversocket and clients == client:
            try:
                client.send(message)
            except:
                client.close()
                if client in clientsockets:
                    clientsockets.remove(client)

def me(serversocket, client, message):
    message = message.replace("me", "", 1)
    currentnick = usernames[client]
    say(serversocket, client, currentnick + message, True)

def whois(serversocket, client, message):
    who = message.split()[1]
    pm(serversocket, client, who + " is connected at " + str(getuser(who).getpeername()))

def check_filter(client, message):
    for item in filter_words:
        if item[0] == client:
            ban_list = item[1]
            #check the list of bans of this user for match in message
            for itemr in ban_list:
                for word in message.split():
                    if itemr == word:
                        return True
    #if user not in list return false
    return False

def filter_s(serversocket, client, message):
    word = message.split()[1]
    exists = False
    for tupple in filter_words:
        if tupple[0] == client:
            tupple[1].append(word)
            exists = True
    if not exists:
        filter_words.append([client, [word]])
    pm(serversocket, client, word + " will be filtered from now on")

def fuelup(serversocket, client, message):

    if message.startswith("/"):
        message = message.replace("/", "", 1)

        if message.split()[0] == ("whisper"):
            whisper(serversocket, client, message)

        elif message.split()[0] == ("nick"):
            nick(serversocket, client, message)

        elif message.split()[0] == ("whois"):
            whois(serversocket, client, message)

        elif message.split()[0] == ("list"):
            list_c(serversocket, client, message)

        elif message.split()[0] == ("help") or message.split()[0] == ("?"):
            help(serversocket, client, message)

        elif message.split()[0] == ("me"):
            me(serversocket, client, message)

        elif message.split()[0] == ("say"):
            say(serversocket, client, message, False)

        elif message.split()[0] == ("filter"):
            filter_s(serversocket, client, message)

        else:
            pm(serversocket, client, "Server: Command not found")
    else:
        say(serversocket, client, message, False)

# Command line parser.
if _name_ == '_main_':
    import sys
    import argparse
    p = argparse.ArgumentParser()
    p.add_argument('--port', help='port to listen on', default=12345, type=int)
    args = p.parse_args(sys.argv[1:])
    serve(args.port)
