# Networks & Network Security - Lab 5 (Group 48)
# NAME: Steven Raaijmakers & Rosco Kalis
# STUDENT ID: 10804242 & 10771603

import sys
import struct
from socket import *
from random import randint
from gui import MainWindow
from sensor import *

import time
import select

ECHO_SQ = 0


# Get random position in NxN grid.
def random_position(n):
    x = randint(0, n)
    y = randint(0, n)
    return (x, y)


# Hanles incomming message for sockets
def msg_handler(data, address, receiver, window, wave, echo_wave):
    m_type, sequence, initiator_pos, neighbor_pos, \
     operation, capability, payload = message_decode(data)

    # Create sensor class for initiator
    initiator = Sensor(None, initiator_pos, [], capability, None)

    # When ping is received from any other socket, send pong back
    if m_type == MSG_PING and receiver != initiator \
       and receiver.in_range(initiator) and initiator.in_range(receiver):
        message = message_encode(MSG_PONG, 0, initiator.position,
                                 receiver.position, OP_NOOP, initiator.range,
                                 0)
        receiver.socket.sendto(message, address)

    # When received a pong, add sender to neighbors
    elif m_type == MSG_PONG and ((neighbor_pos, address)
                                 not in receiver.neighbors):
        receiver.add_neighbor(neighbor_pos, address)

    # When received an echo:
    elif m_type == MSG_ECHO:
        print "Incomming echo"
        echo = message_encode(MSG_ECHO, sequence, initiator.position,
                              receiver.position, operation, capability, 0)
        echo_reply = message_encode(MSG_ECHO_REPLY, sequence,
                                    initiator.position, receiver.position,
                                    operation, receiver.range, 0)

        # Create "identifier": initiator position + sequence is unique combo
        identifier = str((initiator.position, sequence))

        # When identifier is not in the dictionary; make sender its father
        if identifier not in echo_wave:
            replied = 0
            if operation == OP_SAME:
                if payload == receiver.value:
                    size = 1
                else:
                    size = 0
            # TODO: hoe fix je de linebreak van voorwaardes (pep8) ?
            elif operation == OP_SUM or operation == OP_MIN \
                    or operation == OP_MAX:
                size = receiver.value
            else:
                size = 1
            echo_wave[identifier] = (initiator.position, replied, size)

            # If sensor has only 1 neighbor (== father), send reply back
            if len(receiver.neighbors) == 1:
                print "echo_reply to father (1 neighbor)"
                if operation == OP_SIZE:
                    payload = 1
                elif operation == OP_SUM or operation == OP_SAME \
                        or operation == OP_MIN or operation == OP_MAX:
                    payload = size

                if operation != OP_NOOP:
                    echo_reply = message_encode(MSG_ECHO_REPLY, sequence,
                                                initiator.position,
                                                receiver.position, operation,
                                                initiator.range, payload)

                receiver.socket.sendto(echo_reply, address)
            elif not receiver.neighbors:
                print "no neighbors"
            else:
                for n_pos, n_address in receiver.neighbors:
                    if n_pos != initiator.position:
                        receiver.socket.sendto(echo, n_address)
        # When identifier is already in echo wave, there's a duplicate
        else:
            if operation == OP_MIN or operation == OP_MAX:
                _, _, size = echo_wave[identifier]
                echo_reply = message_encode(MSG_ECHO_REPLY, sequence,
                                            initiator.position,
                                            receiver.position, operation,
                                            initiator.range, size)
            receiver.socket.sendto(echo_reply, address)

    # Getting dem echo's
    elif m_type == MSG_ECHO_REPLY:
        print "incoming reply"

        # Create unique identifier
        identifier = str((initiator.position, sequence))
        father, replied, size = echo_wave[identifier]
        replied += 1

        if operation == OP_MIN:
            if payload <= receiver.value:
                size = int(payload)
        elif operation == OP_MAX:
            if payload >= receiver.value:
                size = int(payload)
        elif operation != OP_NOOP:
            size += int(payload)

        echo_wave[identifier] = (father, replied, size)
        neighbor_amount = len(receiver.neighbors)

        # If receiver is also the initiator. This means there's been send an
        # echo reply to the initiator
        if receiver.position == initiator.position:
            if neighbor_amount == replied:
                if operation == OP_SAME:
                    line = "Same: " + str(size)
                elif operation == OP_SIZE:
                    line = "Size: " + str(size)
                elif operation == OP_SUM:
                    line = "Sum: " + str(size)
                elif operation == OP_MIN:
                    line = "Min: " + str(size)
                elif operation == OP_MAX:
                    line = "Max: " + str(size)
                else:
                    line = "Decided"
                if window:
                    window.writeln(line)
                else:
                    print line
                return

        # If t
        elif neighbor_amount - 1 == replied:
            print "echo_reply naar father"
            for n_pos, n_address in receiver.neighbors:
                if n_pos == father:
                    if operation != OP_NOOP:
                        echo_reply = message_encode(MSG_ECHO_REPLY, sequence,
                                                    initiator.position,
                                                    receiver.position,
                                                    operation, initiator.range,
                                                    size)
                    receiver.socket.sendto(echo_reply, n_address)


# Handles command that have been communicated via users
def cmd_handler(cmd, args, sensor, mcast_addr, grid_size, echo_wave):
    global ECHO_SQ
    # Manually send ping to discover neighbors
    if cmd == "ping":
        sensor.neighbors = []
        sensor.ping(mcast_addr)
        return "Sent Ping"

    # Show all neighbors of current sock
    elif cmd == "list":
        if not sensor.neighbors:
            return "Neighbors: None"
        else:
            tmp = "Neighbors: \n"
            for position, address in sensor.neighbors[:-1]:
                tmp += str(position) + ""
            else:
                tmp += str(sensor.neighbors[-1][0])
            return tmp

    # Reposition to random position
    elif cmd == "move":
        sensor.move(random_position(grid_size))
        return "Sensor moved to " + str(sensor.position)

    # Sets the sensor's range to argument (between 20, 70 and divisble by 10)
    elif cmd == "set":
        new_range = int(args[0])
        if new_range >= 20 and new_range <= 70 and new_range % 10 == 0:
            sensor.set_range(new_range)
            return "Sensor capability changed to: " + str(sensor.range)
        return "Value should be between 20 and 70, and divisble by 10."

    # Send to echo
    elif cmd == "echo":
        if len(sensor.neighbors) == 0:
            return "No Neighbors"
        else:
            echo_wave[str((sensor.position, ECHO_SQ))] = (sensor.position,
                                                          0, 0)
            sensor.echo(ECHO_SQ, OP_NOOP, 0)
            ECHO_SQ += 1
            return "Sent Echo"

    elif cmd == "size":
        if len(sensor.neighbors) == 0:
            return "Size: 1"
        else:
            echo_wave[str((sensor.position, ECHO_SQ))] = (sensor.position,
                                                          0, 1)
            sensor.echo(ECHO_SQ, OP_SIZE, 0)
            ECHO_SQ += 1
            return ""

    # Sum: determines sum of value of sensors in network
    elif cmd == "sum":
        if len(sensor.neighbors) == 0:
            return "Sum: " + str(sensor.value)
        else:
            echo_wave[str((sensor.position, ECHO_SQ))] = (sensor.position, 0,
                                                          sensor.value)
            sensor.echo(ECHO_SQ, OP_SUM, 0)
            ECHO_SQ += 1

    # Same: prints number of sensor with same value as initiator
    elif cmd == "same":
        if len(sensor.neighbors) == 0:
            return "Same: 0"
        else:
            echo_wave[str((sensor.position, ECHO_SQ))] = (sensor.position,
                                                          0, 0)
            sensor.echo(ECHO_SQ, OP_SAME, sensor.value)
            ECHO_SQ += 1

    # Min: prints lowest sensor value in network
    elif cmd == "min":
        if len(sensor.neighbors) == 0:
            return "Min: " + str(sensor.value)
        else:
            echo_wave[str((sensor.position, ECHO_SQ))] = (sensor.position, 0,
                                                          sensor.value)
            sensor.echo(ECHO_SQ, OP_MIN, sensor.value)
            ECHO_SQ += 1

    # Max: prints max sensor value in network
    elif cmd == "max":
        if len(sensor.neighbors) == 0:
            return "Max: " + str(sensor.value)
        else:
            echo_wave[str((sensor.position, ECHO_SQ))] = (sensor.position, 0,
                                                          sensor.value)
            sensor.echo(ECHO_SQ, OP_MAX, sensor.value)
            ECHO_SQ += 1

    # Set sensor's value to a random int
    elif cmd == "val":
        new_val = randint(0, 100)
        sensor.set_value(new_val)
        return "Sensor value changed to: " + str(sensor.value)

    # for debugging
    elif cmd == "print":
        return sensor

    else:
        return "Unkown command"


def main(mcast_addr, sensor_pos, sensor_range, sensor_val, grid_size,
         ping_period, pipe):
    # -- Create the multicast listener socket. --
    mcast = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)
    # Sets the socket address as reusable so you can run multiple instances
    # of the program on the same machine at the same time.
    mcast.setsockopt(SOL_SOCKET, SO_REUSEADDR, 1)
    # Subscribe the socket to multicast messages from the given address.
    mreq = struct.pack('4sl', inet_aton(mcast_addr[0]), INADDR_ANY)
    mcast.setsockopt(IPPROTO_IP, IP_ADD_MEMBERSHIP, mreq)
    if sys.platform == 'win32':  # windows special case
        mcast.bind(('localhost', mcast_addr[1]))
    else:  # should work for everything else
        mcast.bind(mcast_addr)

    # -- Create the peer-to-peer socket. --
    peer = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)
    # Set the socket multicast TTL so it can send multicast messages.
    peer.setsockopt(IPPROTO_IP, IP_MULTICAST_TTL, 5)
    # Bind the socket to a random port.
    if sys.platform == 'win32':  # windows special case
        peer.bind(('localhost', INADDR_ANY))
    else:  # should work for everything else
        peer.bind(('', INADDR_ANY))

    # OOP da sensor
    sensor = Sensor(peer, sensor_pos, [], sensor_range, sensor_val)

    if pipe == 0:
        # -- make the gui --
        window = MainWindow()
        window.writeln('my address is %s:%s' % peer.getsockname())
        window.writeln('my position is (%s, %s)' % sensor.position)
        window.writeln('my sensor value is %s' % sensor.value)
        window.writeln('my sensor range is %s' % sensor.range)
        window.writeln("-----------------------------------------------------")

        last_ping = time.time() - ping_period
        echo_wave = {}

        # -- This is the event loop. --
        while window.update():
            read, _, _ = select.select([mcast, peer], [], [], 0)

            # For every sock in read, handle incomming messages
            for sock in read:
                data, address = sock.recvfrom(2048)
                msg_handler(data, address, sensor, window, None, echo_wave)

            # User input
            line = window.getline()
            if line:
                # Parse user input
                input = line.split()
                cmd = input[0]
                args = input[1:]

                # Determine action for input
                out = cmd_handler(cmd, args, sensor, mcast_addr, grid_size,
                                  echo_wave)
                if out:
                    window.writeln(out)

            # Periodically (re)send ping to discover neighborssss
            if (time.time() - last_ping) > ping_period:
                print "resend ping"
                sensor.neighbors = []
                sensor.ping(mcast_addr)
                last_ping = time.time()
    else:
        last_ping = time.time() - ping_period
        echo_wave = {}

        while True:
            read, _, _ = select.select([mcast, peer], [], [], 0)

            # For every sock in read, handle incomming messages
            for sock in read:
                data, address = sock.recvfrom(2048)
                msg_handler(data, address, sensor, None, None, echo_wave)

            # User input
            line = sys.stdin.readline()
            if line:
                # Parse user input
                input = line.split()
                cmd = input[0]
                args = input[1:]
                if cmd == "quit":
                    break

                # Determine action for input
                out = cmd_handler(cmd, args, sensor, mcast_addr, grid_size,
                                  echo_wave)
                if out:
                    print out
                    sys.stdout.flush()

            # Periodically (re)send ping to discover neighborssss
            if (time.time() - last_ping) > ping_period:
                sensor.neighbors = []
                sensor.ping(mcast_addr)
                last_ping = time.time()


# -- program entry point --
if __name__ == '__main__':
    import sys
    import argparse
    p = argparse.ArgumentParser()
    p.add_argument('--group', help='multicast group', default='224.1.1.1')
    p.add_argument('--port', help='multicast port', default=50000, type=int)
    p.add_argument('--pos', help='x,y sensor position', default=None)
    p.add_argument('--grid', help='size of grid', default=100, type=int)
    p.add_argument('--range', help='sensor range', default=50, type=int)
    p.add_argument('--value', help='sensor value', default=-1, type=int)
    p.add_argument('--period', help='period between autopings (0=off)',
                   default=5, type=int)
    p.add_argument('--pipe', help='whether the output should be piped',
                   default=0, type=int)
    args = p.parse_args(sys.argv[1:])
    if args.pos:
        pos = tuple(int(n) for n in args.pos.split(',')[:2])
    else:
        pos = random_position(args.grid)
    if args.value >= 0:
        value = args.value
    else:
        value = randint(0, 100)
    mcast_addr = (args.group, args.port)
    main(mcast_addr, pos, args.range, value, args.grid, args.period, args.pipe)
