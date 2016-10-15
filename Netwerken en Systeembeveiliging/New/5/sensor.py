# Networks & Network Security - Lab 5 (Group 48)
# NAME: Steven Raaijmakers & Rosco Kalis
# STUDENT ID: 10804242 & 10771603

import struct
import math


# Get distance between two 2d-points
def distance(a, b):
    x_distance = float(a[0]) - float(b[0])
    y_distance = float(a[1]) - float(b[1])
    return math.sqrt(x_distance ** 2 + y_distance ** 2)


# Sensor class defines all vars of a sensor
class Sensor:

    def __init__(self, socket, position, neighbors, range, value):
        self.socket = socket
        self.position = position
        self.neighbors = neighbors
        self.range = range
        self.value = value

    # Change sensor poisition
    def move(self, new_position):
        self.position = new_position

    # Change sensor range
    def set_range(self, new_range):
        self.range = new_range

    # Change sensor value
    def set_value(self, new_value):
        self.value = new_value

    # Check wether Other is within range of self
    def in_range(self, other):
        if distance(self.position, other.position) <= self.range:
            return True
        return True

    # TODO: design problem. Should we append sensor classes as neighbors,
    # instead of just this tupple?
    def add_neighbor(self, neighbor_pos, address):
        self.neighbors.append((neighbor_pos, address))

    # Ping
    def ping(self, mcast_addr):
        message = message_encode(MSG_PING, 0, self.position, self.position,
                                 OP_NOOP, self.range, self.value)
        self.socket.sendto(message, mcast_addr)

    # Echo
    def echo(self, sequence, operation, val):
        if operation == OP_SAME:
            payload = val
        else:
            payload = 0
        message = message_encode(MSG_ECHO, sequence, self.position,
                                 self.position, operation, self.range, payload)
        for _, address in self.neighbors:
            self.socket.sendto(message, address)

    # Print function, for debugging
    def __repr__(self):
        return (str(self.position) + " @ " + str(self.socket.getsockname()) +
                " / Value: " + str(self.value) + ", Capability: " +
                str(self.range))

    # Equals is defined by a sensor position
    def __eq__(self, other):
        return self.position == other.position

    # Negation:
    def __ne__(self, other):
        return not self.__eq__(other)


# These are the message types.
MSG_PING = 0        # Multicast ping.
MSG_PONG = 1        # Unicast pong.
MSG_ECHO = 2        # Unicast echo.
MSG_ECHO_REPLY = 3  # Unicast echo reply.

# These are the echo operations.
OP_NOOP = 0       # Do nothing.
OP_SIZE = 1       # Compute the size of network.
OP_SUM = 2        # Compute the sum of all sensor values.
OP_MIN = 3        # Compute the lowest sensor value.
OP_MAX = 4        # Compute the highest sensor value.
OP_SAME = 5       # Compute the number of sensors with the same value.

# This is used to pack message fields into a binary format.
message_format = struct.Struct('!8if')

# Length of a message in bytes.
message_length = message_format.size


def message_encode(type, sequence, initiator, neighbor,
                   operation=0, capability=0, payload=0):
    """
    Encodes message fields into a binary format.
    type: The message type.
    sequence: The wave sequence number.
    initiator: An (x, y) tuple that contains the initiator's position.
    neighbor: An (x, y) tuple that contains the neighbor's position.
    operation: The echo operation.
    capability: The capability range of initiator
    payload: Echo operation data (a number).
    Returns: A binary string in which all parameters are packed.
    """
    ix, iy = initiator
    nx, ny = neighbor
    return message_format.pack(type, sequence, ix, iy, nx, ny,
                               operation, capability, payload)


def message_decode(buffer):
    """
    Decodes a binary message string to Python objects.
    buffer: The binary string to decode.
    Returns: A tuple containing all the unpacked message fields.
    """
    type, sequence, ix, iy, nx, ny, operation, capability, payload = \
        message_format.unpack(buffer)
    return (type, sequence, (ix, iy), (nx, ny), operation, capability, payload)
