from msgpack import packb, unpackb
from zlib import compress, decompress

def encode(data):
    return compress(packb(data))


def decode(data):
    return unpackb(decompress(data))
