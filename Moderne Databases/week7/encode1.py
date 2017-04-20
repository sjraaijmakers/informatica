#!env/bin/python

"""
Moderne Databases Project

Authors:    Nicky Kessels   (nicky_kessels@hotmail.com)
            Huck Nuchelmans (me@hucknuchelmans.nl)

Course:     Moderne Databases
            Bachelor Informatica
            Universiteit van Amsterdam

File:       encode1.py


"""

from msgpack import packb, unpackb
from snappy import compress, decompress


def encode(data):
    return compress(packb(data))


def decode(data):
    return unpackb(decompress(data))
