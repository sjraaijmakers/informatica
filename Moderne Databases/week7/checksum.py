#!env/bin/python

"""
Moderne Databases Project

Authors:    Nicky Kessels   (nicky_kessels@hotmail.com)
            Huck Nuchelmans (me@hucknuchelmans.nl)

Course:     Moderne Databases
            Bachelor Informatica
            Universiteit van Amsterdam

File:       checksum.py


"""

from binascii import crc32
from struct import Struct

pack_uint32 = Struct('>L').pack
unpack_uint32 = lambda x: Struct('>L').unpack(x)[0]


def add_integrity(data):
    data = pack_uint32(crc32(data)) + data
    data = pack_uint32(len(data)) + data

    return data


def check_integrity(data):
    if len(data) < 8:
        raise ValueError('expected at least 8 bytes, got {} bytes.'.format(
            len(data)))

    size, data = unpack_uint32(data[:4]), data[4:]

    if len(data) < size:
        raise ValueError('expected at least {} bytes, got {} bytes.'.format(
            size, len(data)))

    checksum, data = unpack_uint32(data[:4]), data[4:]

    if crc32(data) != checksum:
        raise ValueError('checksum does not match, data may possibly be '
                         'corrupt.')

    return data
