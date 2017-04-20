from binascii import crc32
from struct import Struct

pack_uint32 = Struct('>L').pack
unpack_uint32 = lambda x: Struct('>L').unpack(x)[0]

def pack_footer(rev, offset):
    return pack_uint32(rev) + pack_uint32(offset)

def unpack_footer(footer):
    if len(footer) != 8:
        raise ValueError('expected 8 bytes, got {} bytes'.format(len(footer)))

    return unpack_uint32(footer[:4]), unpack_uint32(footer[4:])

def add_integrity(data):
    data = pack_uint32(crc32(data)) + data
    data = pack_uint32(len(data)) + data
    
    return data
    
def check_integrity(data):
    if len(data) < 8:
        raise ValueError('expected at least 8 bytes, got {} bytes'.format(len(data)))
        
    size, data = unpack_uint32(data[:4]), data[4:]
    
    if len(data) < size:
        raise ValueError('expected at least {} bytes, got {} bytes.'.format(size, len(data)))
        
        
    checksum, data = unpack_uint32(data[:4]), data[4:size]
    
    if crc32(data) != checksum:
        raise ValueError('checksum does not match, data may possibly be corrupt.')
    
    return data
