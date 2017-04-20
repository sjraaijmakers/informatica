import os
from checksum import add_integrity, check_integrity, pack_footer, unpack_footer

class DocumentStore:
    def __init__(self, filename='./storage.txt'):
        self.file = filename
        self.oldfile = None
        self.compacting = False

        f = open(self.file, 'a')
        f.close()

    def get_end(self):
        with open(self.file, "rb") as f:
            f.seek(0, 2)
            return f.tell()

    def get(self, offset):
        filesize = os.stat(self.file).st_size

        if filesize < offset:
            raise ValueError('Trying to retreieve offset {}, but file is only {} bytes long.'.format(offset, filesize))

        with open(self.file, 'rb') as f:
            f.seek(offset)
            return check_integrity(f.read())

    def append(self, data):
        data = add_integrity(data)
        with open(self.file, "ab") as f:
            f.seek(0, 2)
            end = f.tell()
            f.write(data)
            return end

    def write_footer(self, rev, offset):
        with open(self.file, "ab") as f:
            f.write(pack_footer(rev, offset))

    def read_footer(self):
        if os.stat(self.file).st_size < 8:
            return (0, 0)

        with open(self.file, "rb") as f:
            f.seek(-8, 2)
            return unpack_footer(f.read())

    def start_compacting(self):
        if self.compacting:
            return
        
        self.oldfile = self.file
        self.file = self.file + "_new"
        self.compacting = True
    
    def stop_compacting(self):
        if not self.compacting:
            return
        
        os.remove(self.oldfile)
        os.rename(self.file, self.oldfile)

        self.file = self.oldfile
        self.oldfile = None
        self.compacting = False

