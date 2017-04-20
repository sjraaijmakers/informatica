from asteval import Interpreter
from collections import defaultdict

import functools
import re

class Script(object):
    def __init__(self):
        self.interpreter = Interpreter(max_time=60)
        self.symtable['re'] = re

    @property
    def symtable(self):
        return self.interpreter.symtable

    @symtable.setter
    def symtable(self, value):
        self.interpreter.symtable = value

    def add_file(self, path):
        with open(path, 'rb') as f:
            self.interpreter(f.read())

    def invoke(self, name, *args, **kwargs):
        f = self.interpreter.symtable.get(name, None)

        if not callable(f):
            return
        
        return f(*args, **kwargs)

    def __getattr__(self, name):
        if name in ['symtable', 'interpreter']:
            raise AttributeError("{} instance has no attribute '{}'".format(self.__class__.__name__, name))

        if not callable(self.symtable.get(name, None)):
            raise AttributeError("{} instance has no attribute '{}'".format(self.__class__.__name__, name))


        return functools.partial(self.invoke, name)

    def run_mapreduce(self, documents):
        store = defaultdict(list)
        
        for i in range(len(documents)):
            vals = self.mapper(i, documents[i])
            for k, v in vals:
                store[k].append(v)

        output = {}

        for key in store:
            k, v = self.reducer(key, store[key])
            output[k] = v

        return output