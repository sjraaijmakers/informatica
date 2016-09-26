#!/usr/bin/env python3

import os.path

from utils import load_json

def main():
    if not os.path.exists('.config'):
        print('Please configure before packing.')
        return

    students = load_json('.config')

    if students is None:
        print('Please configure before packing.')
        return

    print('_'.join(students.keys()))

if __name__ == '__main__':
    main()

