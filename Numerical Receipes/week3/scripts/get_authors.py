#!/usr/bin/env python3

import os.path
import sys

from utils import load_json

def main():
    if not os.path.exists('.config'):
        print('Please configure before packing.', file=sys.stderr)
        return

    students = load_json('.config')

    if students is None:
        print('Please configure before packing.', file=sys.stderr)
        return

    for student in students.values():
        print(' - {} <{}>'.format(student['name'], student['e-mail']))

if __name__ == '__main__':
    main()

