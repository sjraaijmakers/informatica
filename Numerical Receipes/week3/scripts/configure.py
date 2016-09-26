#!/usr/bin/env python3

from utils import query, query_yes_no, save_json

def query_students():
    students = {}

    while True:
        student_id = query('Student ID:')
        student_name = query('Full name:')
        student_email = query('E-mail address:')

        students[student_id] = {
                'name': student_name,
                'e-mail': student_email,
            }

        if not query_yes_no('Do you want to add another student?', default=False):
            return students

def main():
    save_json('.config', query_students())

if __name__ == '__main__':
    main()

