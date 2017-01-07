#
# Takes the contents of the AUTHORS file via stdin and tries to parse the
# student names and numbers from this mess.
#

from __future__ import print_function
import fileinput
import re

names, nums = [], []
for line in fileinput.input():
    # Split on "and" and "en" so these won't become part of the name
    chunks = re.split(r"\s*\b(?:and|en)\b\s*", line)

    for c in chunks:
        # Regex for a single name (anything without whitespace, number, certain
        # punctuation). This tries to support unicode and eg. a '-' in the name.
        nw = r"[^&:+,\s\d]+"
        # A full name consist of two or more name-words as defined earlier.
        names += re.findall(r"(%s\s+(?:%s\s+)*%s)" % (nw, nw, nw), c, re.U)
        # Numbers are just a word consisting of just digits.
        nums += re.findall(r"\b\d+\b", c, re.U)

# Post-process the results: strip out email-addresses between <>, people putting
# "Student: " in front of the names, further whitespace etc.
names = [n.split("<")[0] for n in names]
names = [n for n in names if "student" not in n.lower()]
names = [n.strip('\n\t ()-') for n in names]

print("Names:", ' + '.join(names))
print("Nums:", ' + '.join(nums))

if len(names) != len(nums):
    print("Number of names and student numbers does not match")
    exit(1)
if len(names) not in (1,2):
    print("Should contain at least 1 and at most 2 authors")
    exit(1)
