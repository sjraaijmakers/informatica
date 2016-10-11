# Steven Raaijmakers
# Bronnen: https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm

# GCD
def ggd(x, y):
    while y != 0:
        (x, y) = (y, x % y)
    return x

# D berekenen via egcd, en vervolgens positief maken
def get_d(a, b):
    d = egcd(a, b)[2]
    while d < 0:
        d+= a
    return d

# Uitgebreid Euclidische Algoritme
def egcd(a, b):
    x,y, u,v = 0,1, 1,0
    while a != 0:
        q, r = b // a, b % a
        m, n = x - u * q, y - v * q
        b,a, x,y, u,v = a,r, u,v, m,n
    gcd = b
    return gcd, x, y

def encode(string, e, n):
    # string naar ascii
    encoded = []
    for c in string:
        encoded.append(ord(c))

    # versleutelen
    for i, t in enumerate(encoded):
        encoded[i] = (t ** e) % n
    return encoded

# vars
p = 9796904389
q = 9990253907

#
n = p * q
o = (p - 1) * (q -1)
e = 7
d = get_d(o, e)

# stringsz

m = "Hello World"
me = encode(m, e, n)

print "String:", m
print "Encoded:", me
