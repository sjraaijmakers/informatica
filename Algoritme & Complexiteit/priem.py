# Steven Raaijmakers
# Programma beschrijft RSA
# Bronnen: https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm
# http://aditya.vaidya.info/blog/2014/06/27/modular-exponentiation-python/

# Functie voor het uitrekenen van modulaire exponentatie
def expmod_iter(a, b, c):
	x = 1
	while(b > 0):
		if(b & 1 == 1): x = (x*a)%c
		a=(a*a)%c
		b >>= 1
	return x%c

# Uitgebreid Euclidische Algoritme. Returned gcd = ax + by
def egcd(a, b):
    x,y, u,v = 0,1, 1,0
    while a != 0:
        q, r = b // a, b % a
        m, n = x - u * q, y - v * q
        b,a, x,y, u,v = a,r, u,v, m,n
    gcd = b
    return gcd, x, y

# RSA klasse. Geef P & Q mee, en E
# Berekent D via UEA
# Kan vervolgens berichten coderen en decoderen
class RSA:
    def __init__(self, p, q, e):
        self.p = p
        self.q = q
        self.e = e

        self.n = p * q
        self.t = (p - 1) * (q - 1)

        self.d = self.get_d()

    def __repr__(self):
        return "n = p * q \n  = %s * %s \n  = %s\nt = %s\ne = %s\nd = %s" % (self.p, self.q, self.n, self.t, self.e, self.d)

    # Versleutelen van string binnen RSA klasse
    def encode(self, string):
        # string naar ascii
        encoded = []
        for c in string:
            encoded.append(ord(c))

        # versleutelen
        for i, t in enumerate(encoded):
            encoded[i] = (t ** self.e) % self.n
        return encoded

    # Ontsleutelen van string binnen dezelfde klasse
    def decode(self, integers):
        decoded = []
        for i, t in enumerate(integers):
            decoded.append(expmod_iter(t, self.d, self.n))
        return decoded

    # D berekenen via UEA (EGCD), en vervolgens positief maken
    def get_d(self):
        d = egcd(self.t, self.e)[2]
        while d < 0:
            d+= self.t
        return d

# UITVOER
# Variabelen
p = 9796904389
q = 9990253907
e = 7

string = list("Hello World")

x = RSA(p, q, e)
xe = x.encode(string)
xd = x.decode(xe)

print x
print ""
print string
print xe
print xd

####
a = 25041
b = 43521
g = 2

aa = (g ** a) % p
bb = (g ** b) % p

xx = (aa ** b) % p
yy = (bb ** a) % p

print xx, yy
