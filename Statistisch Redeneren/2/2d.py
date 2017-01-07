import math

## Binomial Coefficient
def bico(x, y):
    if y == x:
        return 1
    elif y == 1:
        return x
    elif y > x:
        return 0
    else:
        a = math.factorial(x)
        b = math.factorial(y)
        c = math.factorial(x - y)
        return a // (b * c)

## Binomial division
def biver(p, n, k):
    return bico(n, k) * (p ** k) * ((1 - p)**(n - k))

def test(n, p):
    tot = 0
    for k in range(0, n + 1):
        tmp = biver(p, n, k)
        tot += tmp
    return tot

# n & p
print test(10, 0.2)
print test(20, 0.8)
print test(30, 0.3)
print test(40, 0.9)
