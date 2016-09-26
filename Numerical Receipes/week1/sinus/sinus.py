from __future__ import division
import math

def mysin(x, error):
    n = 0
    # normal sinus over x
    sinus = math.sin(x)

    # mysinus over x:
    mysin = 0
    while True:
        # calculate terms
        new_term = math.pow(-1, n) / math.factorial(2*n+1) * math.pow(x, 2*n+1)
        mysin += new_term
        # calculate difference between mysin and legit sinus
        cur_er = math.fabs(math.fabs(mysin) - math.fabs(sinus))
        n += 1
        if cur_er < error:
            return n

# for loop with error 10*-1 to 10*-5
for i in range(1, 5):
    print "At 10^-" + str(i) + ",",
    print str(mysin(0.5, math.pow(10, -i))) + " terms"
