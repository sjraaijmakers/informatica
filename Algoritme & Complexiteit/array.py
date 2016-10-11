# Steven Raaijmakers
# Programma vindt dominante waarde in een array (van integers) of in een array
# bestaande uit RGB kleuren (zie RGB klasse)

# Bronnen:
# http://stackoverflow.com/questions/14743890/find-dominant-mode-of-an-unsorted-array
# http://www.cs.rug.nl/~wim/pub/whh348.pdf
# http://stackoverflow.com/questions/3740371/finding-the-max-repeated-element-in-an-array

# RGB klasse:
# Hierin wordt de kleur gedifineert volgens [red, green, blue] en de equals
# methode zodat de algoritmes niet aangepast hoeven te worden
class RGB:
    # Initialisatie van RGB uitgelezen uit een array
    def __init__(self, rgb):
        self.R = rgb[0]
        self.G = rgb[1]
        self.B = rgb[2]

    # Print methode
    def __repr__(self):
        return "(%s, %s, %s)" % (self.R, self.G, self.B)

    # Equals-methode. Hier wordt gekeken of de waardes van kleur x overeenkomen
    # met de waardes van kleur y.  Wanneer vergeleken wordt met een leeg
    # object; False
    def __eq__(self, other):
        if other == None or self == None:
            return False
        elif self.R == other.R and self.G == other.G and self.B == other.B:
            return True
        else:
            return False

# Lees file uit en verdeel lijnen in 1d-array
image = open("colors", "r").readlines()

# Converteer naar RGB-klasse
for i, t in enumerate(image):
    image[i] = RGB([x.strip() for x in image[i].replace("(", "").replace(")", "").split(',')])

# Dominante waarde berekenen via "Divide & Conquer"-principe
# Worstcase: O(n * log(n)), elk element wordt uiteindelijk bekeken omdat er
# gedeeld wordt totdat de array lengte 1 heeft (n). De arrays die zich hiervoor
# bevinden zijn echter steeds de helft (log(n)).
def dominant(array):
    # Als array 1 element bevat: dominante waarde is gevonden
    if len(array) == 1:
        return array[0]

    # Array in twee (recursieve) delen splitsen
    left = dominant(array[0 : len(array) / 2])
    right = dominant(array[len(array) / 2 : len(array)])

    # Als links en rechts gelijk zijn bestaat de gehele array uit dezelfde
    # dominante waarde
    if left == right:
        return left

    # Scan van beiden arrays de totale waarde
    leftCount = sum(x == left for x in array)
    rightCount = sum(x == right for x in array)

    # Kijk welke een hogere waarde oplevert
    if leftCount > len(array) / 2:
        return left
    if rightCount > len(array) / 2:
        return right

    # Anders:
    return None

# Dominant waarde via Boyer-Moore algoritme
# Worst case: O(n), omdat er over elk element eenmalig gelopen wordt
# LET OP: werkt alleen wanneer een element vaker dan n / 2 voorkomt. Een
# extra test kan hier gemaakt worden maar kost ook extra lineaire stap.
def boyer_moore(array):
    # Current is het eerste element uit de array, de counter begint op 0
    current = array[0]
    counter = 0

    for i in range(len(array)):
        # Als eenzelfde item als de Current wordt gevonden wordt de counter
        # opgehoogd
        if current == array[i]:
            counter += 1
        # Als de counter 0 is wordt het huidige element de current (& counter++)
        elif counter == 0:
            current = array[i]
            counter += 1
        # Als een ander item voorbij komt dan de current wordt de counter
        # verlaagd
        else:
            counter -= 1

    if counter == 0:
        return None

    return current

# test = [1, 1, 1, 0, 2, 3, 1]
#
# print "Array: ", test
# print "Dominant value in array: ", dominant(test)
# print image

print "Dominant RGB in image: ", dominant(image)
