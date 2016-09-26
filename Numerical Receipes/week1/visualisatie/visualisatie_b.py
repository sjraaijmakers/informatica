import numpy as np
import matplotlib.pyplot as plt

my_data = np.genfromtxt('bierkraag.csv', delimiter=',')
my_data = np.delete(my_data, (0), axis=0)

x = my_data[:,0]
y = my_data[:,1]

plt.plot(x, y, 'ro')

# labels
plt.legend(['H'])
plt.xlabel('t(s)')
plt.ylabel('H(cm)')
plt.title('hoogte van bierkraag')

plt.show()
