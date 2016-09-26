from pylab import *

def sorteig(M):
    d, U = eig(M)
    si = argsort(d)[-1::-1]
    d = d[si]
    U = U[:,si]
    return (d, U)

img_size = 256
smpl_size = 25

a = imread('trui.png')
figure(1)
subplot(1, 2, 1)
imshow(a, cmap='Greys_r')

# split samples
samples = []
mean_sample = np.zeros(smpl_size * smpl_size)
for i in range(0, img_size-smpl_size + 1):
    for j in range(0, img_size-smpl_size + 1):
        tmp = a[i:i + smpl_size, j:j + smpl_size].flatten()
        samples.append(tmp.flatten())
        for index in range(len(tmp)):
            mean_sample[index] += tmp[index]
mean_sample = [x / ((img_size - smpl_size + 1) * (img_size - smpl_size + 1)) for x in mean_sample]
print mean_sample


# plt.show()
