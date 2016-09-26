# Vak: Numerical Recipes
# Auteurs: Steven Raaijmakers 10804242, Daan Meijers 10727167
try:
    import matplotlib.pyplot as plt
except ImportError:
    print "Warning: could not import matplotlib.pyplot"
try:
    import numpy as np
    assert np
except ImportError:
    print "Warning: could not import numpy"
try:
    from scipy import ndimage, misc
    assert ndimage, misc
except ImportError:
    print "Warning: could not import scipy.ndimage"

import filters


def convolve_image(img, filter_kernel):
    """Convolves any given image with any given filter"""
    return ndimage.convolve(img, filter_kernel, cval=0.0)


def show_images(img, sobel, laplace, gauss, prewitt, cm=plt.cm.gray, axis='off'):
    """Shows an array of images in a figure-sublot

    Args:
        img(list) - list of images

    Optional:
        cm(plt.cm) - maptlotlib colormap
        axis(str) - argument to give plt
    """
    X, Y, gradient_x, gradient_y, total = prewitt

    fig = plt.figure()
    fig.suptitle("Filtervergelijking")
    fig.add_subplot(2, 3, 2)
    plt.axis(axis)
    plt.title("original")
    plt.imshow(img, cmap=cm)

    for i in range(5, 9, 1):
    	name = ""
    	if i == 5:
    		images = sobel
    		name = "Sobel"
    	elif i == 6:
    		images = laplace
    		name = "Laplace"
    	else:
    		images = gauss
    		name = "Gauss"

    	fig.add_subplot(2, 4, i)
    	plt.axis(axis)
    	plt.imshow(images, cmap=cm)
    	if i == 8:
    		plt.quiver(X, Y, gradient_x[::total,::total], gradient_y[::total, ::total], color='r')
    		name = "Prewitt + Vector"
    	plt.title(name)

    plt.show()

def prewitt_assignment(img):
    """"""
    # Get filter:
    filter_x, filter_y = filters.prewitt()
    x, y = np.shape(img)

    # Apply Filter
    gradient_x = ndimage.convolve(img, filter_x, cval=0.0)
    gradient_y = ndimage.convolve(img, filter_y, cval=0.0)
    
    ylen, xlen = np.shape(img)
    total = 16
    xt = np.linspace(0, xlen, xlen/total)
    yt = np.linspace(0, ylen, ylen/total)
    (X, Y) = np.meshgrid(xt, yt)
    return [X, Y, gradient_x, gradient_y, total]

# Find edges via laplace filter
def laplace_assignment(img):
    laplace = convolve_image(img, filters.laplace())
    return laplace

# Find edges via gauss filter
def gauss_assignment(img):
	fig1 = plt.figure()
	fig1.suptitle("Gauss sigma-vergelijking")
	for x in range(0, 10, 1):
	    sigma = x # find optimal sigma (VERSLAG)
	    blurred = ndimage.gaussian_filter(img, sigma)
	    gauss = img-blurred

	    fig1.add_subplot(3, 3, x)
	    plt.axis('off')
	    plt.title("sigma: "+str(sigma))
	    plt.imshow(gauss, cmap=plt.cm.gray)
	plt.show()

	blurred = ndimage.gaussian_filter(img, 4)
	gauss = img-blurred
	return gauss

# Find edges via sobel filter. Show values inbetween (Sx & Sy)
def sobel_assignment(img):
    x, y = filters.sobel()

    Sx = convolve_image(img, x)
    Sy = convolve_image(img, y)
    G = np.sqrt(Sx ** 2 + Sy ** 2)
    return G

if __name__ == "__main__":
	pictures = ['lena.png', 'gauss.png', 'p1.png','p2.png','p3.jpg']
	for pic in pictures:
		name = 'img/'+pic
		img = ndimage.imread(name, flatten=True)
		sobel = sobel_assignment(img)
		laplace = laplace_assignment(img)
		gauss = gauss_assignment(img)
		prewitt = prewitt_assignment(img)
		show_images(img, sobel, laplace, gauss, prewitt)

    # call other sub-assignments
