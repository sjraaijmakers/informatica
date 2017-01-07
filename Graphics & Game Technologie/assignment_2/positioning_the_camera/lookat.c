/* Computer Graphics, Assignment 4, Positioning the camera
 *
 * Filename ........ lookat.c
 * Description ..... Contains the re-programmed lookAt function
 * Created by ...... Jurgen Sturm
 *
 * Student name .... Steven Raaijmakers Daan Meijers
 * Student email ... sjraaijmakers@gmail.com daanmeijers@live.nl
 * Collegekaart .... 10804242 10727167
 * Date ............ 10-02-2016
 * Comments ........ Deze code is een zelfgebouwde gluLookAt en creert een viewing matrix.
 * De input bestaat uit drie vectoren: de camera, het beeld en de up-vector.
 *
 * (always fill in these fields before submitting!!)
 */
#include <GL/glut.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <math.h>


/* ANSI C/ISO C89 does not specify this constant (?) */
#ifndef M_PI
#define M_PI           3.14159265358979323846  /* pi */
#endif

// Get crossproduct: X x Y
void cross_product(GLdouble *x, GLdouble *y, GLdouble *res){
    res[0] = (x[1] * y[2]) - (x[2] * y[1]);
    res[1] = (x[2] * y[0]) - (x[0] * y[2]);
    res[2] = (x[0] * y[1]) - (x[1] * y[0]);
}

// Normaliseer 3x1 vector
void normalize_3dvector(GLdouble *vector){
    double len = sqrt(pow(vector[0], 2.0) + pow(vector[1], 2.0) + pow(vector[2], 2.0));
    vector[0] = vector[0] / len;
    vector[1] = vector[1] / len;
    vector[2] = vector[2] / len;
}

// Deze functie is een zelfgebouwde gluLookAt, dit maakt een maakt een viewing matrix.
void myLookAt(GLdouble eyeX, GLdouble eyeY, GLdouble eyeZ,
              GLdouble centerX, GLdouble centerY, GLdouble centerZ,
              GLdouble upX, GLdouble upY, GLdouble upZ) {
	GLdouble cz[3], up[3], cx[3], cy[3];

// De cz vector is de vector die van de camera naar het object wijst.
	cz[0] = centerX - eyeX;
	cz[1] = centerY - eyeY;
	cz[2] = centerZ - eyeZ;

// De upvector wijst in de richting van de cz vector.
	up[0] = upX;
	up[1] = upY;
	up[2] = upZ;

	normalize_3dvector(cz);
	normalize_3dvector(up);

// We krijgen de cx vector door de orthogonaal te pakken van cz en up.
	cross_product(cz, up, cx);

	normalize_3dvector(cx);

// We krijgen de cy vector door de orthogonaal van cx en cz te pakken.
	cross_product(cx, cz, cy);

// Tenslotte krijgen we de matrix die we zoeken, deze is alvast geinverteerd.
	GLdouble m[16] = {
		cx[0], cy[0], -cz[0], 0,
		cx[1], cy[1], -cz[1], 0,
		cx[2], cy[2], -cz[2], 0,
		0, 0, 0, 1
	};

// Hier vermenigvuldigen we de huidige matrix met de viewing matrix die we hebben samengesteld.
	glMultMatrixd(m);
	glTranslated(-eyeX, -eyeY, -eyeZ);
}
