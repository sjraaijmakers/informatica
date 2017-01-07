/* Computer Graphics, Assignment 5, Orthogonal projection
 *
 * Filename ........ ortho.c
 * Description ..... Contains the re-programmed orthogonal projection matrix
 * Date ............ 01.09.2006
 * Created by ...... Jurgen Sturm
 *
 * Student name .... Steven Raaijmakers, Daan Meijers
 * Student email ... sjraaijmakers@gmail.com, daanmeijers@live.nl
 * Collegekaart .... 10804242, 10727167
 * Date ............ 12 februari 2016
 * Comments ........ Zelfgebouwde versie van GluOrtho
 */
#include <GL/glut.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <stdio.h>
#include <stdlib.h>

#define sqr(x) ((x)*(x))

/* ANSI C/ISO C89 does not specify this constant (?) */
#ifndef M_PI
#define M_PI           3.14159265358979323846  /* pi */
#endif

// Creates orthogonal projection
void myOrtho(GLdouble left,
             GLdouble right,
             GLdouble bottom,
             GLdouble top,
             GLdouble near,
             GLdouble far) {
    // Vars for shortcuts
    GLdouble tx = -(right + left) / (right - left);
    GLdouble ty = -(top + bottom) / (top - bottom);
    GLdouble tz = -(far + near) / (far - near);

    // Setup orthogonal matrix
    GLfloat M[16] =
    {
        2.0 / (right - left), 0.0, 0.0, 0.0,
        0.0, 2.0 / (top - bottom), 0.0, 0.0,
        0.0, 0.0, -2.0 / (far - near), 0.0,
        tx, ty, tz, 1.0
    };

    // Add to matrixstack
    glMultMatrixf(M);
}
