 /*Computer Graphics, Assignment, Translations, Rotations and Scaling
 *
 * Filename ........ transformations.c
 * Description ..... Contains the re-programmed translation, rotation and scaling functions
 * Student name .... Steven Raaijmakers, Daan Meijers
 * Student email ... sjraaijmakers@gmail.com, daan.meijers@live.nl
 * Collegekaart .... 10804242
 * Date ............ 12 februari 2016
 * Comments ........ rows & columns are swapped in GL
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <GL/gl.h>
#include "transformations.h"

/* ANSI C/ISO C89 does not specify this constant (?) */
#ifndef M_PI
#define M_PI           3.14159265358979323846  /* pi */
#endif

// Print 3x1 vector
void print_vector(GLfloat *vector){
    for(int i = 0; i < 3; i++){
        printf("%f\n", vector[i]);
    }
    printf("\n");
}

// Scale over x, y, z
void myScalef(GLfloat x, GLfloat y, GLfloat z)
{
    GLfloat M[16] =
    {
        x, 0.0, 0.0, 0.0,
        0.0, y, 0.0, 0.0,
        0.0, 0.0, z, 0.0,
        0.0, 0.0, 0.0, 1.0
    };
    glMultMatrixf(M);
}

// Translate over x, y, z
void myTranslatef(GLfloat x, GLfloat y, GLfloat z)
{
    GLfloat M[16] =
    {
        1.0, 0.0, 0.0, 0.0,
        0.0, 1.0, 0.0, 0.0,
        0.0, 0.0, 1.0, 0.0,
        x, y, z, 1.0
    };
    glMultMatrixf(M);
}

// Get crossproduct of two 3x1 vectors
GLfloat * cross_product(GLfloat *x, GLfloat *y){
    GLfloat *tmp = malloc(sizeof(3 * sizeof(float)));
    tmp[0] = (x[1] * y[2]) - (x[2] * y[1]);
    tmp[1] = (x[2] * y[0]) - (x[0] * y[2]);
    tmp[2] = (x[0] * y[1]) - (x[1] * y[0]);
    return tmp;
}

// Normalize 3x1 vector
void normalize_3dvector(GLfloat *vector){
    float len = sqrt(pow(vector[0], 2) + pow(vector[1], 2) + pow(vector[2], 2));
    vector[0] = vector[0] / len;
    vector[1] = vector[1] / len;
    vector[2] = vector[2] / len;
}

// Replace lowest value in vector with 1.
GLfloat * t_action(GLfloat *vector){
    GLfloat *tmp = malloc(sizeof(3 * sizeof(float)));
    // Find index of lowest value
    int index = 0;
    float minimum = vector[0];
    for(int i = 0; i < 3; i++){
        if(vector[i] < minimum){
            index = i;
        }
    }
    // Fill t
    for(int i = 0; i < 3; i++){
        if(i == index){
            tmp[i] = 1;
        }
        else {
            tmp[i] = vector[i];
        }
    }
    return tmp;
}

// Rotate with angle over vector x, y, z
void myRotatef(GLfloat angle, GLfloat x, GLfloat y, GLfloat z)
{
    GLfloat w[3] =
    {
        x,
        y,
        z
    };
    normalize_3dvector(w);

    // T: take smallest value of w and replace by 1
    GLfloat *t = t_action(w);

    // Compute u = t x w / || t x w ||
    GLfloat *u = cross_product(t, w);
    normalize_3dvector(u);

    // Compute v = w x u
    GLfloat *v = cross_product(w, u);

    // Hardcode uvw into a
    GLfloat A[16] =
    {
        u[0], u[1], u[2], 0.0,
        v[0], v[1], v[2], 0.0,
        w[0], w[1], w[2], 0.0,
        0.0, 0.0, 0.0, 1.0
    };

    // Convert angle to rads
    angle = angle * M_PI / 180;
    GLfloat B[16] =
    {
        cos(angle), sin(angle), 0.0, 0.0,
        -sin(angle), cos(angle), 0.0, 0.0,
        0.0, 0.0, 1.0, 0.0,
        0.0, 0.0, 0.0, 1.0
    };

    // Hardcode uvw into c
    GLfloat C[16] =
    {
        u[0], v[0], w[0], 0.0,
        u[1], v[1], w[1], 0.0,
        u[2], v[2], w[2], 0.0,
        0.0, 0.0, 0.0, 1.0
    };

    glMultMatrixf(A);
    glMultMatrixf(B);
    glMultMatrixf(C);

    // Free
    free(u);
    free(v);
}
