/* Computer Graphics assignment, Triangle Rasterization
 * Filename ........ trirast.c
 * Description ..... Implements triangle rasterization
 * Created by ...... Paul Melis
 *
 * Student name .... Steven Raaijmakers, Daan Meijers
 * Student email ... sjraaijmakers@gmail.com, daanmeijers@live.nl
 * Collegekaart .... 10804242
 * Date ............ 5 februari 2016
 * Comments ........
 * source: https://fgiesen.wordpress.com/2013/02/08/triangle-rasterization-in-practice/
 */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "types.h"

// Min & Max functions
#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#define MIN(x, y) (((x) < (y)) ? (x) : (y))

/*
 * Rasterize a single triangle.
 * The triangle is specified by its corner coordinates
 * (x0,y0), (x1,y1) and (x2,y2).
 * The triangle is drawn in color (r,g,b).
 */

// Point struct (easier)
typedef struct Point {
    float x;
    float y;
} Point;

// F_{ab}(x, y) = (a.y - b.y)*x + (b.x - a.x)y + (a.x*b.y) - (b.x*a.y)
float orient(Point *a, Point *b, float x, float y){
    return (a->y - b->y) * x + (b->x - a->x) * y + (a->x * b->y) - (b->x * a->y);
}

void
draw_triangle(float x0, float y0, float x1, float y1, float x2, float y2,
    byte r, byte g, byte b) {
    // Capture parameters into points
    Point *v0 = malloc(sizeof(Point));
    v0->x = x0;
    v0->y = y0;

    Point *v1 = malloc(sizeof(Point));
    v1->x = x1;
    v1->y = y1;

    Point *v2 = malloc(sizeof(Point));
    v2->x = x2;
    v2->y = y2;

    // Determine an offscreen point (typically -1,-1)
    Point *off = malloc(sizeof(Point));
    off->x = -1;
    off->y = -1;

    // Bounding
    float xmin = MIN(MIN(x0, x1), x2);
    float ymin = MIN(MIN(y0, y1), y2);
    float xmax = MAX(MAX(x0, x1), x2);
    float ymax = MAX(MAX(y0, y1), y2);

    // Barry helpers
    float fa = orient(v1, v2, x0, y0);
    float fb = orient(v2, v0, x1, y1);
    float fc = orient(v0, v1, x2, y2);

    // Loop over y and x, and compute barycentric points. When
    // they are inside the edges; draw a pixel in r,g,b
    for (float y = ymin; y <= ymax; y++) {
        for (float x = xmin; x <= xmax; x++) {
            // Get Barycentric points
            float alpha = orient(v1, v2, x, y) / fa;
            float beta = orient(v2, v0, x, y) / fb;
            float gamma = orient(v0, v1, x, y) / fc;

            // If current (x,y) are inside edges
            if(alpha >= 0.0f && beta >= 0.0f && gamma >= 0.0f){
                // Fix double edges with offscreen point
                if((alpha > 0 || fa * orient(v1, v2, off->x, off->y) > 0) &&
                    (beta > 0 || fb * orient(v2, v0, off->x, off->y) > 0) &&
                    (gamma > 0 || fc * orient(v0, v1, off->x, -off->y) > 0)){
                        PutPixel(x, y, r, g, b);
                }
            }
        }
    }
    // Free space
    free(v0);
    free(v1);
    free(v2);
    free(off);

    return;
}

void
draw_triangle_optimized(float x0, float y0, float x1, float y1, float x2, float y2,
    byte r, byte g, byte b) {
    // Capture parameters into points
    Point *v0 = malloc(sizeof(Point));
    v0->x = x0;
    v0->y = y0;

    Point *v1 = malloc(sizeof(Point));
    v1->x = x1;
    v1->y = y1;

    Point *v2 = malloc(sizeof(Point));
    v2->x = x2;
    v2->y = y2;

    // Bounding
    float xmin = MIN(MIN(x0, x1), x2);
    float ymin = MIN(MIN(y0, y1), y2);
    float xmax = MAX(MAX(x0, x1), x2);
    float ymax = MAX(MAX(y0, y1), y2);

    // helpers
    float fa = orient(v1, v2, xmin, ymin);
    float fb = orient(v2, v0, xmin, ymin);
    float fc = orient(v0, v1, xmin, ymin);

    for (float y = ymin; y <= ymax; y++) {
        // compute for whole row
        float alpha = fa;
        float beta = fb;
        float gamma = fc;

        for (float x = xmin; x <= xmax; x++) {
            if (alpha >= 0 && beta >= 0 && gamma >= 0){
                PutPixel(x, y, r, g, b);
            }
            // one up
            alpha += (v1->y - v2->y);
            beta += (v2->y - v0->y);
            gamma += (v0->y - v1->y);
        }
        // one up
        fa += (v2->x - v1->x);
        fb += (v0->x - v2->x);
        fc += (v1->x - v0->x);
    }
}
