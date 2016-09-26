/* Computer Graphics, Assignment, Bezier curves
 * Filename ........ bezier.c
 * Description ..... Bezier curves
 * Date ............ 22.07.2009
 * Created by ...... Paul Melis
 *
 * Student name .... Steven Raaijmakers & Daan Meijers
 * Student email ... sjraaijmakers@gmail.com daanmeijers@live.nl
 * Collegekaart .... 10804242 10727167
 * Date ............ 19-02-2016
 * Comments ........
 * https://www.particleincell.com/2013/cubic-line-intersection/
 * https://en.wikipedia.org/wiki/Binary_search_algorithm
 */

#include <math.h>
#include "bezier.h"
#include <stdio.h>

/* Given a Bezier curve defined by the 'num_points' control points
 * in 'p' compute the position of the point on the curve for parameter
 * value 'u'.
 *
 * Return the x and y values of the point by setting *x and *y,
 * respectively.
 */

// Compute n!
int fac(int n){
    int tmp = 1;
    for(int i = 1; i <= n; i++){
        tmp *= i;
    }
    return tmp;
}

// Get binomial of n,k
float binomial(int n, int k){
    return fac(n) / (fac(k) * fac(n - k));
}

// Get bernstein product
float bernstein(int n, int i, float u){
    return binomial(n, i) * pow(u, i) * pow(1 - u, n - i);
}

// Evaluate bezier curve via num_points
void evaluate_bezier_curve(float *x, float *y, control_point p[], int num_points, float u) {
    *x = 0.0;
    *y = 0.0;
    for(int i = 0; i < num_points; i++){
        float tmp = bernstein(num_points - 1, i, u);
        *x += tmp * p[i].x;
        *y += tmp * p[i].y;
    }
}

/* Draw a Bezier curve defined by the control points in p[], which
 * will contain 'num_points' points.
 *
 * Draw the line segments you compute directly on the screen
 * as a single GL_LINE_STRIP. This is as simple as using
 *
 *      glBegin(GL_LINE_STRIP);
 *      glVertex2f(..., ...);
 *      ...
 *      glEnd();
 *
 * DO NOT SET ANY COLOR!
 *
 * The 'num_segments' parameter determines the "discretization" of the Bezier
 * curve and is the number of straight line segments that should be used
 * to approximate the curve.
 *
 * Call evaluate_bezier_curve() to compute the necessary points on
 * the curve.
 */

 // Draw lines based on beziercurve points
void
draw_bezier_curve(int num_segments, control_point p[], int num_points)
{
    float x, y;
    glBegin(GL_LINE_STRIP);
    for(int i = 0; i < num_segments + 1; i++){
        evaluate_bezier_curve(&x, &y, p, num_points, i * (1 / (float)num_segments));
        glVertex2f(x, y);
    }
    glEnd();
}

/* Find the intersection of a cubic Bezier curve with the line X=x.
   Return 1 if an intersection was found and place the corresponding y
   value in *y.
   Return 0 if no intersection exists.
*/
int intersect_cubic_bezier_curve(float *y, control_point p[], float x){
    // Deze lijn kijkt of de tijdlijn wel tussen de controlepunten ligt.
    if((x < p[0].x && x < p[3].x) || (x > p[0].x && x > p[3].x)){
        return 0;
    }

    float curtime = x;
    x = -1.0;

    float error = pow(10, -3);
    float u = 0;
    float incr = 1;
    int num_points = 4;

    /* Om de intersection te vinden van een bezier curve met een marge van 10^-3
     * tov de tijdlijn gebruiken we Binary Search.
     */
    while(fabs(x - curtime) > error){
        evaluate_bezier_curve(&x, y, p, num_points, u);
        if(x > curtime){
            incr /= 2;
            u -= incr;
        }
        else{
            incr /= 2;
            u += incr;
        }
    }
    // succes
    return 1;
}
