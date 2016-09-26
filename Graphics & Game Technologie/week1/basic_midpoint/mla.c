/* Computer Graphics, Assignment 1, Bresenham's Midpoint Line-Algorithm
 *
 * Filename ........ mla.c
 * Description ..... Midpoint Line Algorithm
 * Created by ...... Jurgen Sturm
 *
 * Student name ....
 * Student email ...
 * Collegekaart ....
 * Date ............
 * Comments ........
 *
 *
 * (always fill in these fields before submitting!!)
 */

#include "SDL.h"
#include "init.h"

/*
 * Midpoint Line Algorithm
 *
 * As you probably will have figured out, this is the part where you prove
 * your programming skills. The code in the mla function should draw a direct
 * line between (x0,y0) and (x1,y1) in the specified color.
 *
 * Until now, the example code below draws only a horizontal line between
 * (x0,y0) and (x1,y0) and a vertical line between (x1,y1).
 *
 * And here the challenge begins..
 *
 * Good luck!
 *
 *
 */
 int get_octant(int dx, int dy) {
     double rc = (double)dy/(double)dx;
     printf("%f\n",rc);
     if(rc >= 0 && rc <= 1 && dx >= 0){
       return 1;
     }
     else if(rc >= 1 && dx >= 0 && dy >= 0){
       return 2;
     }
     else if(rc <= -1 && dx <= 0 && dy >= 0){
       return 3;
     }
     else if(rc >= -1 && rc <= 0 && dx <= 0){
       return 4;
     }
     else if(rc <= 1 && rc >= 0 && dx <= 0){
       return 5;
     }
     else if(rc >= 1 && dx <= 0){
       return 6;
     }
     else if(rc <= -1 && dx >= 0){
       return 7;
     }
     else if(rc >= -1 && rc <= 0 && dx >= 0){
       return 8;
     }
     return 0;
}

void mla(SDL_Surface *s, int x0, int y0, int x1, int y1, Uint32 colour) {

    int octant = get_octant(x1-x0, y1-y0);

    float x_co, y_co;

    int y_start, x_start, x_max, tmp;

    x_co = 1;
    y_co = 0.5;
    y_start = y0;
    x_start = x0;
    x_max = x1;

    if(octant==1){

    }
    else if(octant==2){

    }
    else if(octant==3){

    }
    else if(octant==4){
        tmp = y0;
        y0 = y1;
        y1 = tmp;

        tmp = x0;
        x0 = x1;
        x1 = tmp;

        y_co = -0.5;
    }
    else if(octant==5){
        PutPixel(s, x1, y1, colour);
    }
    else if(octant==6){
        PutPixel(s, x1, y1, colour);
    }
    else if(octant==7){
    }

    // get d
    float a = (y0 - y1);
    float b = (x1 - x0);
    float c = (x0*y1 - x1*y0);
    float d = a*(x0 + x_co) + b*(y0 + y_co) + c;

    int y = y_start;
    for(int x = x_start; x <= x_max; x++){
        PutPixel(s, x, y, colour);
        if(d < 0){
            y += 1;
            d += b + a;
        }
        else {
            d += a;
        }
    }
}
