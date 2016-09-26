/* Computer Graphics, Assignment, Volume rendering with cubes/points/isosurface
 *
 * Student name .... Steven Raaijmakers, Daan Meijers
 * Student email ... sjraaijmakers@gmail.com, daanmeijers@live.nl
 * Collegekaart .... 10804242, 10727167
 * Date ............
 * Comments ........ http://paulbourke.net/geometry/polygonise/
 *
 * (always fill in these fields before submitting!!)
 */

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "marching_tetrahedra.h"

/* Compute a linearly interpolated position where an isosurface cuts
   an edge between two vertices (p1 and p2), each with their own
   scalar value (v1 and v2) */

static vec3
interpolate_points(unsigned char isovalue, vec3 p1, vec3 p2, unsigned char v1, unsigned char v2)
{
    double mu = (isovalue - v1) / (v2 - v1);
    double x = p1.x + mu * (p2.x - p1.x);
    double y = p1.y + mu * (p2.y - p1.y);
    double z = p1.z + mu * (p2.z - p1.z);

    return v3_create(x, y, z);
//  return v3_add(v3_multiply(p1, 0.5), v3_multiply(p2, 0.5));
}

/* Using the given iso-value generate triangles for the tetrahedron
   defined by corner vertices v0, v1, v2, v3 of cell c.

   Store the resulting triangles in the "triangles" array.

   Return the number of triangles created (either 0, 1, or 2).

   Note: the output array "triangles" should have space for at least
         2 triangles.
*/




static int generate_tetrahedron_triangles(triangle *triangles, unsigned char isovalue, cell c, int *vertices){
    // max 2 triangles
    triangle t1, t2;
    // So only max 4 unique edges.
    vec3 edges[4];

    int i, j;
    int k = 0;

    // Loop over all 4 points in tetrahedron
    for(i = 0; i < 4; i++){
        for(j = 0; j < 4; j++){
            // Append point to edges[]
            if(i != j && (c.value[vertices[i]] > isovalue && c.value[vertices[j]] <= isovalue)){
                edges[k] = interpolate_points(isovalue, c.p[vertices[i]], c.p[vertices[j]], c.value[vertices[i]], c.value[vertices[j]]);
                k++;
            }
        }
    }
    // EEN DRIEHOEK
    if(k == 3){
        t1.p[0] = edges[0];
        t1.p[1] = edges[1];
        t1.p[2] = edges[2];
        triangles[0] = t1;
        return 1;
    }
    // hier gaat het mis
    else if(k == 4){
        t1.p[0] = edges[0];
        t1.p[1] = edges[1];
        t1.p[2] = edges[2];

        t2.p[0] = edges[0];
        t2.p[1] = edges[2];
        t2.p[2] = edges[3];

        triangles[0] = t1;
        triangles[1] = t2;
        return 2;
    }
    return 0;
}

/* Generate triangles for a single cell for the given iso-value. This function
   should produce at most 6 * 2 triangles (for which the "triangles" array should
   have enough space).

   Use calls to generate_tetrahedron_triangles().

   Return the number of triangles produced
*/

int
generate_cell_triangles(triangle *triangles, cell c, unsigned char isovalue)
{
    // The different tetrahedron in the cell
    int options[6][4] = {
        {0, 1, 3, 7},
        {0, 2, 6, 7},
        {0, 1, 5, 7},
        {0, 2, 3, 7},
        {0, 4, 5, 7},
        {0, 4, 6, 7}
    };

    int k = 0;
    for(int i = 0; i < 6; i++){
        k += generate_tetrahedron_triangles((triangles + k), isovalue, c, options[i]);
    }
    return k;
}
