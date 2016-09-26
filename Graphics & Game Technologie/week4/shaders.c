/* Computer Graphics and Game Technology, Assignment Ray-tracing
 *
 * Student name .... Steven Raaijmakers, Daan Meijers
 * Student email ... sjraaijmakers@gmail.com, daanmeijers@live.nl
 * Collegekaart .... 10804242, 10727167
 * Date ............ 4 maart 2016
 * Comments ........ ALles goed?!
 *
 *
 * (always fill in these fields before submitting!!)
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "shaders.h"
#include "perlin.h"
#include "v3math.h"
#include "intersection.h"
#include "scene.h"
#include "quat.h"
#include "constants.h"

// shade_constant()
//
// Always return the same color. This shader does no real computations
// based on normal, light position, etc. As such, it merely creates
// a "silhouette" of an object.

vec3
shade_constant(intersection_point ip)
{
    return v3_create(1, 0, 0);
}

// Matte shader
vec3 shade_matte(intersection_point ip){
    // Declare
    vec3 l;
    float cn;
    float c = scene_ambient_light;

    vec3 offset = v3_multiply(ip.n, 0.001);

    // For each lightsource, compute c
    for(int i = 0; i < scene_num_lights; i++){
        l = v3_normalize(v3_subtract(scene_lights[i].position, ip.p));
        // When ray doesnt cross shadow, add to c
        if(!shadow_check(v3_add(ip.p, offset), l)){
            cn = fmaxf(0, v3_dotprod(l, ip.n));
            c += scene_lights[i].intensity * cn;
        }
    }
    // Return c in tripple (grayscale)
    return v3_create(c, c, c);
}

// Blinn-Phong shading
vec3 shade_blinn_phong(intersection_point ip){
    // Defined by instruction
    float kd = 0.8;
    float ks = 0.5;
    int alpha = 50;

    vec3 cd = v3_create(1, 0, 0);
    vec3 cs = v3_create(1, 1, 1);

    // Declare
    float a = 0;
    float b = 0;
    float cn, cm;
    vec3 l, h;

    vec3 offset = v3_multiply(ip.n, 0.01);

    // For each light, compute sumation (a & b)
    for(int i = 0; i < scene_num_lights; i++){
        l = v3_normalize(v3_subtract(scene_lights[i].position, ip.p));        if(!shadow_check(v3_add(ip.p, offset), l)){
            cn = fmaxf(0, v3_dotprod(ip.n, l));
            a += scene_lights[i].intensity * cn;

            h = v3_normalize(v3_add(ip.i, l));
            cm = pow(v3_dotprod(ip.n, h), alpha);
            b += scene_lights[i].intensity * cm;
        }
    }
    // End formula to get cf
    vec3 term1 = v3_multiply(cd, fminf(1.0, scene_ambient_light + (kd * a)));
    vec3 term2 = v3_multiply(v3_multiply(cs, ks), fminf(1, b));
    vec3 c = v3_add(term1, term2);

    // Max value of row can be 1 (white)
    c.x = fminf(1.0, c.x);
    c.y = fminf(1.0, c.y);
    c.z = fminf(1.0, c.z);

    return c;
}

// Reflections added
vec3 shade_reflection(intersection_point ip){
    vec3 matte, reflections, r;

    // Get matte color
    matte = shade_matte(ip);

    // Get reflections
    r = v3_subtract(v3_multiply(v3_multiply(ip.n, 2), v3_dotprod(ip.i, ip.n)), ip.i);

    vec3 offset = v3_multiply(ip.n, 0.001);

    ip.ray_level += 1;
    reflections = ray_color(ip.ray_level, v3_add(ip.p, offset), r);

    // matte : reflections, 1 : 3
    return v3_add(v3_multiply(matte, 0.75), v3_multiply(reflections, 0.25));
}

// Returns the shaded color for the given point to shade.
// Calls the relevant shading function based on the material index.
vec3
shade(intersection_point ip)
{
  switch (ip.material)
  {
    case 0:
      return shade_constant(ip);
    case 1:
      return shade_matte(ip);
    case 2:
      return shade_blinn_phong(ip);
    case 3:
      return shade_reflection(ip);
    default:
      return shade_constant(ip);

  }
}

// Determine the surface color for the first object intersected by
// the given ray, or return the scene background color when no
// intersection is found
vec3
ray_color(int level, vec3 ray_origin, vec3 ray_direction)
{
    intersection_point  ip;

    // If this ray has been reflected too many times, simply
    // return the background color.
    if (level >= 3)
        return scene_background_color;

    // Check if the ray intersects anything in the scene
    if (find_first_intersection(&ip, ray_origin, ray_direction))
    {
        // Shade the found intersection point
        ip.ray_level = level;
        return shade(ip);
    }

    // Nothing was hit, return background color
    return scene_background_color;
}
