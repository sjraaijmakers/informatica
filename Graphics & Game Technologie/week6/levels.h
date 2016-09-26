#pragma once

/*
 * Represents a tuple of 2 floats.
 */
typedef struct {
    float x, y;
} point_t;

/*
* A joint can be of any of these types.
*/
enum joint_type_t
{
	JOINT_REVOLUTE,
    //JOINT_WHEEL,
    JOINT_PULLEY
};

typedef struct {
    bool is_dynamic;
    point_t position;
    float radius;
} circle_t;

/*
 * A joint (relation between two bodies) has a type, two bodies (given as the
 * index in the polygons array), an anchor point and some joint-specific
 * settings.
 */
typedef struct {
	joint_type_t joint_type;
	unsigned objectA, objectB;

	point_t anchor;

	union
	{
		// A wheel has an axis (usually up-vector)
		//point_t wheel_axis;

        struct
        {
            point_t ground1;
            point_t ground2;
            // anchor1 is the normal anchor field
            point_t anchor2;
            float ratio;
        } pulley;

	};

} joint_t;

/*
 * A polygon has a bunch of vertices and can be either static of dynamic.
 * The vertices are relative to the position; the position can be 0,0 in which
 * case the vertices are in absolute coordinates. Box2D all handles this
 * internally, but may not like it if all positions are at 0,0.
 */
typedef struct {
    bool is_dynamic;
    point_t position;
    unsigned int num_verts;
    point_t *verts;
} poly_t;

/*
 * Represents a level with start and end positions, and a bunch of polygons.
 */
typedef struct {
    // A start and finish point for the player
    point_t start, end;

	// Every level has a number of objects (bodies with single shape/fixture)
    unsigned int num_polygons;
    poly_t *polygons;

	// These objects can have joints between them
	unsigned int num_joints;
	joint_t *joints;
} level_t;


int load_levels(level_t **levels);
level_t load_level(const char *level_name);
