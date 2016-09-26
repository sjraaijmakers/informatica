/*
 * Parses all level files and puts them in a bunch of structs.
 * The resulting datastructures are kept as C-like as possible.
 */

#include "levels.h"

#include <string.h>
#include <stdlib.h>
#include <string>
#include <vector>
#include <iostream>
#include <fstream>

const char *levels_file = "levels.txt";
const char *levels_dir = "levels/";

// Debug prints, usefull when modifying this code or encountering bugs when
// adding/modifying levels.
const bool debug = true;


/*
 * Will read levels_file and load any level in there. It will return how many
 * levels were loaded, and an array with all levels will be returned via the
 * passed pointer.
 */
int load_levels(level_t **levels)
{
    std::ifstream f(levels_file);
    std::vector<level_t> levels_tmp;
    char line[512];
    while (f.getline(line, 512))
    {
        char *token = strtok(line, "\t ");
        if (!token || token[0] == '#' || token[0] == '\0')
            continue;

        if (debug)
            std::cout << "Loading level '" << token << "'" << std::endl;

        levels_tmp.push_back(load_level(token));
    }

    // Copy it to a C array.
    *levels = new level_t[levels_tmp.size()];
    std::copy(levels_tmp.begin(), levels_tmp.end(), *levels);

    return levels_tmp.size();
}

/*
 * Loads a level with a given name from the file into a level_t.
 *
 * Each level has a start and end point. Furtermore, there can be any number of
 * polygons in a level.
 *
 * Currently, any stray coordinate pair is treated as the start of a new
 * polygon.
 */
level_t load_level(const char *level_name)
{
    level_t level;
    level.num_polygons = 0;
    level.polygons = NULL;
    level.start.x = 0.f;
    level.start.y = 0.f;
    level.end.x = 0.f;
    level.end.y = 0.f;

    std::string filename = levels_dir + (std::string(level_name) + ".txt");
    std::ifstream file(filename.c_str());
    if (!file)
    {
        std::cerr << "Warning: level '" << filename << "' does not exist." <<
            std::endl;
        return level;
    }

    std::vector<poly_t> polys;
    std::vector<point_t> verts;
	std::vector<joint_t> joints;
    bool poly_is_dynamic = false;
    point_t poly_pos;
    bool push_poly;
    char line[512];
    while (file.getline(line, 512))
    {
        if (!line[0])
            continue;

        // Convert this entire line into tokens (separated by any whitespace).
        // This way, we can quite easily parse the line but also keep a bit more
        // flexibility than scanf/stringstream parsing.
        std::vector<std::string> tokens;
        char *token = strtok(line, "\t ");
        while (token)
        {
            if (token[0] == '#')
                break;
            tokens.push_back(std::string(token));
            token = strtok(NULL, "\t ");
        }

        if (tokens.size() == 0)
            continue;

        // We want to push ('finish') a polygon in almost all cases, except for
        // when this line contained an vertex of said polygon.
        push_poly = true;

        // Parse those tokens to some command. It either starts with a name (ie.
        // 'start', 'end') or is a vertex pair for a polygon.
        if (tokens.size() >= 3 && tokens[0] == "start")
        {
            // Start position
            level.start.x = atof(tokens[1].c_str());
            level.start.y = atof(tokens[2].c_str());
            if (debug)
                std::cout << "load_level: Found start: " << level.start.x << " "
                    << level.start.y << std::endl;
        }
        else if (tokens.size() >= 3 && tokens[0] == "end")
        {
            // End position
            level.end.x = atof(tokens[1].c_str());
            level.end.y = atof(tokens[2].c_str());
            if (debug)
                std::cout << "load_level: Found end: " << level.end.x << " " <<
                    level.end.y << std::endl;
        }
        /*
		else if (tokens.size() >= 6 && tokens[0] == "wheel")
		{
			joint_t wheel;
			wheel.joint_type = JOINT_WHEEL;
			wheel.objectA = atoi(tokens[1].c_str());
			wheel.objectB = atoi(tokens[2].c_str());
			wheel.anchor.x = atof(tokens[3].c_str());
			wheel.anchor.y = atof(tokens[4].c_str());
			wheel.wheel_axis.x = atof(tokens[5].c_str());
			wheel.wheel_axis.y = atof(tokens[6].c_str());
			joints.push_back(wheel);
			if (debug)
				std::cout << "load_level: wheel joint with objects " << wheel.objectA << " and " << wheel.objectB << " at (" << wheel.anchor.x << "," << wheel.anchor.y << ") axis (" << wheel.wheel_axis.x << "," << wheel.wheel_axis.y << ")" << std::endl;
		}
        */
		else if (tokens.size() >= 5 && tokens[0] == "revolute")
		{
			joint_t revolute;
			revolute.joint_type = JOINT_REVOLUTE;
			revolute.objectA = atoi(tokens[1].c_str());
			revolute.objectB = atoi(tokens[2].c_str());
			revolute.anchor.x = atof(tokens[3].c_str());
			revolute.anchor.y = atof(tokens[4].c_str());
			joints.push_back(revolute);
			if (debug)
				std::cout << "load_level: revolute joint with objects " << revolute.objectA << " and " << revolute.objectB << " at (" << revolute.anchor.x << "," << revolute.anchor.y << ")" << std::endl;
        }
		else if (tokens.size() >= 12 && tokens[0] == "pulley")
		{
			joint_t pulley;
			pulley.joint_type = JOINT_PULLEY;
			pulley.objectA = atoi(tokens[1].c_str());
			pulley.objectB = atoi(tokens[2].c_str());
			pulley.pulley.ground1.x = atof(tokens[3].c_str());
			pulley.pulley.ground1.y = atof(tokens[4].c_str());
			pulley.pulley.ground1.x = atof(tokens[5].c_str());
			pulley.pulley.ground1.y = atof(tokens[6].c_str());
			pulley.anchor.x = atof(tokens[7].c_str());
			pulley.anchor.y = atof(tokens[8].c_str());
			pulley.pulley.anchor2.x = atof(tokens[9].c_str());
			pulley.pulley.anchor2.y = atof(tokens[10].c_str());
			pulley.pulley.ratio = atof(tokens[11].c_str());
			joints.push_back(pulley);
			if (debug)
				std::cout << "load_level: pulley joint with objects " << pulley.objectA << " and " << pulley.objectB << std::endl;
		}
        else if (tokens.size() >= 1 && tokens[0] == "poly")
        {
            // New polygon
            // Vertices follow later as seperate lines

			// Push old one if any
			if (verts.size() > 0)
			{
				if (debug)
					std::cout << "load_level: Pushing poly with " << verts.size() <<
					" verts." << std::endl;
                poly_t poly;
                poly.is_dynamic = poly_is_dynamic;
                poly.position = poly_pos;
                poly.num_verts = verts.size();
                poly.verts = new point_t[verts.size()];
                std::copy(verts.begin(), verts.end(), poly.verts);
                verts.clear();
                polys.push_back(poly);
			}

            if (tokens.size() >= 4)
            {
                poly_is_dynamic = tokens[1] == "dynamic";
                poly_pos.x = atof(tokens[2].c_str());
                poly_pos.y = atof(tokens[3].c_str());
            }
            else
            {
                poly_is_dynamic = false;
                poly_pos.x = poly_pos.y = 0;
            }
            if (debug)
                std::cout << "load_level: Starting new poly" << std::endl;

        }
        else if (tokens.size() >= 2)
        {
            // Vertex pair
            point_t p;
            p.x = atof(tokens[0].c_str());
            p.y = atof(tokens[1].c_str());
            verts.push_back(p);
            push_poly = false;
            if (debug)
                std::cout << "load_level: Found vert: " << p.x << " " << p.y <<
                    std::endl;
        }
        else
        {
            if (debug)
                std::cerr << "load_level: unknown line: '" << line << std::endl;
        }

        // Submit the polygon we were working on to the list of polygons when it
        // is 'finished'.
        if (push_poly && verts.size() > 0)
        {
            if (debug)
                std::cout << "load_level: Pushing poly with " << verts.size() <<
                    " verts." << std::endl;
            poly_t poly;
            poly.is_dynamic = poly_is_dynamic;
            poly.position = poly_pos;
            poly.num_verts = verts.size();
            poly.verts = new point_t[verts.size()];
            std::copy(verts.begin(), verts.end(), poly.verts);
            verts.clear();
            polys.push_back(poly);
        }
    }

    // Also push the last polygon, if any.
    if (verts.size() > 0)
    {
        if (debug)
            std::cout << "load_level: Pushing poly with " << verts.size() <<
                " verts." << std::endl;
        poly_t poly;
        poly.is_dynamic = poly_is_dynamic;
        poly.position = poly_pos;
        poly.num_verts = verts.size();
        poly.verts = new point_t[verts.size()];
        std::copy(verts.begin(), verts.end(), poly.verts);
        polys.push_back(poly);
    }

    // Finally, now that we have all polygons for this level, convert that list
    // into the final structure.
    level.num_polygons = polys.size();
    level.polygons = new poly_t[polys.size()];
    std::copy(polys.begin(), polys.end(), level.polygons);

	level.num_joints = joints.size();
	level.joints = new joint_t[joints.size()];
	std::copy(joints.begin(), joints.end(), level.joints);

    return level;
}

