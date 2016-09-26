/* Computer Graphics and Game Technology, Assignment Box2D game
 *
 * Student name .... Steven Raaijmakers, Daan Meijers
 * Student email ... sjraaijmakers@gmail.com, daanmeijers@live.nl
 * Collegekaart .... 10804242, 10727167
 * Date ............ 18 maart 2016
 * Comments ........ jo
 *
 *
 * (always fill in these fields before submitting!!)
 */

#include <cstdio>

#include <GL/gl.h>
#include <GL/glut.h>
#include <Box2D/Box2D.h>
#include <math.h>

#include "levels.h"

unsigned int reso_x = 800, reso_y = 600; // Window size in pixels
const float world_x = 8.f, world_y = 6.f; // Level (world) size in meters

int last_time;
int frame_count;

// Information about the levels loaded from files will be available in these.
unsigned int num_levels;
level_t *levels;

// Globals
b2World* world;
int lvl;
b2Body* bodies[100]; // allocate big array (just in case)
bool play = false;

// Points which represent a polygon drawn by user
point_t points[4];
int append;

// Check if point is in circle
bool is_in_circle(float center_x, float center_y, float r, float x, float y){
    float sq = pow((center_x - x), 2) + pow((center_y - y), 2);
    return sq <= pow(r, 2);
}

// Create body for circle t
void create_body_circle(b2World* w, circle_t t){
    b2BodyDef def;
    if(t.is_dynamic){
        def.type = b2_dynamicBody;
    }
    def.position.Set(t.position.x, t.position.y);
    b2Body* ball = world->CreateBody(&def);

    b2CircleShape circle;
    circle.m_radius = t.radius;

    b2FixtureDef fixtureDef;
    fixtureDef.shape = &circle;
    fixtureDef.density = 1.0f;
    fixtureDef.friction = 10.0f;
    ball->CreateFixture(&fixtureDef);
}

// Create polygon t and add to w
void create_body_polygon(b2World* w, poly_t t, int index){
    b2BodyDef def;
    def.position.Set(t.position.x, t.position.y);
    if(t.is_dynamic){
        def.type = b2_dynamicBody;
    }

    b2Body* body = w->CreateBody(&def);

    b2Vec2 vertices[4]; // polygon will have max 4 vecs
    for(unsigned int i = 0; i < t.num_verts; i++){
        vertices[i].Set(t.verts[i].x, t.verts[i].y);
    }
    b2PolygonShape polygon;
    polygon.Set(vertices, t.num_verts);

    b2FixtureDef fixtureDef;
    fixtureDef.shape = &polygon;
    fixtureDef.density = 1.0f;
    fixtureDef.friction = 1.0f;

    body->CreateFixture(&fixtureDef);

    if(index >= 0){
        bodies[index] = body;
    }
}

// Transform point_t to b2Vec2
b2Vec2 p2v(point_t t){
    b2Vec2 v;
    v.Set(t.x, t.y);
    return v;
}

// Convert pixels to point in meters
point_t convert(point_t pix){
    pix.x = pix.x / (reso_x / world_x);
    pix.y = world_y - (pix.y / (reso_y / world_y));
    return pix;
}

// Create point_t
point_t create_point(float x, float y){
    point_t t;
    t.x = x;
    t.y = y;
    return t;
}

// Load level
void load_world(unsigned int level){
    // reset some globals
    play = false;
    append = 0;
    memset(points, 0, 4*sizeof(point_t));
    lvl = level;

    if (level >= num_levels)
    {
        printf("Warning: level %d does not exist.\n", level);
        return;
    }

    // Create world and add gravity
    b2Vec2 gravity(0.0f, -10.0f);
    world = new b2World(gravity);

    // Bal maken (beweegt)
    circle_t ball;
    ball.is_dynamic = true;
    ball.position.x = levels[lvl].start.x;
    ball.position.y = levels[lvl].start.y;
    ball.radius = 0.3f;
    create_body_circle(world, ball);

    // Create all polygons
    for(unsigned int i = 0; i < levels[level].num_polygons; i++){
        poly_t current = levels[level].polygons[i];
        create_body_polygon(world, current, i);
    }

    // Create all joints
    for(unsigned int i = 0; i < levels[level].num_joints; i++){
        joint_t current = levels[level].joints[i];

        // Revolute joint
        if(current.joint_type == JOINT_REVOLUTE){
            b2RevoluteJointDef jointDef;
            jointDef.Initialize(bodies[current.objectA], bodies[current.objectB], p2v(current.anchor));
            world->CreateJoint(&jointDef);
        }
        // Pulley joint
        else if(current.joint_type == JOINT_PULLEY){
            b2PulleyJointDef jointDef;
              jointDef.Initialize(bodies[current.objectA], bodies[current.objectB], p2v(current.pulley.ground1), p2v(current.pulley.ground1), p2v(current.anchor), p2v(current.pulley.anchor2), current.pulley.ratio);
              world->CreateJoint(&jointDef);
        }
    }
}

/*
 * Called when we should redraw the scene (i.e. every frame).
 * It will show the current framerate in the window title.
 */
void draw(void)
{
    int time = glutGet(GLUT_ELAPSED_TIME);
    int frametime = time - last_time;
    frame_count++;

    // Clear the buffer
    glColor3f(0, 0, 0);
    glClear(GL_COLOR_BUFFER_BIT);

    // Dynamic objects
    if(play){
        world->Step(1.0f / 60.0f, 6, 2);
    }

    // Draw bodys
    b2Body* b = world->GetBodyList();
    while(b){
        // Draw fixtures from body
        b2Fixture* f = b->GetFixtureList();
        b2Vec2 pos = b->GetPosition();

        // Polygon
        if(f->GetType() == b2Shape::e_polygon){
            b2PolygonShape* s = (b2PolygonShape*)f->GetShape();

            glColor3f(0, 1, 0); // green
            if (b->GetType() == b2_dynamicBody){
                glColor3f(0.7, 0.2, 0.2); // brownish
            }
            glBegin(GL_POLYGON);
            for(int i = 0; i < s->GetVertexCount(); i++){
                b2Vec2 e = s->GetVertex(i);
                b2Vec2 v = b->GetWorldPoint(e);
                glVertex2f(v.x, v.y);
            }
            glEnd();
        }
        // Circle
        else if(f->GetType() == b2Shape::e_circle){
            b2CircleShape* s = (b2CircleShape*)f->GetShape();
            int am = 10 / s->m_radius; // triangles per fan
            glBegin(GL_TRIANGLE_FAN);
            glColor3f(1, 0, 0); // red
            glVertex2f(pos.x, pos.y);
            for(int i = 0; i <= am;i++) {
                glVertex2f(
                    s->m_radius * cosf(2.0 * M_PI * float(i) / float(am)) + pos.x,
                    s->m_radius * sinf(2.0 * M_PI * float(i) / float(am)) + pos.y
                );
            }
            glEnd();
            // If circle touches endpoint
            if(is_in_circle(pos.x, pos.y, s->m_radius, levels[lvl].end.x, levels[lvl].end.y)){
                load_world(++lvl);
            }
        }
        b = b->GetNext();
    }

    // Draw end point
    float size = 0.05f;
    float size2 = -0.05f;

    glColor3f(0, 0, 1); // blue
    glBegin(GL_POLYGON);
    glColor3f(1.0, 0.0, 0.0);
    glVertex2f(levels[lvl].end.x - size, levels[lvl].end.y - size);
    glVertex2f(levels[lvl].end.x - size, levels[lvl].end.y + size);
    glVertex2f(levels[lvl].end.x + size, levels[lvl].end.y + size);
    glVertex2f(levels[lvl].end.x + size, levels[lvl].end.y - size);

    glColor3f(1.0, 0.0, 0.0);
    glVertex2f(levels[lvl].end.x - size2, levels[lvl].end.y - size2);
    glVertex2f(levels[lvl].end.x - size2, levels[lvl].end.y + size2);
    glVertex2f(levels[lvl].end.x + size2, levels[lvl].end.y + size2);
    glVertex2f(levels[lvl].end.x + size2, levels[lvl].end.y - size2);
    glEnd();

    glShadeModel(GL_FLAT);
    glBegin(GL_POINTS);
    glColor3f(1.0, 0.0, 0.0);
    glColor3f(0.0, 1.0, 0.0);
    glColor3f(0.0, 0.0, 1.0);
    glVertex3i(0, 0, 0);
    glVertex3i(1, 0, 0);
    glVertex3i(2, 0, 0);
    glEnd();

    // Show rendered frame
    glutSwapBuffers();

    // Display fps in window title.
    if (frametime >= 1000)
    {
        char window_title[128];
        snprintf(window_title, 128,
                "Box2D: %f fps, level %d/%d",
                frame_count / (frametime / 1000.f), -1, num_levels);
        glutSetWindowTitle(window_title);
        last_time = time;
        frame_count = 0;
    }
}

/*
 * Called when window is resized. We inform OpenGL about this, and save this
 * for future reference.
 */
void resize_window(int width, int height)
{
    glViewport(0, 0, width, height);
    reso_x = width;
    reso_y = height;
}

/*
 * Called when the user presses a key.
 */
void key_pressed(unsigned char key, int x, int y)
{
    switch (key)
    {
        case 27: // Esc
        case 'q':
            exit(0);
            break;
        case 'p':
            if(play){
                play = false;
            }
            else{
                play = true;
            }
            break;
        case 'r':
            load_world(lvl);
            break;
        case '3':
            load_world(3);
            break;
        case '4':
            load_world(4);
            break;
        // Add any keys you want to use, either for debugging or gameplay.
        default:
            break;
    }
}

void mouse_clicked(int button, int state, int x, int y){
    if(button == 0){
        if(state == 1){ // on mouse release
            point_t tmp = convert(create_point(x, y));
            if(points[append-1].x != tmp.x && points[append-1].y != tmp.y){
                points[append] = tmp;
                append++;
            }
            if(append == 4){ // 4 clicks = new polygon
                poly_t t;
                t.is_dynamic = true;
                t.position = create_point(0, 0);
                t.num_verts = 4;
                t.verts = points;

                create_body_polygon(world, t, -1);

                append = 0;
            }
        }
    }
}

/*
 * Called when the mouse is moved to a certain given position.
 */
void mouse_moved(int x, int y)
{
}


int main(int argc, char **argv)
{
    // Create an OpenGL context and a GLUT window.
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGBA);
    glutInitWindowSize(reso_x, reso_y);
    glutCreateWindow("Box2D");

    // Bind all GLUT events do callback function.
    glutDisplayFunc(&draw);
    glutIdleFunc(&draw);
    glutReshapeFunc(&resize_window);
    glutKeyboardFunc(&key_pressed);
    glutMouseFunc(&mouse_clicked);
    glutMotionFunc(&mouse_moved);
    glutPassiveMotionFunc(&mouse_moved);

    // Initialise the matrices so we have an orthogonal world with the same size
    // as the levels, and no other transformations.
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(0, world_x, 0, world_y, 0, 1);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();

    // Read the levels into a bunch of structs.
    num_levels = load_levels(&levels);
    printf("Loaded %d levels.\n", num_levels);

    // Load the first level (i.e. create all Box2D stuff).
    load_world(0);

    last_time = glutGet(GLUT_ELAPSED_TIME);
    frame_count = 0;
    glutMainLoop();

    return 0;
}
