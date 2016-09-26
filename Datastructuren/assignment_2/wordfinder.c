// Steven Raaijmakers, 10804242
// Program returns can find positions of words in a field
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <error.h>
#include <assert.h>
#include <math.h>
#include "debug.h"

#define GSIZE 50

// X and Y values per one point
typedef struct point {
    int x;
    int y;
} point;

// Checks letters in given direction
int check_direction(char *word, int letter, char grid[GSIZE][GSIZE], point old, point direction){
    // Point that needs to be checked = old + direction
    point new = {old.x + direction.x, old.y + direction.y};

    // Null-character: end of string.
    if(word[letter] == '\0'){
        return 1;
    }

    // Check if new point's letter has same value as letter searched for
    else if(tolower(word[letter]) == tolower(grid[new.x][new.y])){
        check_direction(word, letter + 1, grid, new, direction);
        return 1;
    }
    return 0;
}

// Check surrounding letters of point
int check_neighbours(char *word, char grid[GSIZE][GSIZE], point old){
    for (int i = old.x - 1; i <= old.x + 1; i++){
        for (int j = old.y - 1; j <= old.y + 1; j++){
            if (tolower(word[1]) == tolower(grid[i][j])){
                point new = {i, j};
                point direction = {(i - old.x), (j - old.y)};

                // Check in direction with given points
                if (check_direction(word, 2, grid, new, direction)){
                    return 1;
                }
            }
        }
    }
    return 0;
}

// Returns point where first letter of searched word occurs.
point occurence(char *word, char grid[GSIZE][GSIZE], point size){
    // Start reading grid at 1,1
    for (int i = 1; i <= size.x; i++){
        for (int j = 1; j <= size.y; j++){
            if (tolower(word[0]) == tolower(grid[i][j])){
                point new = {i, j};
                if (check_neighbours(word, grid, new)){
                    return new;
                }
            }
        }
    }
    // Returns 0, 0 if word doesn't occurs in grid.
    point of_no_return = {0, 0};
    return of_no_return;
}

int main(void) {
    // Read file and put into variables:
    int rows, cols;
    fscanf(stdin, "%d %d\n", &rows, &cols);
    point size = {rows, cols};

    // Grid will be initialised at 0
    char grid[GSIZE][GSIZE] = {{"0"}};

    // Letters will be placed in grid, on (1, 1)
    // So there'll be an empty row and column on the leftside of the grid
    // Because otherwise, letters in the corners couldn't be checked easily at 2
    for (int i = 1; i <= rows; i++){
        fscanf(stdin, "%s\n", &grid[i][1]);
    }

    int words_length;
    fscanf(stdin, "%d\n", &words_length);

    char words[GSIZE][GSIZE];
    for (int i = 0; i < words_length; i++){
        fscanf(stdin, "%s", &words[i][0]);
    }

    // Find points
    printf("Field (%d x %d): \n", rows, cols);
    for (int i = 1; i <= rows; i++){
        for (int j = 1; j <= cols; j++){
            printf("%c ", grid[i][j]);
        }
        printf("\n");
    }
    printf("\n");

    // Print words & location
    printf("Words: \n");
    for (int i = 0; i < words_length; i++){
        printf("%d. %s ", (i + 1), words[i]);
        point occ = occurence(words[i], grid, size);
        printf("(%d, %d) \n", occ.x, occ.y);
    }
    printf("\n");
}
