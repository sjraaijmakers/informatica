// Steven Raaijmakers, 10804242
// Checks if serial on euro-banknode is legit.

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <error.h>
#include <assert.h>
#include <math.h>
#include "debug.h"

// Returns sum of each digit in one number
// Takes latest number of digit, by using mod 10
int sums(long long num){
    int sum = 0;
    while(num > 0){
        sum = sum + num % 10;
        num = num / 10;
    }
    return sum;
}

// (BOOL) Checks if sum + ASCI of char can be devided by 9
int second(int sum, int c){
    if((sum + c) % 9 == 0){
        return 1;
    }
    return 0;
}

// Checksum Table
int checksum(char c){
    switch(c){
        case 'D' : // Estonia
        case 'M' : // Portugal
        case 'V' : // Spain
            return 4;
        case 'E' : // Slovakia
        case 'N' : // Austria
        case 'W' : // Denmark
            return 3;
        case 'F' : //  Malta
        case 'X' : // Germany
            return 2;
        case 'G' : // Cyprus
        case 'P' : // Netherlands
        case 'Y' : // Greece
            return 1;
        case 'H' : // Slovenia
        case 'Z' : // Belgium
            return 9;
        case 'L' : // Finland
        case 'U' : // France
            return 5;
        case 'S' : // Italia
        case 'J' : // UK
            return 7;
        case 'K' : // Sweden
        case 'T' : // Ireland
            return 6;
        case 'R' : // Luxembourg
            return 8;
        // Root can never be zero!
        default :
            return 0;
    }
}

int check_serial(char c, long long num) {
    assert(isupper(c));
    assert(num > 0);

    // Gets digitroot of number
    int sum = sums(num);
    int digitsum = sums(sum);
    int root = sums(digitsum);

    // Checks if both methods apply to number
    if(checksum(c) == root && second(sum, c)){
        return 1;
    }
    return 0;
}

int main(void) {
    long long num; // Need 64 bit integer to store 11 digit serial number.
    char c;

    // Example of debug printing
    printf("Start program.\n");
    DEBUG_PRINT(("Start program\n"));

    while (fscanf(stdin, "%c%lld\n", &c, &num) == 2) {
        printf("%c%lld: ", c, num);

        if (check_serial(c, num)) {
            printf("OK");
        } else {
            printf("FAILED");
        }
        printf("\n");
    }
    return EXIT_SUCCESS;
}
