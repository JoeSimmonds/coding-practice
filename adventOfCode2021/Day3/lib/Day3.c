#include <stdlib.h>

#define ZERO 48
#define ONE 49
#define NEWLINE 10
#define CARRIAGE_RETURN 13

int totalCount = 0;
int *bitCounts = NULL;

void cleanUp() {
    if (bitCounts != NULL) free(bitCounts);
    bitCounts = NULL;
    return;
}

void resetCounts(int noOfBits) {
    totalCount = 0;
    cleanUp();
    bitCounts = malloc(noOfBits * sizeof(int));
    return;
}

void performCount(int input) {
    totalCount++;
    return;
}

int getTotalCount() {return totalCount;}

int readIntFromTextLine(char *line, int noOfCharacters) {
    int value = 0;
    int bit = 0;
    for (bit=0; bit < noOfCharacters; bit++) {
        switch (line[bit]) {
        case ZERO:
            value = value << 1;
            break;
        case ONE:
            value = value << 1;
            value = value + 1;
            break;
        default:
            return -4;
            break;
        }
    }
    return value;
}