#include <stdlib.h>
#include <stdio.h>
#include "Day3.h"

#define ZERO 48
#define ONE 49
#define NEWLINE 10
#define CARRIAGE_RETURN 13

int totalCount = 0;
int *bitCounts = NULL;
int _noOfBits = 32;

void cleanUp() {
    if (bitCounts != NULL) free(bitCounts);
    bitCounts = NULL;
    return;
}

void resetCounts(int noOfBits) {
    totalCount = 0;
    cleanUp();
    bitCounts = malloc(noOfBits * sizeof(int));
    _noOfBits = noOfBits;
    for (int idx=0; idx<noOfBits; idx++) bitCounts[idx] = 0;
    return;
}

void performCount(int input) {
    int mask = 1;
    for (int bit=0; bit<_noOfBits; bit++) {
        if ((input & mask) == mask) {
            bitCounts[bit] = bitCounts[bit] + 1;
        }
        mask = mask << 1;
    }
    totalCount++;
    return;
}

int getTotalCount() {return totalCount;}

bool majorityOnesAtBitPosition(int position) {
    return bitCounts[position] > (totalCount/2);
}

int getGammaRate() {
    int gamma = 0;
    for (int idx=0; idx<_noOfBits; idx++) {
        if (majorityOnesAtBitPosition(idx)) {
            gamma = gamma + (1 << idx);
        }
    }
    return gamma;
}

int getEpsilonRate() {
    int epsilon = 0;
    for (int idx=0; idx<_noOfBits; idx++) {
        if (!majorityOnesAtBitPosition(idx)) {
            epsilon = epsilon + (1 << idx);
        }
    }
    return epsilon;
}

int getPowerConsumption() {
    return getGammaRate() * getEpsilonRate();
}

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