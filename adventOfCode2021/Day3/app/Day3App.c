#include <stdio.h>
#include <stdlib.h>
#include "Day3.h"

#define CHARS_PER_LINE 12
#define NEWLINE_LENGTH 1

int readNCharacters(FILE *fp, char* buffer, int n) {
    int charsRead = 0;
    for (int idx=0; idx<n; idx++) {
        int c = getc(fp);
        if (c != EOF) 
        {
            buffer[idx] = c;
            charsRead++;
        } else {
            buffer[idx] = 0; 
        }
    }
    buffer[n] = 0;
    return charsRead;
}

char readBuffer[50];

int main() {
    FILE *fp;
    fp = fopen("input.txt", "r");
    if (fp == NULL)
        exit(EXIT_FAILURE);

    resetCounts(12);
    readNCharacters(fp, readBuffer, CHARS_PER_LINE);
    while (readBuffer[0] != 0) {
        int value = readIntFromTextLine(readBuffer, CHARS_PER_LINE);
        performCount(value);
        readNCharacters(fp, readBuffer, NEWLINE_LENGTH);
        readNCharacters(fp, readBuffer, CHARS_PER_LINE);
    }
    printf("Power consumption is %i\n", getPowerConsumption());
    cleanUp();
    fclose(fp);
    return EXIT_SUCCESS;
}