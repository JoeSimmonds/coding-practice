#include <stdio.h>

char* greet(char *buffer, size_t bufsize, char *name) {
    snprintf(buffer, bufsize, "Hello %s!", name);
    return buffer;
}