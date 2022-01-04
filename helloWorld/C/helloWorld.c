#include <stdio.h>

char* greet(char *buffer, char *name) {
    sprintf(buffer, "Hello %s!", name);
    return buffer;
}

int main() {
    char buf[50];
    greet(buf, "World");
    puts(buf);
    return 0;
}