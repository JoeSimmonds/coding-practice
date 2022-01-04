#include <stdio.h>
#include "helloWorld.h"

int main() {
    char buf[25];
    greet(buf, sizeof(buf), "World");
    puts(buf);
    return 0;
}