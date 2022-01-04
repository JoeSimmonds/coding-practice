#include "unity.h"

int main() {
    int a = 1;
    TEST_ASSERT(a == 1); //this one will pass
    TEST_ASSERT(a == 2); //this one will fail
}