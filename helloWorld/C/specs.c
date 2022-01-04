#include "unity.h"
#include "helloWorld.h"

void setUp(void) {
    // set stuff up here
}

void tearDown(void) {
    // clean stuff up here
}

void test_greet_says_hello() {
    char buf[50];
    TEST_ASSERT_EQUAL_STRING("Hello Bob!", greet(buf, sizeof(buf), "Bob"));
}

void test_when_buffer_would_overflow_greeting_is_truncated() {
    char buf[8];
    TEST_ASSERT_EQUAL_STRING("Hello B", greet(buf, sizeof(buf), "Bob"));
}

int main() {
    UNITY_BEGIN();
    RUN_TEST(test_greet_says_hello);
    RUN_TEST(test_when_buffer_would_overflow_greeting_is_truncated);
    return UNITY_END();
}