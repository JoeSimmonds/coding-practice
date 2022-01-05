#include "unity.h"
#include "Day3.h"

void setUp(void) {
    // set stuff up here
}

void tearDown(void) {
    // clean stuff up here
}

void readIntFromTextLine_reads_zero() {
    TEST_ASSERT_EQUAL_INT(0, readIntFromTextLine("000000000000", 12));
}

void readIntFromTextLine_reads_all_ones() {
    TEST_ASSERT_EQUAL_INT(4095, readIntFromTextLine("111111111111", 12));
}

void readIntFromTextLine_reads_powers_of_2() {
    TEST_ASSERT_EQUAL_INT(1     , readIntFromTextLine("000000000001", 12));
    TEST_ASSERT_EQUAL_INT(2     , readIntFromTextLine("000000000010", 12));
    TEST_ASSERT_EQUAL_INT(4     , readIntFromTextLine("000000000100", 12));
    TEST_ASSERT_EQUAL_INT(8     , readIntFromTextLine("000000001000", 12));
    TEST_ASSERT_EQUAL_INT(16    , readIntFromTextLine("000000010000", 12));
    TEST_ASSERT_EQUAL_INT(32    , readIntFromTextLine("000000100000", 12));
    TEST_ASSERT_EQUAL_INT(64    , readIntFromTextLine("000001000000", 12));
    TEST_ASSERT_EQUAL_INT(128   , readIntFromTextLine("000010000000", 12));
    TEST_ASSERT_EQUAL_INT(256   , readIntFromTextLine("000100000000", 12));
    TEST_ASSERT_EQUAL_INT(512   , readIntFromTextLine("001000000000", 12));
    TEST_ASSERT_EQUAL_INT(1024  , readIntFromTextLine("010000000000", 12));
    TEST_ASSERT_EQUAL_INT(2048  , readIntFromTextLine("100000000000", 12));
}

void resetCount_sets_totalCountToZero() {
    resetCounts(16);
    performCount(100);
    performCount(234);
    resetCounts(16);
    TEST_ASSERT_EQUAL_INT(0, getTotalCount());
    cleanUp();
}

void performCount_increments_the_total_count() {
    resetCounts(16);
    performCount(333);
    performCount(666);
    performCount(456);
    TEST_ASSERT_EQUAL_INT(3, getTotalCount());
    cleanUp();
}

int main() {
    UNITY_BEGIN();
    RUN_TEST(readIntFromTextLine_reads_zero);
    RUN_TEST(readIntFromTextLine_reads_all_ones);
    RUN_TEST(readIntFromTextLine_reads_powers_of_2);
    RUN_TEST(resetCount_sets_totalCountToZero);
    RUN_TEST(performCount_increments_the_total_count);
    return UNITY_END();
}