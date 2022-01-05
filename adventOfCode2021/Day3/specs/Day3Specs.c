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

void majorityOnesAtBitPosition_is_false_for_all_positions_when_zeros_have_supplied() {
    resetCounts(8);
    performCount(0);
    for (int idx=0; idx<8; idx++) {
        char errMsg[256];
        sprintf(errMsg, "Expected FALSE but was TRUE at bit position %i", idx);
        TEST_ASSERT_FALSE_MESSAGE(majorityOnesAtBitPosition(idx), errMsg);
    }
    cleanUp();
}

void majorityOnesAtBitPosition_is_true_for_position_0_when_majority_1s_have_been_supplied() {
    resetCounts(8);
    performCount(0);
    performCount(1);
    performCount(1);
    TEST_ASSERT_TRUE_MESSAGE(majorityOnesAtBitPosition(0), "Expected TRUE but was FALSE at bit position 0");
    for (int idx=1; idx<8; idx++) {
        char errMsg[256];
        sprintf(errMsg, "Expected FALSE but was TRUE at bit position %i", idx);
        TEST_ASSERT_FALSE_MESSAGE(majorityOnesAtBitPosition(idx), errMsg);
    }
    cleanUp();
}

void majorityOnesAtBitPosition_is_false_for_position_0_when_minority_1s_have_been_supplied() {
    resetCounts(8);
    performCount(0);
    performCount(0);
    performCount(1);
    TEST_ASSERT_FALSE_MESSAGE(majorityOnesAtBitPosition(0), "Expected FALSE but was TRUE at bit position 0");
    for (int idx=1; idx<8; idx++) {
        char errMsg[256];
        sprintf(errMsg, "Expected FALSE but was TRUE at bit position %i", idx);
        TEST_ASSERT_FALSE_MESSAGE(majorityOnesAtBitPosition(idx), errMsg);
    }
    cleanUp();
}

void doCountsOnExampleData() {
    performCount(readIntFromTextLine("00100", 5));
    performCount(readIntFromTextLine("11110", 5));
    performCount(readIntFromTextLine("10110", 5));
    performCount(readIntFromTextLine("10111", 5));
    performCount(readIntFromTextLine("10101", 5));
    performCount(readIntFromTextLine("01111", 5));
    performCount(readIntFromTextLine("00111", 5));
    performCount(readIntFromTextLine("11100", 5));
    performCount(readIntFromTextLine("10000", 5));
    performCount(readIntFromTextLine("11001", 5));
    performCount(readIntFromTextLine("00010", 5));
    performCount(readIntFromTextLine("01010", 5));
}

void majorityOnesAtBitPosition_conforms_to_provided_example() {
    resetCounts(5);
    doCountsOnExampleData();
    TEST_ASSERT_TRUE_MESSAGE(majorityOnesAtBitPosition(4), "Expected TRUE but was FALSE at bit position 4");
    TEST_ASSERT_FALSE_MESSAGE(majorityOnesAtBitPosition(3), "Expected FALSE but was TRUE at bit position 3");
    TEST_ASSERT_TRUE_MESSAGE(majorityOnesAtBitPosition(2), "Expected TRUE but was FALSE at bit position 2");
    TEST_ASSERT_TRUE_MESSAGE(majorityOnesAtBitPosition(1), "Expected TRUE but was FALSE at bit position 1");
    TEST_ASSERT_FALSE_MESSAGE(majorityOnesAtBitPosition(0), "Expected FALSE but was TRUE at bit position 0");
    cleanUp();
}

void getGammaRate_conforms_to_provided_example() {
    resetCounts(5);
    doCountsOnExampleData();
    TEST_ASSERT_EQUAL_INT(22, getGammaRate());
    cleanUp();
}

void getEpsilonRate_conforms_to_provided_example() {
    resetCounts(5);
    doCountsOnExampleData();
    TEST_ASSERT_EQUAL_INT(9, getEpsilonRate());
    cleanUp();
}

void getPowerConsumption_conforms_to_provided_example() {
    resetCounts(5);
    doCountsOnExampleData();
    TEST_ASSERT_EQUAL_INT(198, getPowerConsumption());
    cleanUp();
}

int main() {
    UNITY_BEGIN();
    RUN_TEST(readIntFromTextLine_reads_zero);
    RUN_TEST(readIntFromTextLine_reads_all_ones);
    RUN_TEST(readIntFromTextLine_reads_powers_of_2);
    RUN_TEST(resetCount_sets_totalCountToZero);
    RUN_TEST(performCount_increments_the_total_count);
    RUN_TEST(majorityOnesAtBitPosition_is_false_for_all_positions_when_zeros_have_supplied);
    RUN_TEST(majorityOnesAtBitPosition_is_true_for_position_0_when_majority_1s_have_been_supplied);
    RUN_TEST(majorityOnesAtBitPosition_is_false_for_position_0_when_minority_1s_have_been_supplied);
    RUN_TEST(majorityOnesAtBitPosition_conforms_to_provided_example);
    RUN_TEST(getGammaRate_conforms_to_provided_example);
    RUN_TEST(getEpsilonRate_conforms_to_provided_example);
    RUN_TEST(getPowerConsumption_conforms_to_provided_example);
    return UNITY_END();
}