package com.a3pmtea.algorithms;

import java.util.Arrays;

/**
 * Exercise 4.1-5
 * Find max subarray in O(N) time.
 */

public class Exercise_4_1_5 {

    /**
     * @param a The given array
     * @return The start(inclusive) and end(exclusive) of the max subarray packed in int[].
     */
    public static int[] maxSubarray(int[] a) {
        int maxFrom = 0, maxTo = 1, max = a[0],
            from = 0, to = 1, sum = a[0];
        for (int i = 1; i < a.length; i++) {
            sum += a[i];

            if (sum > a[i]) {
                to = i + 1;
            } else {
                sum = a[i];
                from = i;
                to = i + 1;
            }

            if (sum > max) {
                max = sum;
                maxFrom = from;
                maxTo = to;
            }
        }
        return new int[]{maxFrom, maxTo};
    }

    public static void main(String[] args) {
        System.out.println(Arrays.toString(maxSubarray(new int[]{-1, -3, 2, -1, -5, 4, 3, -4, 5, 1, 0, -2, 2, 1})));
    }
}
