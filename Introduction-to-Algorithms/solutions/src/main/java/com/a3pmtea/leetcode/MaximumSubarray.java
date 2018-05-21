package com.a3pmtea.leetcode;

/**
 * Leetcode #53
 */

public class MaximumSubarray {
    public int maxSubArray(int[] nums) {
        int max = nums[0], sum = nums[0];
        for (int i = 1; i < nums.length; i++) {
            sum += nums[i];

            if (sum < nums[i]) {
                sum = nums[i];
            }

            if (sum > max) {
                max = sum;
            }
        }
        return max;
    }

    public static void main(String[] args) {
        MaximumSubarray test = new MaximumSubarray();
        System.out.println(test.maxSubArray(new int[]{-2, 1, -3, 4, -1, 2, 1, -5, 4}));
    }
}
