package com.a3pmtea.algorithms;

/**
 * Exercise 2.3-7
 * Describe a O(NlgN) time algorithm that, given a set S of n integers and another
 * integer x, determines whether or not there exist two elements in S whose sum is
 * exactly x.
 *
 * Solution:
 * 1. Sort S, which takes O(NlgN) time
 * 2. For each element e in S, search for x - e in S with binary search, which takes O(NlgN) time
 *
 */

public class Exercise_2_3_7 {

    public static boolean findSum(int[] a, int n) {
        if (a.length < 2) return false;
        mergeSort(a, 0, a.length);
        for (int i = 0; i < a.length; i++) {
            if (binarySearch(a, n - a[i], i + 1, a.length) > 0)
                return true;
        }
        return false;
    }

    /**
     * Sort [start, end) part of an array using merge sort.
     */
    private static void mergeSort(int[] a, int start, int end) {
        if (end - start < 2) return;

        int mid = (start + end) / 2;
        mergeSort(a, start, mid);
        mergeSort(a, mid, end);
        merge(a, start, mid, end);
    }

    /**
     * Merge an array with two sorted parts into one sorted array.
     * The first part is [start, mid), and the second part is [mid, end).
     */
    private static void merge(int[] a, int start, int mid, int end) {
        if (start >= mid || mid >= end) return;

        int len = end - start;
        int[] tmp = new int[len];
        System.arraycopy(a, start, tmp, 0, len);

        int i = 0, j = mid - start, k = start;
        while (i < mid - start && j < end - start) {
            if (tmp[i] <= tmp[j]) {
                a[k++] = tmp[i++];
            } else {
                a[k++] = tmp[j++];
            }
        }
        while (i < mid - start) a[k++] = tmp[i++];
        while (j < end - start) a[k++] = tmp[j++];
    }

    /**
     * Search for a given element from start(inclusive) to end(exclusive) in a sorted array.
     * @return Index in the array or -1 if absent.
     */
    private static int binarySearch(int[] a, int n, int start, int end) {
        if (end - start <= 0) return -1;

        int mid;
        while (start < end) {
            mid = (start + end) / 2;
            if (a[mid] == n) return mid;
            else if (a[mid] > n) end = mid;
            else start = mid + 1;
        }
        return -1;
    }

    public static void main(String[] args) {
        int[] a = new int[] { 2, 19, 5, 3, 11, 7, 17, 13 };
        int[] b = new int[] { 6, 3, 6, 7, 9, 10, 2 };
        int[] c = new int[] { 2 };
        int[] d = new int[] {};
        System.out.println(Exercise_2_3_7.findSum(a, 12));
        System.out.println(Exercise_2_3_7.findSum(b, 12));
        System.out.println(Exercise_2_3_7.findSum(a, 23));
        System.out.println(Exercise_2_3_7.findSum(a, 23));
        System.out.println(Exercise_2_3_7.findSum(c, 2));
        System.out.println(Exercise_2_3_7.findSum(c, 4));
        System.out.println(Exercise_2_3_7.findSum(d, 0));
    }
}
