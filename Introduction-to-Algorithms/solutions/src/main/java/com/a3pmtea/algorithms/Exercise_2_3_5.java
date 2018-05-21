package com.a3pmtea.algorithms;

/**
 * Exercise 2.3-5
 * Referring back to the searching problem (see Exercise 2.1-3), observe that if the
 * sequence A is sorted, we can check the midpoint of the sequence against v and
 * eliminate half of the sequence from further consideration.
 * The binary search algorithm repeats this procedure, halving the size of the remaining portion of the
 * sequence each time. Write pseudocode, either iterative or recursive, for binary
 * search. Argue that the worst-case running time of binary search is o(lgN).
 */
public class Exercise_2_3_5 {

    /**
     * Find the index of a given number in an array, using recursion
     * @param a The sorted array
     * @param n The number to be searched
     * @return The index of {@code n} in {@code a}, or -1 if absent.
     */
    public static int searchRecursive(int[] a, int n) {
        return searchRecursive(a, n, 0, a.length);
    }

    /**
     * Search in {@code a} from {@code start} inclusive to {@code end} exclusive
     */
    private static int searchRecursive(int[] a, int n, int start, int end) {
        if (start >= end) return -1;
        
        int mid = (start + end) / 2;
        if (a[mid] == n) {
            return mid;
        } else if (a[mid] > n) {
            return searchRecursive(a, n, start, mid);
        } else {
            return searchRecursive(a, n, mid + 1, end);
        }
    }

    /**
     * Find the index of a given number in an array
     * @param a The sorted array
     * @param n The number to be searched
     * @return The index of {@code n} in {@code a}, or -1 if absent.
     */
    public static int search(int[] a, int n) {
        if (a.length == 0) return -1;
        
        int start = 0, end = a.length, mid;
        while (start < end) {
            mid = (start + end) / 2;
            if (a[mid] == n) {
                return mid;
            } else if (a[mid] > n) {
                end = mid;
            } else {
                start = mid + 1;
            }
        }
        return -1;
    }

    public static void main(String[] args) {
        int[] a = new int[] {1, 3, 5, 8, 9};
        int[] b = new int[] {};
        System.out.println(searchRecursive(a, 2));
        System.out.println(search(a, 2));
        System.out.println(searchRecursive(a, 8));
        System.out.println(search(a, 8));
        System.out.println(searchRecursive(b, 2));
        System.out.println(search(b, 2));
    }
}
