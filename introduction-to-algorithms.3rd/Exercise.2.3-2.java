package algorithms;

import java.util.Arrays;

/**
 * Exercise 2.3-2
 * Rewrite the MERGE procedure so that it does not use sentinels, instead stopping
 * once either array L or R has had all its elements copied back to A and then copying
 * the remainder of the other array back into A.
 */
public class ArrayMerge {
    /**
     * Merge two sorted arrays into one sorted array
     *
     * @param a The first sorted array
     * @param b The second sorted array
     * @return The merged array
     */
    public static int[] merge(int[] a, int[] b) {
        int[] merged = new int[a.length + b.length];
        int i = 0, j = 0, k = 0;
        while (i < a.length && j < b.length) {
            if (a[i] < b[j]) merged[k++] = a[i++];
            else merged[k++] = b[j++];
        }
        while (i < a.length) {
            merged[k++] = a[i++];
        }
        while (j < b.length) {
            merged[k++] = b[j++];
        }
        return merged;
    }

    public static void main(String[] args) {
        System.out.println(Arrays.toString(merge(new int[] {2}, new int[] {1})));
        System.out.println(Arrays.toString(merge(new int[] {1, 2, 7}, new int[] {3, 6, 8, 9})));
    }
}
