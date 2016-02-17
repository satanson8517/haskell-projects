package com.avg.recursion;

import java.util.Arrays;

/**
 *
 * @author Michal Nedbálek <michal.nedbalek@avg.com> on 16/02/2016
 */
public class Quicksort implements Runnable {

	private static final int[] input = {10, 2, 5, 3, 1, 6, 7, 4, 2, 3, 4, 8, 9};
//	private static final int[] input = {8, 6, 4, 1, 0, 2, 5, 3, 7, 9};

	@Override
	public void run() {
		System.out.println(Arrays.toString(quicksort(input)));
	}

	private static int[] quicksort(int[] input) {
		int[] copy = new int[input.length];
		System.arraycopy(input, 0, copy, 0, copy.length);
		quicksort(copy, 0, copy.length);
		return copy;
	}

	public static void quicksort(int[] array, int left, int right) {
		if (left < right) { // pozice elementů v poli - odkud až kam se má řadit
			int pivotPos = left;
//			System.out.println(pivotPos);
			for (int i = left + 1; i < right; i++) {
				if (array[i] < array[left]) { // tohle a jenom tohle zařizuje ASC nebo DESC
					swap(array, i, ++pivotPos);
//					System.out.println("Inner: " + Arrays.toString(array));
				}
			}
			swap(array, left, pivotPos);
//			System.out.println("Outer: " + Arrays.toString(array));
			quicksort(array, left, pivotPos);
			quicksort(array, pivotPos + 1, right);
		}
	}

	private static void swap(int[] array, int left, int right) {
		int aux = array[right];
		array[right] = array[left];
		array[left] = aux;
	}

}
