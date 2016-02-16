package com.avg.recursion;

/**
 *
 * @author Michal Nedbálek <michal.nedbalek@avg.com> on 16/02/2016
 */
public class Elem implements Runnable {

	private static final int[] input = {};
//	private static final int[] input = {1, 2, 3, 4, 5, 6, 7, 8, 9};

	@Override
	public void run() {
		final int WANTED = 0;
		System.out.println(elem(input, WANTED));
	}

	private static boolean elem(int[] input, int elem) {
		return elem(input, elem, 0);
	}

	private static boolean elem(int[] input, int elem, int pos) {
		if (input == null || input.length == 0 || pos == input.length) {
			return false;
		}

		return (input[pos] == elem) ? true : elem(input, elem, pos + 1);
	}
}
