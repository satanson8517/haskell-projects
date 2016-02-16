package com.avg.recursion;

/**
 *
 * @author Michal Nedbálek <michal.nedbalek@avg.com> on 16/02/2016
 */
public class MaxOfArray implements Runnable {

	private static final int[] input = {1, 3, 0, 7, 5};

	@Override
	public void run() {
		System.out.println(max(input));
	}

	private static int max(int[] input) {
		if (input == null || input.length == 0) {
			throw new IllegalArgumentException();
		}

		if (input.length == 1) {
			return input[0];
		}

		int maxTail = max(tail(input));

		return (input[0] > maxTail) ? input[0] : maxTail;
	}

	private static int[] tail(int[] input) {
		int[] dst = new int[input.length - 1];
		System.arraycopy(input, 1, dst, 0, input.length - 1);
		return dst;
	}

//	private static void print(int[] input) {
//		IntStream.of(input).forEach(System.out::println);
//	}

}
