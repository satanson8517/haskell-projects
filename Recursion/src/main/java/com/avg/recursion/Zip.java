package com.avg.recursion;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Michal Nedbálek <michal.nedbalek@avg.com> on 16/02/2016
 */
public class Zip implements Runnable {

	private static final int[] input1 = {1, 2, 3, 4, 5};
	private static final int[] input2 = {1, 2, 3};

	@Override
	public void run() {
		System.out.println(
				Reverse.reverse(
						zip(input1, input2).toArray()
				)
		);
	}

	private static List<Doublette> zip(int[] input1, int[] input2) {
		return zip(input1, input2, 0);
	}

	private static List<Doublette> zip(int[] input1, int[] input2, int pos) {
		if (input1 == null || input2 == null
				|| input1.length == 0 || input2.length == 0
				|| pos == input1.length || pos == input2.length) {
			return new ArrayList<>();
		}

		List<Doublette> output = zip(input1, input2, pos + 1);
		output.add(new Doublette(input1[pos], input2[pos]));

		return output;
	}

	private static class Doublette {

		private final int first;
		private final int second;

		public Doublette(int first, int second) {
			this.first = first;
			this.second = second;
		}

		@Override
		public String toString() {
			return "{" + first + ", " + second + '}';
		}
	}

}
