package com.avg.recursion;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Michal Nedbálek <michal.nedbalek@avg.com> on 16/02/2016
 */
public class Zip implements Runnable {

//	private static final int[] input1 = {1, 2, 3, 4, 5};
//	private static final int[] input2 = {1, 2, 3};
	
	private static final String[] input1 = {"a", "b", "c"};
	private static final String[] input2 = {"a", "b", "c", "d", "e"};

	@Override
	public void run() {
		System.out.println(
				Reverse.reverse(
						zip(input1, input2).toArray()
				)
		);
	}

	private static List<Doublette> zip(Object[] input1, Object[] input2) {
		return zip(input1, input2, 0);
	}

	private static List<Doublette> zip(Object[] input1, Object[] input2, int pos) {
		if (input1 == null || input2 == null
				|| input1.length == 0 || input2.length == 0
				|| pos == input1.length || pos == input2.length) {
			return new ArrayList<>();
		}

		List<Doublette> output = zip(input1, input2, pos + 1);
		output.add(new Doublette(input1[pos], input2[pos]));

		return output;
	}

	private static class Doublette<F, S> {

		private final F first;
		private final S second;

		public Doublette(F first, S second) {
			this.first = first;
			this.second = second;
		}

		@Override
		public String toString() {
			return "{" + first + ", " + second + '}';
		}
	}

}
