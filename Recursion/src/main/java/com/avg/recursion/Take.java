package com.avg.recursion;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Michal Nedbálek <michal.nedbalek@avg.com> on 16/02/2016
 */
public class Take implements Runnable {

	private static final int[] input = {2, 5, 7, 9, 0, 8, 4};

	@Override
	public void run() {
		final int COUNT = 5;
//		System.out.println(Arrays.toString(input));
		System.out.println(take(input, COUNT));
	}

	private static List<String> take(int[] input, int count) {
		if (count <= 0 || input == null || input.length == 0) {
			return new ArrayList<>();
		}

		List<String> output = take(input, count - 1);
		output.add("" + input[count - 1]);

		return output;
	}

}
