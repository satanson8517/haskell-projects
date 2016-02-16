package com.avg.recursion;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Michal Nedbálek <michal.nedbalek@avg.com> on 16/02/2016
 */
public class Reverse implements Runnable {

	private static final Integer[] input = {1, 2, 3, 4, 5};

	@Override
	public void run() {
		System.out.println(reverse(input));
	}

	private static List<String> reverse(Integer[] input) {
		final int POS = 0;
		return reverse(input, POS);
	}

	private static List<String> reverse(Integer[] input, int pos) {
		if (input == null || input.length == 0 || pos == input.length) {
			return new ArrayList<>();
		}

		List<String> output = reverse(input, pos + 1);
		output.add(input[pos].toString());

		return output;
	}

}
