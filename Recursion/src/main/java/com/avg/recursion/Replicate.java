package com.avg.recursion;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 *
 * @author Michal Nedbálek <michal.nedbalek@avg.com> on 16/02/2016
 */
public class Replicate implements Runnable {

	/**
	 * int[][] test = {{1, 1}, {1, 2}};
	 * Arrays.asList(test).forEach(s -> Arrays.asList(s).forEach(System.out::println));
	 */
	
	private static final Integer[] input = {3, 1, 0, 8, 4, 7};
//	private static final String input = "a";

	@Override
	public void run() {
		final int TIMES = 3;
		System.out.println(replicate(input, TIMES));
	}

	private static List<String> replicate(Object input, int times) {
		if (times <= 0) {
			return new ArrayList<>();
		}

		List<String> output = replicate(input, times - 1);
		add(output, input);

		return output;
	}

	private static boolean add(List<String> list, Object elem) {
		if (elem.getClass().isArray()) {
			Object[] wrapper = (Object[]) elem;
			return list.add(Arrays.toString(wrapper));
		} else {
			return list.add(elem.toString());
		}
	}

}
