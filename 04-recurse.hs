maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum z prázdného seznamu"  
maximum' [x] = x  
maximum' (x:xs)  
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs 

{-|
	JAVA 8 EQUIVALENT:

	public class Main {

		private static final int[] input = {1, 3, 0, 7, 5};

		public static void main(String[] args) {
			System.out.println(max(input));
		}

		private static int max(int[] input) {
			if (input == null || input.length == 0) {
				throw new IllegalArgumentException();
			}

			if (input.length == 1) {
				return input[0];
			}

			return (input[0] > input[1]) ? input[0] : max(tail(input));
		}

		private static int[] tail(int[] input) {
			int[] dst = new int[input.length - 1];
			System.arraycopy(input, 1, dst, 0, input.length - 1);
			return dst;
		}
	
		private static void print(int[] input){
			IntStream.of(input).forEach(System.out::println);
		}
	}
|-}