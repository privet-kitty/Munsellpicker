package com.privetkitty;


public class Colortool {
	public static double red (int col) {
		double r = col >> 16 & 0xFF;
		return r;
	}

	public static double green (int col) {
		double g = col >> 8 & 0xFF;
		return g;
	}

	public static double blue (int col) {
		double b = col & 0xFF;
		return b;
	}

	public static double getRoughLuminance (int col) {
		// doesn't take the gamma value into consideration. Just for rough estimate.
		return red(col)*0.299 + green(col)*0.587 + blue(col)*0.114;
	}
}

