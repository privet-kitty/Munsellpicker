package com.privet_kitty;

public class Colortool {

	/**
	 * A small color library mainly for conversions between several color models.
	 * Every nominal range of Y (of XYZ and xyY) and RGB values are [0, 1].
	 * Standard illuminant is D65 unless otherwise noted.
	 */

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

	static double clamp (double x, double inf, double sup) {
		if (x <= inf) return inf;
		if (x >= sup) return sup;
		return x;
	}

	public static int mod360 (int x) {
		int y = x%360;
		if (y >= 0) return y;
		else return y + 360;
	}

	public static double mod360 (double x) {
		double y = x % 360;
		if (y >= 0) return y;
		else return y + 360.0;
	}


	public static int minusMod360 (int x, int y) {
		return mod360(x-y);
	}

	public static double minusMod360 (double x, double y) {
		return mod360(x-y);
	}

	// Hue of Munsell Color (40 colors beginning from 5R) to hue of HSV Color. The value is 7 and the chroma is 8.
	static int[] hueTable = {
			3, 10, 15, 21, 26, 32, 37, 42, 48, 53, 58, 65, 75, 92, 114, 141, 154, 161, 165, 170,
			175, 179, 183, 187, 191, 195, 200, 207, 214, 228, 244, 261, 276, 300, 316, 327, 339, 345, 353, 358
	};
	// inversion of hueTable, beginning with H = 0
	static double[] mhueTable = {
			354.59999999999997, 356.40000000000003, 358.2, 0.0, 1.2857142857142856, 2.571428571428571, 3.8571428571428568, 5.142857142857142, 6.428571428571429, 7.7142857142857135,
			9.0, 10.799999999999999, 12.6, 14.4, 16.2, 18.0, 19.5, 21.0, 22.5, 24.0,
			25.5, 27.0, 28.8, 30.599999999999998, 32.4, 34.199999999999996, 36.0, 37.5, 39.0, 40.5,
			42.0, 43.5, 45.0, 46.800000000000004, 48.6, 50.4, 52.199999999999996, 54.0, 55.800000000000004, 57.6,
			59.4, 61.199999999999996, 63.0, 64.5, 66.0, 67.5, 69.0, 70.5, 72.0, 73.8,
			75.60000000000001, 77.39999999999999, 79.2, 81.0, 82.8, 84.60000000000001, 86.39999999999999, 88.2, 90.0, 91.28571428571428,
			92.57142857142858, 93.85714285714286, 95.14285714285714, 96.42857142857142, 97.71428571428572, 99.0, 99.89999999999999, 100.8, 101.7, 102.60000000000001,
			103.5, 104.39999999999999, 105.3, 106.2, 107.10000000000001, 108.0, 108.52941176470588, 109.05882352941177, 109.58823529411764, 110.11764705882352,
			110.64705882352942, 111.1764705882353, 111.70588235294117, 112.23529411764706, 112.76470588235294, 113.29411764705883, 113.8235294117647, 114.35294117647058, 114.88235294117648, 115.41176470588236,
			115.94117647058823, 116.47058823529412, 117.0, 117.4090909090909, 117.81818181818183, 118.22727272727273, 118.63636363636364, 119.04545454545453, 119.45454545454547, 119.86363636363636,
			120.27272727272727, 120.68181818181817, 121.0909090909091, 121.5, 121.9090909090909, 122.31818181818183, 122.72727272727273, 123.13636363636364, 123.54545454545453, 123.95454545454547,
			124.36363636363636, 124.77272727272727, 125.18181818181817, 125.5909090909091, 126.0, 126.33333333333333, 126.66666666666667, 127.0, 127.33333333333334, 127.66666666666667,
			128.0, 128.33333333333334, 128.66666666666666, 129.0, 129.33333333333334, 129.66666666666666, 130.0, 130.33333333333331, 130.66666666666669, 131.0,
			131.33333333333331, 131.66666666666666, 132.0, 132.33333333333334, 132.66666666666666, 133.0, 133.33333333333334, 133.66666666666666, 134.0, 134.33333333333334,
			134.66666666666669, 135.0, 135.69230769230768, 136.3846153846154, 137.07692307692307, 137.76923076923077, 138.46153846153845, 139.15384615384616, 139.84615384615384, 140.53846153846155,
			141.23076923076923, 141.92307692307693, 142.6153846153846, 143.30769230769232, 144.0, 145.28571428571428, 146.57142857142856, 147.85714285714283, 149.14285714285717, 150.42857142857144,
			151.71428571428572, 153.0, 155.25, 157.5, 159.75, 162.0, 163.79999999999998, 165.6, 167.4, 169.20000000000002,
			171.0, 172.79999999999998, 174.6, 176.4, 178.20000000000002, 180.0, 182.25, 184.5, 186.75, 189.0,
			191.25, 193.5, 195.75, 198.0, 200.25, 202.5, 204.75, 207.0, 209.25, 211.5,
			213.75, 216.0, 218.25, 220.5, 222.75, 225.0, 226.79999999999998, 228.6, 230.4, 232.20000000000002,
			234.0, 235.28571428571428, 236.57142857142856, 237.85714285714283, 239.14285714285717, 240.42857142857144, 241.71428571428572, 243.0, 244.28571428571428, 245.57142857142856,
			246.85714285714283, 248.14285714285717, 249.42857142857144, 250.71428571428572, 252.0, 252.64285714285717, 253.28571428571428, 253.92857142857144, 254.57142857142856, 255.21428571428572,
			255.85714285714283, 256.5, 257.14285714285717, 257.7857142857143, 258.42857142857144, 259.07142857142856, 259.7142857142857, 260.35714285714283, 261.0, 261.5625,
			262.125, 262.6875, 263.25, 263.8125, 264.375, 264.9375, 265.5, 266.0625, 266.625, 267.1875,
			267.75, 268.3125, 268.875, 269.4375, 270.0, 270.52941176470586, 271.05882352941177, 271.5882352941176, 272.11764705882354, 272.6470588235294,
			273.1764705882353, 273.70588235294116, 274.2352941176471, 274.7647058823529, 275.29411764705884, 275.8235294117647, 276.3529411764706, 276.88235294117646, 277.4117647058824, 277.94117647058823,
			278.47058823529414, 279.0, 279.6, 280.2, 280.8, 281.4, 282.0, 282.59999999999997, 283.2, 283.8,
			284.40000000000003, 285.0, 285.6, 286.2, 286.8, 287.4, 288.0, 288.375, 288.75, 289.125,
			289.5, 289.875, 290.25, 290.625, 291.0, 291.375, 291.75, 292.125, 292.5, 292.875,
			293.25, 293.625, 294.0, 294.375, 294.75, 295.125, 295.5, 295.875, 296.25, 296.625,
			297.0, 297.5625, 298.125, 298.6875, 299.25, 299.8125, 300.375, 300.9375, 301.5, 302.0625,
			302.625, 303.1875, 303.75, 304.3125, 304.875, 305.4375, 306.0, 306.81818181818187, 307.6363636363636, 308.45454545454544,
			309.2727272727273, 310.09090909090907, 310.90909090909093, 311.7272727272727, 312.54545454545456, 313.3636363636364, 314.18181818181813, 315.0, 315.75, 316.5,
			317.25, 318.0, 318.75, 319.5, 320.25, 321.0, 321.75, 322.5, 323.25, 324.0,
			325.5, 327.0, 328.5, 330.0, 331.5, 333.0, 334.125, 335.25, 336.375, 337.5,
			338.625, 339.75, 340.875, 342.0, 343.8, 345.59999999999997, 347.40000000000003, 349.2, 351.0, 352.8
	};


	public static double[] constructMhueTable(int[] hTable) {
		int mhueStep = 360/hTable.length; //mhueStepが整数になるような長さでなければならない
		double[] tmpMhueTable = new double[360];
		for (int h=0; h<hTable.length; h++) {
			int i = (h+1) % hTable.length;
			int beg = hTable[h];
			int end = hTable[i];
			int dist = minusMod360(end, beg);
			for(int k = 0; k<dist; k++) {
				tmpMhueTable[mod360(beg+k)] = ((double) h + (double) k / (double) dist) * mhueStep;
			}
		}
		return tmpMhueTable;
	}

	public static void printMhueTable(int[] hTable) {
		double[] tmpMhueTable = constructMhueTable(hTable);
		System.out.print("{");
		for(int i =0; i< tmpMhueTable.length; i++) {
			if(i!=0) System.out.print(", ");
			if(i%10 == 0) System.out.print("\n");
			System.out.print(tmpMhueTable[i]);
		}
		System.out.println("}");
	}



	public static double mhueToHue(double mhue) {
		double x = mhue/360 * hueTable.length;
		int x_floor = (int) Math.floor(x);
		double r = x - x_floor;
		double beg = hueTable[x_floor];
		double end = hueTable[(x_floor+1)%hueTable.length];
		double dist = minusMod360(end, beg);
		double hue = beg + r*dist;
		return mod360(hue);
	}

	public static double hueToMhue(double hue) {
		hue = mod360(hue);
		int hue_floor = (int) Math.floor(hue);
		double r = hue - hue_floor;
		double beg = mhueTable[hue_floor];
		double end = mhueTable[(hue_floor+1)%360];
		double dist = minusMod360(end, beg);
		double mhue = beg + r*dist;
		return mod360(mhue);
	}


	public static double getRoughLuminance (int col) {
		return red(col)*0.299 + green(col)*0.587 + blue(col)*0.114;
	}

	public static double[] D65_WHITE = {0.95047, 1.0, 1.08883}; //noon daylight
	public static double[] D50_WHITE = {0.96422, 1.0000, 0.82521}; //horizon light
	public static double[] A_WHITE = {1.09850, 1.00000, 0.35585}; //typical, domestic, tungsten-filament lighting
	public static double[] C_WHITE = {0.98074, 1.0000, 1.18232}; //north sky daylight

	static double[][] BRADFORD_MATRIX =
		{{0.8951, 0.2664, -0.1614}, {-0.7502, 1.7135, 0.0367}, {0.0389, -0.0685, 1.0296}};
	static double[][] REV_BRADFORD_MATRIX =
		{{0.98699290546671, -0.147054256421, 0.15996265166373},
				{0.4323052697234, 0.51836027153678, 0.049291228212856},
				{-0.0085286645751773, 0.040042821654085, 0.96848669578755}};


	public static void printMatrix(double[][] mat) {
		System.out.print("{");
		for(int i =0; i< mat.length; i++) {
			if(i!=0) System.out.print(", ");
			System.out.print("{");
			for (int j = 0; j<mat[i].length; j++) {
				if(j!=0) System.out.print(", ");
				System.out.print(mat[i][j]);
			}
			System.out.print("}");
		}
		System.out.println("}");
	}


	//２つの光源間のホワイトポイントからXYZの変換行列を与える。
	//この行列はchromaticAdaptation()で使う。
	public static double[][] getBradfordTransformation(double[] sourceWHITE, double[] destWHITE) {
		double[] sourceLMS = new double[3];
		sourceLMS[0] = BRADFORD_MATRIX[0][0]*sourceWHITE[0] + BRADFORD_MATRIX[0][1]*sourceWHITE[1] + BRADFORD_MATRIX[0][2]*sourceWHITE[2];
		sourceLMS[1] = BRADFORD_MATRIX[1][0]*sourceWHITE[0] + BRADFORD_MATRIX[1][1]*sourceWHITE[1] + BRADFORD_MATRIX[1][2]*sourceWHITE[2];
		sourceLMS[2] = BRADFORD_MATRIX[2][0]*sourceWHITE[0] + BRADFORD_MATRIX[2][1]*sourceWHITE[1] + BRADFORD_MATRIX[2][2]*sourceWHITE[2];

		double[] destLMS = new double[3];
		destLMS[0] = BRADFORD_MATRIX[0][0]*destWHITE[0] + BRADFORD_MATRIX[0][1]*destWHITE[1] + BRADFORD_MATRIX[0][2]*destWHITE[2];
		destLMS[1] = BRADFORD_MATRIX[1][0]*destWHITE[0] + BRADFORD_MATRIX[1][1]*destWHITE[1] + BRADFORD_MATRIX[1][2]*destWHITE[2];
		destLMS[2] = BRADFORD_MATRIX[2][0]*destWHITE[0] + BRADFORD_MATRIX[2][1]*destWHITE[1] + BRADFORD_MATRIX[2][2]*destWHITE[2];

		double[][] M1 = new double[3][3];

		double Lratio = destLMS[0]/sourceLMS[0];
		double Mratio = destLMS[1]/sourceLMS[1];
		double Sratio = destLMS[2]/sourceLMS[2];

		M1[0][0] = Lratio*BRADFORD_MATRIX[0][0];
		M1[0][1] = Lratio*BRADFORD_MATRIX[0][1];
		M1[0][2] = Lratio*BRADFORD_MATRIX[0][2];
		M1[1][0] = Mratio*BRADFORD_MATRIX[1][0];
		M1[1][1] = Mratio*BRADFORD_MATRIX[1][1];
		M1[1][2] = Mratio*BRADFORD_MATRIX[1][2];
		M1[2][0] = Sratio*BRADFORD_MATRIX[2][0];
		M1[2][1] = Sratio*BRADFORD_MATRIX[2][1];
		M1[2][2] = Sratio*BRADFORD_MATRIX[2][2];

		double[][] M2 = new double[3][3];
		for (int i = 0; i < 3; i++) {
			for (int j = 0; j < 3; j++) {
				double sum = 0;
				for (int k = 0; k < 3; k++) {
					sum += REV_BRADFORD_MATRIX[i][k] * M1[k][j];
				}
				M2[i][j] = sum;
			}
		}

		return M2;
	}

	public static double[] chromaticAdaptation(double[] XYZ, double[][] MATRIX) {
		double[] XYZ2 = new double[3];
		XYZ2[0] = MATRIX[0][0] * XYZ[0] + MATRIX[0][1] * XYZ[1] + MATRIX[0][2] * XYZ[2];
		XYZ2[1] = MATRIX[1][0] * XYZ[0] + MATRIX[1][1] * XYZ[1] + MATRIX[1][2] * XYZ[2];
		XYZ2[2] = MATRIX[2][0] * XYZ[0] + MATRIX[2][1] * XYZ[1] + MATRIX[2][2] * XYZ[2];
		return XYZ2;
	}

	static double SRGB_BOUNDARY = 0.04045;


	//[0, 1]のR, G, B値をリニアにして返す。sRGBに基づく。
	public static double linearize(double x) {
		if (x <= SRGB_BOUNDARY) return x/12.92;
		else  return  Math.pow(((x + 0.055)/1.055), 2.4);
	}

	public static double linearize255(double x) {
		return 255*linearize(x/255);
	}

	//linearizeの逆変換。
	public static double unlinearize (double x) {
		if (x <= 0.0031308) return x * 12.92;
		else return 1.055 * Math.pow(x, 0.41666666666) - 0.055;
	}

	public static double unlinearize255(double x) {
		return 255*unlinearize(x/255);
	}

	//HexからXYZ、D65に従ってY[0, 1]を求める。
	public static double getY(int col) {
		double r, g, b;
		//sRGB D65におけるRGB->XYZの変換。Yは光度を表す。
		r = linearize(red(col)/255);
		g = linearize(green(col)/255);
		b = linearize(blue(col)/255);
		return 0.212671*r + 0.715160*g + 0.072169*b;
	}

	//HexからL*の値をもとめる。
	public static double getLstar(int col) {
		//YからL*へ変換する。どの照明においてもwhite pointはY=1なので、D65でもD50でも同じ。
		double Y = getY(col);
		return 116.0 * function_f(Y/D65_WHITE[1]) - 16.0;
	}

	static double DELTA = 0.20689655172;
	static double DELTA2 = 0.04280618311;
	static double DELTA3 = 0.00885645167;

	static double function_f (double t) {
		if (t > DELTA3)
			return Math.pow(t, 0.333333333);
		else
			return t/(3*DELTA2) + 0.13793103448;
	}


	public static double[] hexToXYZ (int col) {
		double r, g, b;
		//sRGB D65におけるRGB->XYZの変換。Yは光度を表す。
		r = linearize(red(col)/255);
		g = linearize(green(col)/255);
		b = linearize(blue(col)/255);
		double[] XYZ = new double[3];
		XYZ[0] = 0.4124564*r + 0.3575761*g + 0.1804375*b;
		XYZ[1] = 0.2126729*r + 0.7151522*g + 0.0721750*b;
		XYZ[2] = 0.0193339*r + 0.1191920*g + 0.9503041*b;
		return XYZ;
	}

	public static int hexToGrayscale (int col) {
		int val = (int) Math.round(unlinearize(getY(col))*255);
		//double[] XYZ = hexToXYZ(col);
		//int val = (int) Math.round(unlinearize(XYZ[1])*255);
		return (val << 16) + (val << 8) + val;
	}

	public static double[] XYZToLab (double X, double Y, double Z, double[] white) {
		double fX = function_f(X/white[0]);
		double fY = function_f(Y/white[1]);
		double fZ = function_f(Z/white[2]);
		double[]  lab = new double[3];
		lab[0] = 116.0 * fY - 16.0;
		lab[1] = 500.0 * (fX - fY);
		lab[2] = 200.0 * (fY - fZ);

		return lab;
	}

	public static double[] XYZToLab (double X, double Y, double Z) {
		return XYZToLab(X, Y, Z, D65_WHITE);
	}

	//HexからL*a*b*を返す。
	public static double[] hexToLab(int col) {
		double[] XYZ = hexToXYZ(col);
		return XYZToLab(XYZ[0], XYZ[1], XYZ[2]);
	}

	public static double[] labToXYZ(double Lstar, double astar, double bstar) {
		double fy = (Lstar+16)/116;
		double fx = fy + astar/500;
		double fz = fy - bstar/200;
		double[] XYZ = new double[3];
		if(fy > DELTA) XYZ[1] = D65_WHITE[1]*fy*fy*fy;
		else XYZ[1] = (fy-0.13793103448)*0.12841854934*D65_WHITE[1];
		if(fx > DELTA) XYZ[0] = D65_WHITE[0]*fx*fx*fx;
		else XYZ[0] = (fx-0.13793103448)*0.12841854934*D65_WHITE[0];
		if(fz > DELTA) XYZ[2] = D65_WHITE[2]*fz*fz*fz;
		else XYZ[2] = (fz-0.13793103448)*0.12841854934*D65_WHITE[2];
		return XYZ;
	}

	//clamp がtrueならr,g, bがそれぞれ[0,255]に収まるようにする。
	public static int XYZToHex(double X, double Y, double Z, boolean clamp) {
		double r = 3.2404542*X -1.5371385*Y -0.4985314*Z;
		double g = -0.9692660*X +1.8760108*Y  +0.0415560*Z;
		double b = 0.0556434*X -0.2040259*Y  +1.0572252*Z;

		if(clamp) {
			r = clamp(r, 0, 1);
			g = clamp(g, 0, 1);
			b = clamp(b, 0, 1);
		}

		int lr = (int) Math.round(unlinearize(r)*255);
		int lg = (int) Math.round(unlinearize(g)*255);
		int lb = (int) Math.round(unlinearize(b)*255);
		return (lr << 16) + (lg << 8) + lb;
	}

	//rgbの値が範囲外になると-1を返すバージョン。
	public static int XYZToHexWithSelection(double X, double Y, double Z) {
		double r = 3.2404542*X -1.5371385*Y -0.4985314*Z;
		double g = -0.9692660*X +1.8760108*Y  +0.0415560*Z;
		double b = 0.0556434*X -0.2040259*Y  +1.0572252*Z;

		if ((r < 0) || (r > 1) || (g < 0) || (1 < g) || (b < 0) || (1 < b)) return -1;

		int lr = (int) Math.round(unlinearize(r)*255);
		int lg = (int) Math.round(unlinearize(g)*255);
		int lb = (int) Math.round(unlinearize(b)*255);
		return (lr << 16) + (lg << 8) + lb;
	}

	public static int XYZToHex(double X, double Y, double Z) {
		return XYZToHex(X, Y, Z, true);
	}

	public static int labToHex(double Lstar, double astar, double bstar) {
		double[] XYZ = labToXYZ(Lstar, astar, bstar);
		return XYZToHex(XYZ[0], XYZ[1], XYZ[2], true);
	}


	public static int labToHexWithSelection(double Lstar, double astar, double bstar) {
		double[] XYZ = labToXYZ(Lstar, astar, bstar);
		return XYZToHexWithSelection(XYZ[0], XYZ[1], XYZ[2]);
	}


	//Labの値がsRGB(D65)の範囲に収まっているか調べる。
	//これを調べてからhexを計算するのは効率が悪い。もう少しうまいやりかたがありそう。
	public static boolean labIsInSrgb(double Lstar, double astar, double bstar) {
		double[] XYZ = labToXYZ(Lstar, astar, bstar);
		return XYZIsInSrgb(XYZ[0], XYZ[1], XYZ[2]);
	}

	public static boolean XYZIsInSrgb(double X, double Y, double Z) {
		double r = 3.2404542*X -1.5371385*Y -0.4985314*Z;
		double g = -0.9692660*X +1.8760108*Y  +0.0415560*Z;
		double b = 0.0556434*X -0.2040259*Y  +1.0572252*Z;
		if ((0 <= r) && (r <= 1) &&
				(0 <= g) && (g <= 1) &&
				(0 <= b) && (b <= 1))
			return true;
		else return false;
	}

	public static boolean xyYIsInSrgb(double x, double y, double Y) {
		double[] XYZ = xyYToXYZ(x, y, Y);
		return XYZIsInSrgb(XYZ[0], XYZ[1], XYZ[2]);
	}

	public static double getLabhue(double astar, double bstar) {
		double angle = Math.atan2(bstar,  astar);
		return mod360(angle/Math.PI * 180);
	}

	public static double getLabchroma(double astar, double bstar) {
		return Math.sqrt(astar*astar + bstar* bstar);
	}


	public static double[] labToHclab(double[] lab) {
		double[] hclab = new double[3];
		hclab[0] = getLabhue(lab[1], lab[2]);
		hclab[1] = getLabchroma(lab[1], lab[2]);
		hclab[2] = lab[0];
		return hclab;
	}

	public static double[] hexToHclab(int col) {
		return labToHclab(hexToLab(col));
	}

	public static double[] hexToLchab(int col) {
		return hexToHclab(col);
	}

	public static int hclabToHex(double hue, double chroma, double Lstar) {
		double angle = hue/360 * Math.PI * 2;
		double astar = chroma * Math.cos(angle); // x = radius * cos(angle);
		double bstar = chroma * Math.sin(angle); // y = radius * sin(angle);
		return labToHex(Lstar, astar, bstar);
	}

	public static int lchabToHex(double hue, double chroma, double Lstar ) {
		return hclabToHex(hue, chroma, Lstar);
	}

	public static int hclabToHexWithSelection(double hue, double chroma, double Lstar) {
		double angle = hue/360 * Math.PI * 2;
		double astar = chroma * Math.cos(angle); // x = radius * cos(angle);
		double bstar = chroma * Math.sin(angle); // y = radius * sin(angle);
		return labToHexWithSelection(Lstar, astar, bstar);
	}

	public static int lchabToHexWithSelection(double hue, double chroma, double Lstar) {
		return lchabToHexWithSelection(hue, chroma, Lstar);
	}

	public static double[] hexToRgb (int col) {
		double[] rgb = new double[3];
		rgb[0] = red(col); rgb[1] = green(col); rgb[2] = blue(col);
		return rgb;
	}

	public static double[] xyYToXYZ (double x, double y, double Y) {
		double[] XYZ = new double[3];
		if (y == 0) {
			XYZ[0] = 0; XYZ[1] = 0; XYZ[2] = 0;
		} else {
			XYZ[0] = x*Y/y; XYZ[1] = Y; XYZ[2] = (1-x-y)*Y/y;
		}
		return XYZ;
	}

	public static double[] XYZToXyY (double X, double Y, double Z) {
		double sum = X+Y+Z;
		double[] xyY = new double[3];
		if (sum == 0) {
			xyY[0] = 0.31272661468; //0.95047/(0.95047+ 1.0+1.08883) reference white
			xyY[1] = 0.32902313032; //1/(0.95047+ 1.0+1.08883)
			xyY[2] = Y;
		} else {
			xyY[0] = X/sum;
			xyY[1] = Y/sum;
			xyY[2] = Y;
		}
		return xyY;
	}

	public static int xyYToHex(double x, double y, double Y, boolean clamp) {
		double[] XYZ = xyYToXYZ(x, y, Y);
		return XYZToHex(XYZ[0], XYZ[1], XYZ[2], clamp);
	}

	public static int xyYToHex(double x, double y, double Y) {
		double[] XYZ = xyYToXYZ(x, y, Y);
		return XYZToHex(XYZ[0], XYZ[1], XYZ[2], true);
	}

	public static int xyYToHexWithSelection(double x, double y, double Y) {
		double[] XYZ = xyYToXYZ(x, y, Y);
		return XYZToHexWithSelection(XYZ[0], XYZ[1], XYZ[2]);
	}

	public static double[] hexToXyY (int col) {
		double[] XYZ = hexToXYZ(col);
		return XYZToXyY(XYZ[0], XYZ[1], XYZ[2]);
	}

	public static double[] xyYToLab (double x, double y, double Y) {
		double[] XYZ = xyYToXYZ(x, y, Y);
		return XYZToLab(XYZ[0], XYZ[1], XYZ[2]);
	}

	public static double colorDifference(double[] lab1, double[] lab2) {
		double dl = lab1[0] - lab2[0];
		double da = lab1[1] - lab2[1];
		double db = lab1[2] - lab2[2];
		return Math.sqrt(dl*dl + da*da + db*db);
	}

	public static double getCIEDE2000(double[] lab1, double[] lab2) {
		double[] hclab1 = labToHclab(lab1);
		double[] hclab2 = labToHclab(lab2);
		double astar1 = lab1[1], astar2=lab2[1];
		double bstar1 = lab1[2], bstar2=lab2[2];
		double C1=hclab1[1], C2 = hclab2[1];
		double Lstar1 = hclab1[2], Lstar2 = hclab2[2];
		double deltaLprime = Lstar2-Lstar1;
		double Lavg = (Lstar1+Lstar2)*0.5, Cavg = (C1+C2)*0.5;
		double Cavg7 = Math.pow(Cavg, 7);
		double const1 = 0.5*(1-Math.sqrt(Cavg7/(Cavg7 + 6103515625.0)));
		double aprime1 = astar1 + astar1*const1;
		double aprime2 = astar2 + astar2*const1;
		double Cprime1 = Math.sqrt(aprime1*aprime1 + bstar1*bstar1);
		double Cprime2 = Math.sqrt(aprime2*aprime2 + bstar2*bstar2);
		double Cavgprime = (Cprime1 + Cprime2)*0.5;
		double deltaCprime = Cprime2 - Cprime1;
		double hprime1, hprime2;
		if (bstar1==0 && aprime1==0)
			hprime1=0;
		else
			hprime1 = mod360(Math.atan2(bstar1, aprime1)*180/Math.PI);
		if (bstar2==0 && aprime2==0)
			hprime2=0;
		else
			hprime2 = mod360(Math.atan2(bstar2, aprime2)*180/Math.PI);

		double deltahprime;
		if(Cprime1==0 || Cprime2==0)
			deltahprime=0;
		else if (Math.abs(hprime1-hprime2) <= 180)
			deltahprime = hprime2 - hprime1;
		else if (hprime2 <= hprime1)
			deltahprime = hprime2 - hprime1 + 360;
		else
			deltahprime = hprime2-hprime1-360;

		double deltaHprime = 2*Math.sqrt(Cprime1*Cprime2)*Math.sin(deltahprime*0.5*Math.PI/180);
		double Havgprime;
		if(Cprime1==0 || Cprime2==0)
			Havgprime = hprime1+hprime2;
		else if (Math.abs(hprime1-hprime2) <= 180)
			Havgprime = (hprime1 + hprime2)*0.5;
		else if (hprime1 + hprime2 < 360)
			Havgprime = (hprime1 + hprime2 + 360)*0.5;
		else
			Havgprime = (hprime1 + hprime2 - 360)*0.5;

		double T = 1 - 0.17*Math.cos((Havgprime-30)*Math.PI/180)
				+ 0.24*Math.cos(2*Havgprime*Math.PI/180)
				+ 0.32*Math.cos((3*Havgprime+6)*Math.PI/180)
				- 0.20*Math.cos((4*Havgprime-63)*Math.PI/180);
		double SL = 1+(0.015*(Lavg-50)*(Lavg-50))/Math.sqrt(20+(Lavg-50)*(Lavg-50));
		double SC = 1+0.045*Cavgprime;
		double SH = 1+0.015*Cavgprime*T;
		double Cavgprime7 = Math.pow(Cavgprime, 7);
		double RT = -2*Math.sqrt(Cavgprime7/(Cavgprime7+6103515625.0))
				*Math.sin(60*Math.exp(-((Havgprime-275)/25)*((Havgprime-275)/25))*Math.PI/180);

		//System.out.println(deltaCprime);
		return Math.sqrt((deltaLprime*deltaLprime)/(SL*SL)
				+ (deltaCprime*deltaCprime)/(SC*SC)
				+ (deltaHprime*deltaHprime)/(SH*SH)
				+ RT*(deltaCprime/SC)*(deltaHprime/SH));
	}

	public static double XYZCIEDE2000(double[] XYZ1, double[] XYZ2) {
		return getCIEDE2000(XYZToLab(XYZ1[0], XYZ1[1], XYZ1[2]), XYZToLab(XYZ2[0], XYZ2[1], XYZ2[2]));
	}

	public static double xyYColorDifference(double[] xyY1, double[] xyY2) {
		return colorDifference(xyYToLab(xyY1[0], xyY1[1], xyY1[2]), xyYToLab(xyY2[0], xyY2[1], xyY2[2]));
	}

	public static double XYZColorDifference(double[] XYZ1, double[] XYZ2, double[] white) {
		return colorDifference(XYZToLab(XYZ1[0], XYZ1[1], XYZ1[2], white), XYZToLab(XYZ2[0], XYZ2[1], XYZ2[2], white));
	}

	public static double XYZColorDifference(double[] XYZ1, double[] XYZ2) {
		return XYZColorDifference(XYZ1, XYZ2, D65_WHITE);
	}
}

