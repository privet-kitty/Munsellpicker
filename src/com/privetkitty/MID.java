package com.privetkitty;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.IntBuffer;
import java.nio.channels.FileChannel;

/**
 * MID (Munsell Inversion Data) holds a 64MB binary file and provides a converter from RGB to Munsell.
 * @author hugo
 *
 */
public class MID {
	static final int possibleColors = 16777216;
	final int[] midArray = new int[possibleColors];
	static String[] mhueTable = {"R", "YR", "Y", "GY", "G", "BG", "B", "PB", "P", "RP"};
	boolean isPreciseValue;
	boolean isPreciseHue;


	/**
	 * The given file must be 64MB binary data produced by <a href="https://github.com/privet-kitty/munsell-inversion-data">munsell-inversion-data</a>.
	 * @param path : path of a binary file
	 * @throws IOException
	 */
	MID (String path) throws IOException {
		loadData(path);
		isPreciseValue = false;
		isPreciseHue = false;
	}

	public void loadData(String path) throws IOException{
		FileInputStream stream = null;
			stream = new FileInputStream(path);
			FileChannel inChannel = stream.getChannel();

			ByteBuffer buffer = inChannel.map(FileChannel.MapMode.READ_ONLY, 0, inChannel.size());
			buffer.order(ByteOrder.BIG_ENDIAN);
			IntBuffer intBuffer = buffer.asIntBuffer();
			intBuffer.get(midArray);

			if (stream != null)
				stream.close();
	}

	public void enablePreiciseValue() {
		isPreciseValue = true;
	}
	public void disablePreiciseValue() {
		isPreciseValue = false;
	}
	public void enablePreiciseHue() {
		isPreciseHue = true;
	}
	public void disablePreiciseHue() {
		isPreciseHue = false;
	}

	int[] decodeHVC1000 (int hex) {
		int[] hvc = new int[3];
		hvc[0] = (hex >> 20) & 1023; // 0b1111111111
		hvc[1] = (hex >> 10) & 1023;
		hvc[2] = hex & 1023;
		return hvc;
	}

	public int[] getHVC1000 (int hex) {
		return decodeHVC1000(midArray[hex & 0xffffff]);
	}

	String[] hvc1000ToSpec (int[] hvc) {
		int mhueNumber = (int) Math.floor(hvc[0]/100.0)%10;
		String mhueName = mhueTable[mhueNumber];
		double mhueSuffix;
		if (isPreciseHue)
			mhueSuffix = ((hvc[0] - mhueNumber*100)/10.0);
		else
			mhueSuffix = Math.round((hvc[0] - mhueNumber*100)/5.0)/2.0; // reduce 100 steps to 20 steps

		if (mhueSuffix == 0.0) {
			--mhueNumber;
			if (mhueNumber < 0) mhueNumber = 9;
			mhueName = mhueTable[mhueNumber];
			mhueSuffix = 10.0;
		}
		double mvalue = hvc[1]/100.0;
		double mchroma = hvc[2]/10.0;
		String mhueString = String.format("%.1f", mhueSuffix) + mhueName;
		if(mchroma == 0.0) {
			mhueString = "N";
		}

		String[] spec = new String[3];
		spec[0] = mhueString;
		if (isPreciseValue)
			spec[1] = String.format("%.2f", mvalue);
		else
			spec[1] = String.format("%.1f", mvalue);
		spec[2] = String.format("%.1f", mchroma);
		return spec;
	}

	public String[] getMunsellSpec(int hex) {
		return hvc1000ToSpec(getHVC1000(hex));
	}

	public String getMunsellString(int hex) {
		String[] spec = getMunsellSpec(hex);
		if (spec[0] == "N")
			return String.format("%s %s", spec[0], spec[1]);
		else
			return String.format("%s %s/%s", spec[0], spec[1], spec[2]);
	}

}
