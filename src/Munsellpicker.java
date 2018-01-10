import java.awt.AWTException;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.MouseInfo;
import java.awt.Point;
import java.awt.PointerInfo;
import java.awt.Rectangle;
import java.awt.Robot;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.Timer;
import javax.swing.border.TitledBorder;

import hugo.Colortool;


public class Munsellpicker extends JFrame {
	static MID mid;
	static String midPath;
	Timer timer;
	JLabel pointedColorLabel;
	JLabel loupeLabel;
	JTextField munsellHField;
	JTextField munsellVField;
	JTextField munsellCField;
	JTextField rgbRField;
	JTextField rgbGField;
	JTextField rgbBField;
	JTextField hsvHField;
	JTextField hsvSField;
	JTextField hsvVField;



	public static void main(String args[]) {
		midPath = "srgbd65.dat";
		mid = new MID(midPath);

		Munsellpicker frame = new Munsellpicker ();
		frame.setVisible(true);
	}

	Munsellpicker () {
		//System.out.println(Arrays.toString(mid.getHVC1000(0xFF00FF)));

		setTitle("Munsellpicker (" + midPath +")");
		setBounds(100, 100, 450, 330);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		Font fieldFont= new JTextField().getFont().deriveFont(16.0f);
		//Font labelFont = new JLabel().getFont().deriveFont(14.0f);
		setFont(fieldFont);

		//JPanel primaryPanel = new JPanel();
		//primaryPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

		// southBoxes include Munsell, RGB and HSV indicators.
		JPanel southBoxes = new JPanel();
		GridLayout boxesLayout = new GridLayout(1, 3);
		southBoxes.setLayout(boxesLayout);
		//boxes.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));


		GridLayout boxLayout = new GridLayout(4, 2);

		// construct the Munsell box
		JPanel munsellBox = new JPanel();
		munsellBox.setBorder(new TitledBorder("Munsell (" + getPreffix(midPath) + ")"));
		munsellBox.setLayout(boxLayout);
		munsellBox.setFont(fieldFont);

		munsellBox.add(new JLabel("H", JLabel.CENTER));
		munsellHField = new JTextField("N");
		munsellHField.setHorizontalAlignment(JTextField.CENTER);
		munsellHField.setFont(fieldFont);
		munsellHField.setEditable(false);
		munsellHField.setBackground(Color.WHITE);
		munsellBox.add(munsellHField, BorderLayout.CENTER);
		munsellBox.add(new JLabel("V", JLabel.CENTER));
		munsellVField = new JTextField("10.0");
		munsellVField.setHorizontalAlignment(JTextField.CENTER);
		munsellVField.setFont(fieldFont);
		munsellVField.setEditable(false);
		munsellVField.setBackground(Color.WHITE);
		munsellBox.add(munsellVField);
		munsellBox.add(new JLabel("C", JLabel.CENTER));
		munsellCField = new JTextField("0.0");
		munsellCField.setHorizontalAlignment(JTextField.CENTER);
		munsellCField.setFont(fieldFont);
		munsellCField.setEditable(false);
		munsellCField.setBackground(Color.WHITE);
		munsellBox.add(munsellCField);
		JButton munsellCopyButton = new JButton("Copy (M)");
		munsellBox.add(munsellCopyButton);

		southBoxes.add(munsellBox);


		// construct the RGB box
		JPanel rgbBox = new JPanel();
		rgbBox.setBorder(new TitledBorder("RGB"));
		rgbBox.setLayout(boxLayout);

		rgbBox.add(new JLabel("R", JLabel.CENTER));
		rgbRField = new JTextField("255");
		rgbRField.setHorizontalAlignment(JTextField.CENTER);
		rgbRField.setFont(fieldFont);
		rgbRField.setEditable(false);
		rgbRField.setBackground(Color.WHITE);
		rgbBox.add(rgbRField, BorderLayout.CENTER);
		rgbBox.add(new JLabel("G", JLabel.CENTER));
		rgbGField = new JTextField("255");
		rgbGField.setHorizontalAlignment(JTextField.CENTER);
		rgbGField.setFont(fieldFont);
		rgbGField.setEditable(false);
		rgbGField.setBackground(Color.WHITE);
		rgbBox.add(rgbGField);
		rgbBox.add(new JLabel("B", JLabel.CENTER));
		rgbBField = new JTextField("255");
		rgbBField.setHorizontalAlignment(JTextField.CENTER);
		rgbBField.setFont(fieldFont);
		rgbBField.setEditable(false);
		rgbBField.setBackground(Color.WHITE);
		rgbBox.add(rgbBField);
		//JButton rgbCopyButton = new JButton("Copy (R)");
		//rgbBox.add(rgbCopyButton);

		southBoxes.add(rgbBox);

		// construct the HSV box
		JPanel hsvBox = new JPanel();
		hsvBox.setBorder(new TitledBorder("HSV"));
		hsvBox.setLayout(boxLayout);

		hsvBox.add(new JLabel("H", JLabel.CENTER));
		hsvHField = new JTextField("0");
		hsvHField.setHorizontalAlignment(JTextField.CENTER);
		hsvHField.setFont(fieldFont);
		hsvHField.setEditable(false);
		hsvHField.setBackground(Color.WHITE);
		hsvBox.add(hsvHField, BorderLayout.CENTER);
		hsvBox.add(new JLabel("S", JLabel.CENTER));
		hsvSField = new JTextField("0");
		hsvSField.setHorizontalAlignment(JTextField.CENTER);
		hsvSField.setFont(fieldFont);
		hsvSField.setEditable(false);
		hsvSField.setBackground(Color.WHITE);
		hsvBox.add(hsvSField);
		hsvBox.add(new JLabel("V", JLabel.CENTER));
		hsvVField = new JTextField("100");
		hsvVField.setHorizontalAlignment(JTextField.CENTER);
		hsvVField.setFont(fieldFont);
		hsvVField.setEditable(false);
		hsvVField.setBackground(Color.WHITE);
		hsvBox.add(hsvVField);
		//JButton hsvCopyButton = new JButton("Dummy");
		//hsvBox.add(hsvCopyButton);

		southBoxes.add(hsvBox);


		getContentPane().add(southBoxes, BorderLayout.SOUTH);


		TimerListener timerlistener = new TimerListener();
		timer = new Timer (50, timerlistener);
		timer.start();



		// north boxes
		JPanel northBoxes = new JPanel();
		northBoxes.setLayout(boxesLayout);

		// pointed color
		JPanel pointedColorBox = new JPanel();
		pointedColorLabel = new JLabel(new ImageIcon(timerlistener.getPointedColorImg()));
		pointedColorBox.add(pointedColorLabel);
		northBoxes.add(pointedColorBox);



		// Loupe
		JPanel loupeBox = new JPanel();
		loupeLabel = new JLabel(new ImageIcon(timerlistener.getLoupeImg()));
		loupeBox.add(loupeLabel);
		northBoxes.add(loupeBox);

		// Buttons
		JPanel buttonsBox = new JPanel();
		northBoxes.add(buttonsBox);

		getContentPane().add(northBoxes, BorderLayout.NORTH);


	}

	public static String getPreffix(String fileName) {
	    if (fileName == null)
	        return null;
	    int point = fileName.lastIndexOf(".");
	    if (point != -1) {
	        return fileName.substring(0, point);
	    }
	    return fileName;
	}

	void updateMunsellBox(Color col) {
		String[] spec =mid.getMunsellSpec(col.getRGB() & 0xffffff);
		munsellHField.setText(spec[0]);
		munsellVField.setText(spec[1]);
		munsellCField.setText(spec[2]);
	}

	void updateRGBBox(Color col) {
		if (col != null) {
			rgbRField.setText(Integer.toString(col.getRed()));
			rgbGField.setText(Integer.toString(col.getGreen()));
			rgbBField.setText(Integer.toString(col.getBlue()));
		}
	}

	void updateHSVBox(Color col) {
		if (col != null) {
			float[] hsv = Color.RGBtoHSB(col.getRed(), col.getGreen(), col.getBlue(), null);
			hsvHField.setText(Integer.toString((int) Math.round(hsv[0]*360)));
			hsvSField.setText(Integer.toString((int) Math.round(hsv[1]*100)));
			hsvVField.setText(Integer.toString((int) Math.round(hsv[2]*100)));
		}
	}


	public class TimerListener implements ActionListener {
		Point point;
		BufferedImage sourceImg;
		BufferedImage loupeImg;
		int[] loupePixels;
		BufferedImage pointedColorImg;
		int[] pointedColorPixels;
		Robot robot;
		int radius;
		int scale;
		int diameter;
		int width;
		int pixelSize;

		TimerListener() {
			point = new Point(10, 10);
			try {
			    robot = new Robot();
			  } catch (AWTException e) {
			    e.printStackTrace();
			    System.exit(-1);
			  }

			reconstructImages(6, 9);

		}

		public BufferedImage getLoupeImg () {
			return loupeImg;
		}

		public BufferedImage getPointedColorImg () {
			return pointedColorImg;
		}

		public int getImgWidth () {
			return width;
		}

		public void reconstructImages (int _radius, int _scale) {
			radius = _radius;
			scale = _scale;
			diameter = 2*radius+1;
			width = diameter * scale;
			pixelSize = width * width;
			loupeImg = new BufferedImage(width, width, BufferedImage.TYPE_INT_RGB);
			loupePixels = new int[pixelSize];
			pointedColorImg = new BufferedImage(width, width, BufferedImage.TYPE_INT_RGB);
			pointedColorPixels = new int[pixelSize];
		}

		@Override
		public void actionPerformed (ActionEvent e) {
			PointerInfo pointerinfo = MouseInfo.getPointerInfo();
			if (pointerinfo != null) {
				point = pointerinfo.getLocation();

				int mouseX = (int) Math.round(point.getX());
				int mouseY = (int) Math.round(point.getY());

				Color pointedColor = robot.getPixelColor(mouseX, mouseY);
				int hex = pointedColor.getRGB();
				for(int i=0; i<pixelSize; i++)
					pointedColorPixels[i] = hex;
				pointedColorImg.setRGB(0, 0, width, width, pointedColorPixels,  0, width);
				pointedColorLabel.setIcon(new ImageIcon(pointedColorImg));

				drawNeighborhood(mouseX, mouseY);
				loupeLabel.setIcon(new ImageIcon(loupeImg));

				updateMunsellBox(pointedColor);
				updateRGBBox(pointedColor);
				updateHSVBox(pointedColor);
			}

		}

		// pg must be (radius*2+1)*scale pixels square.
		void drawNeighborhood(int centerX, int centerY) {
		  int col;
		  sourceImg = robot.createScreenCapture(new Rectangle(centerX-radius, centerY-radius, diameter, diameter));
		  int[] px = sourceImg.getRGB(0, 0, diameter, diameter, null, 0, diameter);
		  for(int y= 0; y<diameter; y++) {
		    int y_mult_diameter = y*diameter;
		    int wid_mult_y = width*y;
		    for(int x=0; x<diameter; x++) {
		      col = px[y_mult_diameter+x];
		      for(int j=0; j<scale; j++) {
		        int wid_mult_j = width*j;
		        for(int i=0; i<scale; i++)
		          loupePixels[(wid_mult_y + x)*scale + wid_mult_j + i] = col;
		      }
		    }
		  }

		  // indicate center
		  col = sourceImg.getRGB(radius, radius);
		  //System.out.println("col: " + Colortool.red(col));
		  int rectCol;
		  if(Colortool.getRoughLuminance(col) >= 128) rectCol = 0x000000;
		  else rectCol = 0xFFFFFF;
		  drawRectInPixels(loupePixels, width, width, radius*scale, radius*scale, scale, scale, rectCol);

		  loupeImg.setRGB(0, 0, width, width, loupePixels,  0, width);
		}

		void drawRectInPixels(int[] pixels, int pxWidth, int pxHeight, int x, int y, int w, int h, int hex) {
			int northWest = y * pxWidth + x;
			int northEast = y * pxWidth + x + w;
			int southWest = (y + h) * pxWidth + x;
			int southEast = (y + h) * pxWidth + x + w;
			for (int i = northWest; i<northEast; i++)
				pixels[i] = hex;
			for (int i = southWest; i<southEast; i++)
				pixels[i] = hex;
			for (int i = northWest; i<southWest; i = i+pxWidth)
				pixels[i] = hex;
			for (int i = northEast; i<southEast; i = i+pxWidth)
				pixels[i] = hex;
			pixels[southEast] = hex;
		}

	}
}
