package com.privetkitty;
import java.awt.AWTException;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.MouseInfo;
import java.awt.Point;
import java.awt.PointerInfo;
import java.awt.Rectangle;
import java.awt.Robot;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.Arrays;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.Timer;
import javax.swing.border.TitledBorder;

import com.qiita.JeJeNeNo.Utils;


public class Munsellpicker extends JFrame {
	MID mid;
	String[] midFiles;
	int currentMidIndex;
	String defaultMidFile = "sRGB-D65.dat";
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
	JButton munsellCopyButton;
	JButton rgbCopyButton;
	JButton hsvCopyButton;
	Robot robot;
	Point point;


	public static void main(String[] args) {
		Munsellpicker frame = new Munsellpicker ();
		frame.setVisible(true);
	}

	Munsellpicker () {
		// Find all data files: they are in the same directory and whose extension is '.dat'.
		midFiles = Utils.listUpFiles(Paths.get("."), "dat", false)
				.stream()
				.map(file -> file.getName())
				.toArray(String[]::new);
		if (midFiles.length == 0) {
			System.out.println("No .dat files found.");
			System.exit(1);
		}

		// Read the default .dat file.
		currentMidIndex = Arrays.asList(midFiles).indexOf(defaultMidFile);
		if(currentMidIndex == -1) {
			System.out.println("Coundn't find " + defaultMidFile +". Use " + midFiles[0] + " instead");
			currentMidIndex = 0;
		}

		try {
			mid = new MID(midFiles[currentMidIndex]);
		} catch (IOException e) {
			e.printStackTrace();
			System.exit(1);
		}

		// Make listeners for components and the keyboard.
		CopyButtonListener copyButtonListener = new CopyButtonListener();
		CopyKeyListener myKeyListener = new CopyKeyListener();
		this.addKeyListener(myKeyListener);
		this.setFocusable(true);

		// Set up the whole frame.
		setTitle("Munsellpicker: " + midFiles[currentMidIndex]);
		setBounds(100, 100, 450, 335);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		Font fieldFont= new JTextField().getFont().deriveFont(16.0f);
		Font labelFont = new JLabel().getFont().deriveFont(14.0f);
		setFont(fieldFont);

		// The frame is divided into the two JPanels, northPanel and southPanel.
		// southPanel contains the indicators for Munsell, RGB, and HSV.
		JPanel southPanel = new JPanel();
		GridLayout boxesLayout = new GridLayout(1, 3);
		southPanel.setLayout(boxesLayout);

		// Layout for southPanel
		GridBagLayout boxLayout = new GridBagLayout();

		GridBagConstraints coord1 = new GridBagConstraints(0, 0, 1, 1, 1.0, 1.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(1, 1, 1, 1), 0, 0);
		GridBagConstraints coord2 = new GridBagConstraints(1, 0, 1, 1, 1.0, 1.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(1, 1, 1, 1), 0, 0);
		GridBagConstraints coord3 = new GridBagConstraints(0, 1, 1, 1, 1.0, 1.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(1, 1, 1, 1), 0, 0);
		GridBagConstraints coord4 = new GridBagConstraints(1, 1, 1, 1, 1.0, 1.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(1, 1, 1, 1), 0, 0);
		GridBagConstraints coord5 = new GridBagConstraints(0, 2, 1, 1, 1.0, 1.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(1, 1, 1, 1), 0, 0);
		GridBagConstraints coord6 = new GridBagConstraints(1, 2, 1, 1, 1.0, 1.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(1, 1, 1, 1), 0, 0);
		GridBagConstraints coord7 = new GridBagConstraints(0, 3, 2, 1, 1.0, 1.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(1, 1, 1, 1), 1, 1);

		// Construct the Munsell panel.
		JPanel munsellPanel = new JPanel();
		TitledBorder munsellTitle = new TitledBorder("Munsell");
		munsellTitle.setTitleFont(labelFont);
		munsellPanel.setBorder(munsellTitle);
		munsellPanel.setLayout(boxLayout);
		munsellPanel.setFont(fieldFont);

		JLabel munsellHLabel = new JLabel("H", JLabel.CENTER);
		munsellHLabel.setFont(labelFont);
		boxLayout.setConstraints(munsellHLabel, coord1);
		munsellPanel.add(munsellHLabel);
		munsellHField = new JTextField("10.0");
		munsellHField.setHorizontalAlignment(JTextField.CENTER);
		munsellHField.setFont(fieldFont);
		munsellHField.setEditable(false);
		munsellHField.setBackground(Color.WHITE);
		boxLayout.setConstraints(munsellHField, coord2);
		munsellPanel.add(munsellHField);

		JLabel munsellVLabel = new JLabel("V", JLabel.CENTER);
		munsellVLabel.setFont(labelFont);
		boxLayout.setConstraints(munsellVLabel, coord3);
		munsellPanel.add(munsellVLabel);
		munsellVField = new JTextField("10.0");
		munsellVField.setHorizontalAlignment(JTextField.CENTER);
		munsellVField.setFont(fieldFont);
		munsellVField.setEditable(false);
		munsellVField.setBackground(Color.WHITE);
		boxLayout.setConstraints(munsellVField, coord4);
		munsellPanel.add(munsellVField);

		JLabel munsellCLabel = new JLabel("C", JLabel.CENTER);
		munsellCLabel.setFont(labelFont);
		boxLayout.setConstraints(munsellCLabel, coord5);
		munsellPanel.add(munsellCLabel);
		munsellCField = new JTextField("0.0");
		munsellCField.setHorizontalAlignment(JTextField.CENTER);
		munsellCField.setFont(fieldFont);
		munsellCField.setEditable(false);
		munsellCField.setBackground(Color.WHITE);
		boxLayout.setConstraints(munsellCField, coord6);
		munsellPanel.add(munsellCField);

		munsellCopyButton = new JButton("Copy (M)");
		munsellCopyButton.addActionListener(copyButtonListener);
		boxLayout.setConstraints(munsellCopyButton, coord7);
		munsellPanel.add(munsellCopyButton);

		southPanel.add(munsellPanel);


		// Construct the RGB panel.
		JPanel rgbPanel = new JPanel();
		TitledBorder rgbTitle = new TitledBorder("RGB");
		rgbTitle.setTitleFont(labelFont);
		rgbPanel.setBorder(rgbTitle);
		rgbPanel.setLayout(boxLayout);
		rgbPanel.setFont(fieldFont);

		JLabel rgbRLabel = new JLabel("R", JLabel.CENTER);
		rgbRLabel.setFont(labelFont);
		boxLayout.setConstraints(rgbRLabel, coord1);
		rgbPanel.add(rgbRLabel);
		rgbRField = new JTextField("255");
		rgbRField.setHorizontalAlignment(JTextField.CENTER);
		rgbRField.setFont(fieldFont);
		rgbRField.setEditable(false);
		rgbRField.setBackground(Color.WHITE);
		boxLayout.setConstraints(rgbRField, coord2);
		rgbPanel.add(rgbRField);

		JLabel rgbGLabel = new JLabel("G", JLabel.CENTER);
		rgbGLabel.setFont(labelFont);
		boxLayout.setConstraints(rgbGLabel, coord3);
		rgbPanel.add(rgbGLabel);
		rgbGField = new JTextField("255");
		rgbGField.setHorizontalAlignment(JTextField.CENTER);
		rgbGField.setFont(fieldFont);
		rgbGField.setEditable(false);
		rgbGField.setBackground(Color.WHITE);
		boxLayout.setConstraints(rgbGField, coord4);
		rgbPanel.add(rgbGField);

		JLabel rgbBLabel = new JLabel("B", JLabel.CENTER);
		rgbBLabel.setFont(labelFont);
		boxLayout.setConstraints(rgbBLabel, coord5);
		rgbPanel.add(rgbBLabel);
		rgbBField = new JTextField("255");
		rgbBField.setHorizontalAlignment(JTextField.CENTER);
		rgbBField.setFont(fieldFont);
		rgbBField.setEditable(false);
		rgbBField.setBackground(Color.WHITE);
		boxLayout.setConstraints(rgbBField, coord6);
		rgbPanel.add(rgbBField);

		rgbCopyButton = new JButton("Copy (R)");
		rgbCopyButton.addActionListener(copyButtonListener);
		boxLayout.setConstraints(rgbCopyButton, coord7);
		rgbPanel.add(rgbCopyButton);

		southPanel.add(rgbPanel);


		// Construct the HSV panel.
		JPanel hsvPanel = new JPanel();
		TitledBorder hsvTitle = new TitledBorder("HSV");
		hsvTitle.setTitleFont(labelFont);
		hsvPanel.setBorder(hsvTitle);
		hsvPanel.setBorder(hsvTitle);
		hsvPanel.setLayout(boxLayout);
		hsvPanel.setFont(fieldFont);

		JLabel hsvHLabel = new JLabel("H", JLabel.CENTER);
		hsvHLabel.setFont(labelFont);
		boxLayout.setConstraints(hsvHLabel, coord1);
		hsvPanel.add(hsvHLabel);
		hsvHField = new JTextField("360");
		hsvHField.setHorizontalAlignment(JTextField.CENTER);
		hsvHField.setFont(fieldFont);
		hsvHField.setEditable(false);
		hsvHField.setBackground(Color.WHITE);
		boxLayout.setConstraints(hsvHField, coord2);
		hsvPanel.add(hsvHField);

		JLabel hsvSLabel = new JLabel("S", JLabel.CENTER);
		hsvSLabel.setFont(labelFont);
		boxLayout.setConstraints(hsvSLabel, coord3);
		hsvPanel.add(hsvSLabel);
		hsvSField = new JTextField("0");
		hsvSField.setHorizontalAlignment(JTextField.CENTER);
		hsvSField.setFont(fieldFont);
		hsvSField.setEditable(false);
		hsvSField.setBackground(Color.WHITE);
		boxLayout.setConstraints(hsvSField, coord4);
		hsvPanel.add(hsvSField);

		JLabel hsvVLabel = new JLabel("V", JLabel.CENTER);
		hsvVLabel.setFont(labelFont);
		boxLayout.setConstraints(hsvVLabel, coord5);
		hsvPanel.add(hsvVLabel);
		hsvVField = new JTextField("0");
		hsvVField.setHorizontalAlignment(JTextField.CENTER);
		hsvVField.setFont(fieldFont);
		hsvVField.setEditable(false);
		hsvVField.setBackground(Color.WHITE);
		boxLayout.setConstraints(hsvVField, coord6);
		hsvPanel.add(hsvVField);

		hsvCopyButton = new JButton("Copy (H)");
		hsvCopyButton.addActionListener(copyButtonListener);
		boxLayout.setConstraints(hsvCopyButton, coord7);
		hsvPanel.add(hsvCopyButton);

		southPanel.add(hsvPanel);
		getContentPane().add(southPanel, BorderLayout.SOUTH);


		// northPanel displays the pointed color, loupe, and miscellaneaous buttons.
		JPanel northPanel = new JPanel();
		northPanel.setLayout(boxesLayout);

		TimerListener timerlistener = new TimerListener();
		timer = new Timer (50, timerlistener);
		timer.start();

		// Pointed color
		JPanel pointedColorBox = new JPanel();
		pointedColorLabel = new JLabel(new ImageIcon(timerlistener.getPointedColorImg()));
		pointedColorBox.add(pointedColorLabel);
		northPanel.add(pointedColorBox);


		// Loupe
		JPanel loupeBox = new JPanel();
		loupeLabel = new JLabel(new ImageIcon(timerlistener.getLoupeImg()));
		loupeBox.add(loupeLabel);
		northPanel.add(loupeBox);

		// Buttons
		JPanel buttonsBox = new JPanel();

		// Combo box for selecting a data file
		JComboBox<String> midCombo = new JComboBox<String>(midFiles);
		midCombo.setSelectedIndex(currentMidIndex);
		midCombo.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				int newMidIndex = midCombo.getSelectedIndex();
				if (newMidIndex != -1) {
					String newMidFile = midFiles[newMidIndex];
					System.out.println("Loading "+ newMidFile + "...");
					try { mid.loadData(newMidFile); }
					catch (IOException e1) {
						e1.printStackTrace();
						String errorMsg = "Failed to read " + midFiles[newMidIndex];
						System.out.println(errorMsg);
						setTitle(errorMsg);
						return;
					}
					System.out.println(newMidFile + " successfully loaded.");
					setTitle("Munsellpicker: " + midFiles[newMidIndex]);
					currentMidIndex = newMidIndex;
				}
			}
		});
		buttonsBox.add(midCombo);
		northPanel.add(buttonsBox);
		getContentPane().add(northPanel, BorderLayout.NORTH);
	}

	public Color getPointedColor(PointerInfo pointerInfo) {
		if (pointerInfo != null) {
			point = pointerInfo.getLocation();

			int mouseX = (int) Math.round(point.getX());
			int mouseY = (int) Math.round(point.getY());

			return robot.getPixelColor(mouseX, mouseY);
		} else
			return new Color(0, 0, 0);
	}

	public static String getPrefix(String fileName) {
		if (fileName == null)
			return null;
		int point = fileName.lastIndexOf(".");
		if (point != -1) {
			return fileName.substring(0, point);
		}
		return fileName;
	}

	void updateMunsellPanel(Color col) {
		String[] spec =mid.getMunsellSpec(col.getRGB() & 0xffffff);
		munsellHField.setText(spec[0]);
		munsellVField.setText(spec[1]);
		munsellCField.setText(spec[2]);
	}

	void updateRGBPanel(Color col) {
		if (col != null) {
			rgbRField.setText(Integer.toString(col.getRed()));
			rgbGField.setText(Integer.toString(col.getGreen()));
			rgbBField.setText(Integer.toString(col.getBlue()));
		}
	}

	void updateHSVPanel(Color col) {
		if (col != null) {
			float[] hsv = Color.RGBtoHSB(col.getRed(), col.getGreen(), col.getBlue(), null);
			hsvHField.setText(Integer.toString((int) Math.round(hsv[0]*360)));
			hsvSField.setText(Integer.toString((int) Math.round(hsv[1]*100)));
			hsvVField.setText(Integer.toString((int) Math.round(hsv[2]*100)));
		}
	}

	// Processing for every frame
	public class TimerListener implements ActionListener {
		BufferedImage sourceImg;
		BufferedImage loupeImg;
		int[] loupePixels;
		BufferedImage pointedColorImg;
		int[] pointedColorPixels;
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

		public void actionPerformed (ActionEvent e) {
			PointerInfo pointerInfo = MouseInfo.getPointerInfo();
			Color pointedColor = getPointedColor(pointerInfo);
			int hex = pointedColor.getRGB();
			for(int i=0; i<pixelSize; i++)
				pointedColorPixels[i] = hex;
			pointedColorImg.setRGB(0, 0, width, width, pointedColorPixels,  0, width);
			pointedColorLabel.setIcon(new ImageIcon(pointedColorImg));

			int mouseX = (int) Math.round(point.getX());
			int mouseY = (int) Math.round(point.getY());

			drawNeighborhood(mouseX, mouseY);
			loupeLabel.setIcon(new ImageIcon(loupeImg));

			updateMunsellPanel(pointedColor);
			updateRGBPanel(pointedColor);
			updateHSVPanel(pointedColor);
		}

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

			// Emphasize center.
			col = sourceImg.getRGB(radius, radius);
			int rectCol;
			if(Colortool.getRoughLuminance(col) >= 128)
				rectCol = 0x000000;
			else
				rectCol = 0xFFFFFF;
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

	public class CopyButtonListener implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			PointerInfo pointerInfo = MouseInfo.getPointerInfo();
			Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
			Color col = getPointedColor(pointerInfo);

			if (e.getSource() == munsellCopyButton) {
				String munsellstr = mid.getMunsellString(col.getRGB());
				StringSelection selection = new StringSelection(munsellstr);
				clipboard.setContents(selection, selection);
			}
			if (e.getSource() == rgbCopyButton) {
				String hexstr = String.format("%06x", col.getRGB() & 0x00ffffff);
				StringSelection selection = new StringSelection(hexstr);
				clipboard.setContents(selection, selection);
			}
			if (e.getSource() == hsvCopyButton) {
				float[] hsv = Color.RGBtoHSB(col.getRed(), col.getGreen(), col.getBlue(), null);
				String hsvstr = String.format("%d %d %d", Math.round(hsv[0]*360), Math.round(hsv[1]*100), Math.round(hsv[2]*100));
				StringSelection selection = new StringSelection(hsvstr);
				clipboard.setContents(selection, selection);
			}
		}
	}

	public class CopyKeyListener implements KeyListener {
		public void keyPressed(KeyEvent e) {
			char key = e.getKeyChar();
			switch(key) {
			case 'm':
				munsellCopyButton.doClick();
				break;
			case 'r':
				rgbCopyButton.doClick();
				break;
			case 'h':
				hsvCopyButton.doClick();
				break;
			}
		}

		public void keyReleased(KeyEvent e){
		}

		public void keyTyped(KeyEvent e){
		}
	}
}

