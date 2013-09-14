package proglang.java.calculator;

import java.awt.Color;
import java.awt.Font;

import javax.swing.JLabel;
import javax.swing.border.EtchedBorder;

/**
 * Implements one digit of the output display 
 * 
 * @package   proglang.java.calculator
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 */
@SuppressWarnings("serial")
public class CalcCharacterGUI extends JLabel {

	private static Font f = new Font("Courier", Font.BOLD, 14);
	private static Color myColor= new Color(0,225,0);
	
	public CalcCharacterGUI () {
		super();

		setLabelStyle();
	}
	
	public CalcCharacterGUI (String param) {
		super(param);

		setLabelStyle();
	}
	
	private void setLabelStyle() {
//		setBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null));
		setFont(f);
//		setForeground(Color.GREEN);
		setForeground(myColor);
		setOpaque(false);
	}

}
