package proglang.java.calculator;

import java.awt.Color;
import java.awt.Font;

import javax.swing.JLabel;

/**
 * Implements one digit of the output display 
 * 
 * @package   proglang.java.calculator
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 */
@SuppressWarnings("serial")
public class CalcCharacterGUI extends JLabel {

	private static Font f = new Font("Courier", Font.BOLD, 12);
	
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
		setForeground(Color.DARK_GRAY);
	}

}
