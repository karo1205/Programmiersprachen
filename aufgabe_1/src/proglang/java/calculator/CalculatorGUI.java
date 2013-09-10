package proglang.java.calculator;

import javax.swing.JDialog;
import javax.swing.JSplitPane;

import java.awt.BorderLayout;

import javax.swing.JPanel;
import javax.swing.BoxLayout;
import javax.swing.JTextField;
import javax.swing.JButton;

import java.awt.GridLayout;

import javax.swing.JTextArea;
import javax.swing.JLabel;

@SuppressWarnings("serial")
public class CalculatorGUI extends JDialog {
	public CalculatorGUI() {
		
		JPanel panel = new JPanel();
		getContentPane().add(panel, BorderLayout.NORTH);
		panel.setLayout(new GridLayout(2, 64, 5, 5));
		
		JPanel panel_1 = new JPanel();
		getContentPane().add(panel_1, BorderLayout.CENTER);
		
		JTextArea textArea = new JTextArea();
		textArea.setColumns(64);
		textArea.setToolTipText("Insert your formula here...");
		textArea.setLineWrap(true);
		textArea.setRows(4);
		panel_1.add(textArea);
		
		JPanel panel_2 = new JPanel();
		panel_1.add(panel_2);
		panel_2.setLayout(new BoxLayout(panel_2, BoxLayout.Y_AXIS));
		
		JButton btnEnter = new JButton("Enter");
		btnEnter.setToolTipText("Lets compute it");
		panel_2.add(btnEnter);
		
		JButton btnClear = new JButton("Clear");
		btnClear.setToolTipText("Clear everything");
		panel_2.add(btnClear);
		
		JLabel lblCalculater = new JLabel("Calculator 0.1");
		getContentPane().add(lblCalculater, BorderLayout.SOUTH);
	}

	
}
