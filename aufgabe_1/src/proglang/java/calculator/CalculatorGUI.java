package proglang.java.calculator;

import javax.swing.*;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;

import javax.swing.JPanel;
import javax.swing.BoxLayout;
import javax.swing.JTextField;
import javax.swing.JButton;

import java.awt.GridLayout;
import java.awt.event.ActionListener;

import javax.swing.JTextArea;
import javax.swing.JLabel;

import proglang.java.calculator.exception.CalcIndexOutOfRangeException;

import javax.swing.border.EtchedBorder;
import javax.swing.border.BevelBorder;

@SuppressWarnings("serial")
public class CalculatorGUI extends JDialog implements ICalcDisplayGUI  {
	// Default display: 4*64
	private CalcCharacterGUI[][] characterMap;
	private JPanel displayPanel;
	private String emptyFieldString = new String(".");
	private JTextArea formulaArea ;
	private JLabel lblErrorMessage;
	int rowCount, colCount;
	
	public CalculatorGUI(ActionListener al) {
		// TODO Make GUI
		this.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		
		BorderLayout borderLayout = (BorderLayout) getContentPane().getLayout();
		borderLayout.setHgap(10);
		borderLayout.setVgap(10);
		getContentPane().setLayout(borderLayout);
		
		displayPanel = new JPanel();
		getContentPane().add(displayPanel, BorderLayout.NORTH);
		displayPanel.setBorder(new BevelBorder(BevelBorder.LOWERED, null, null, null, null));
						
		JPanel inputPanel = new JPanel();
		getContentPane().add(inputPanel, BorderLayout.CENTER);
		inputPanel.setLayout(new BorderLayout(5, 5));
		
		JPanel buttonPanel = new JPanel();
		inputPanel.add(buttonPanel,  BorderLayout.EAST);
		buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.Y_AXIS));
		
		JButton btnEnter = new JButton("Enter");
		btnEnter.setMnemonic('E');
		btnEnter.setActionCommand("enter");
		btnEnter.setToolTipText("Lets compute it");
		buttonPanel.add(btnEnter);
		btnEnter.addActionListener(al);
		
		JButton btnClear = new JButton("Clear");
		btnClear.setMnemonic('C');
		btnClear.setActionCommand("clear");
		btnClear.setToolTipText("Clear everything");
		buttonPanel.add(btnClear);
		btnClear.addActionListener(al);
		
		formulaArea = new JTextArea();
		formulaArea.setColumns(64);
		formulaArea.setToolTipText("Insert your formula here...");
		formulaArea.setLineWrap(true);
		formulaArea.setRows(8);
		
		JScrollPane scrollPane = new JScrollPane(formulaArea);
		scrollPane.setViewportBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null));
		inputPanel.add(scrollPane);
		
		lblErrorMessage = new JLabel("");
		getContentPane().add(lblErrorMessage, BorderLayout.SOUTH);
		this.pack();
		this.setResizable(false);
		this.setTitle("Calculator 0.1");
	}

	// --------------------------------------
	// ICalcDisplayGUI - implementations
	// --------------------------------------
	
	public void setDisplaySize(int rows, int cols) {
		rowCount = rows; colCount = cols;
		
		characterMap = new CalcCharacterGUI[cols][rows];
		displayPanel.removeAll();
		displayPanel.setLayout(new GridLayout(rows, cols, 1, 1));
		for (int r = rows-1; r >= 0;  r--) {
			for (int c = cols-1; c >= 0; c--) {
				characterMap[c][r] = new CalcCharacterGUI(emptyFieldString);
				displayPanel.add(characterMap[c][r]);		
			}
		}
	}
	
	public void setCharacter(int row, int column, char character) throws CalcIndexOutOfRangeException {
		characterMap[column][row].setText(Character.toString(character)); 
	}

	public void clearCharacter(int row, int column) throws CalcIndexOutOfRangeException {
		characterMap[column][row].setText(emptyFieldString); 
	}
	
	public void clearAll() {
		for (int r = rowCount-1; r >= 0;  r--) {
			for (int c = colCount-1; c >= 0; c--) {
				characterMap[c][r].setText(emptyFieldString); 
			}
		}
		formulaArea.setText("");
		
	}
	
	public String getFormulaText() {
		return formulaArea.getText();
	}
	
	public void setCommentLineText(String text) {
		lblErrorMessage.setText(text);
	}
}
