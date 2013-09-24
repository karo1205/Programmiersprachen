package proglang.java.calculator;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.WindowConstants;
import javax.swing.border.BevelBorder;
import javax.swing.border.EtchedBorder;

import proglang.java.calculator.Calculator.ModeType;
import proglang.java.calculator.exception.CalcIndexOutOfRangeException;

@SuppressWarnings("serial")
public class CalculatorGUI extends JFrame implements ICalcDisplayGUI  {

	private CalcCharacterGUI[][] characterMap;
	private JPanel displayPanel;
	private String emptyFieldString = new String(".");
	private JTextArea formulaArea ;
	private JLabel lblErrorMessage;
	int rowCount, colCount;
	Calculator calc;
	
	public CalculatorGUI(Calculator calc) {

		this.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		
		BorderLayout borderLayout = (BorderLayout) getContentPane().getLayout();
		borderLayout.setHgap(10);
		borderLayout.setVgap(10);
		getContentPane().setLayout(borderLayout);

		this.calc = calc;

		displayPanel = new JPanel();
		getContentPane().add(displayPanel, BorderLayout.NORTH);
		displayPanel.setBorder(new BevelBorder(BevelBorder.LOWERED, null, null, null, null));
		displayPanel.setBackground(Color.BLACK);

		KeyListener keyListener = new KeyListener() {

			@Override
			public void keyTyped(KeyEvent arg0) {
				parseKeyStroke(arg0);
			}

			@Override
			public void keyReleased(KeyEvent arg0) {
				if(arg0.getKeyCode()==KeyEvent.VK_ENTER) {
					parseKeyStroke(arg0);
				}
			}

			@Override
			public void keyPressed(KeyEvent arg0) {
				// TODO Auto-generated method stub

			}
		};

		JPanel inputPanel = new JPanel();
		getContentPane().add(inputPanel, BorderLayout.CENTER);
		inputPanel.setLayout(new BorderLayout(5, 5));
		
		JPanel buttonPanel = new JPanel();
		inputPanel.add(buttonPanel,  BorderLayout.EAST);
		buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.Y_AXIS));
		
		JButton btnClear = new JButton("Clear");
		btnClear.setMnemonic('C');

		btnClear.setActionCommand("clear");
		btnClear.setToolTipText("Clear everything");
		buttonPanel.add(btnClear);
		btnClear.addActionListener(calc);
		
		formulaArea = new JTextArea();
		formulaArea.setRows(8);
		formulaArea.setColumns(64);
		formulaArea.setToolTipText("Insert your formula here...");
		//formulaArea.setLineWrap(true);
		formulaArea.addKeyListener(keyListener);
		formulaArea.setEditable(false);

        JScrollPane scrollPane = new JScrollPane(formulaArea);
		scrollPane.setViewportBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null));
		inputPanel.add(scrollPane);
		scrollPane.setPreferredSize(new Dimension (800, 250));
		
		lblErrorMessage = new JLabel("");
		getContentPane().add(lblErrorMessage, BorderLayout.SOUTH);
		this.pack();
		this.setResizable(false);
		this.setTitle("Calculator 0.1");
	}

	protected void parseKeyStroke(KeyEvent ev){
		if(calc.getMode()==ModeType.NORMAL) {
			formulaArea.setText(null);
			formulaArea.repaint();
		}
		calc.keyPressed(ev,formulaArea.getText().replace('\n',' ').trim());
		if(ev.getKeyCode()==KeyEvent.VK_ENTER) {
			formulaArea.setText(null);
		}
	}

	public String getFormulaText() {
		return formulaArea.getText();
	}
	
	public void setCommentLineText(String text) {
		lblErrorMessage.setText(text);
	}
	
	public void setFocus() {
		formulaArea.requestFocusInWindow();	
	}

	// --------------------------------------
	// ICalcDisplayGUI - implementations
	// --------------------------------------
	
	@Override
	public void setDisplaySize(int rows, int cols) {
		rowCount = rows; colCount = cols;
		
		characterMap = new CalcCharacterGUI[rows][cols];
		displayPanel.removeAll();
		displayPanel.setLayout(new GridLayout(rows, cols, 1, 1));
		for (int r = 0; r <= rows-1;  r++) {
			for (int c = 0; c <= cols-1; c++) {
				characterMap[r][c] = new CalcCharacterGUI(emptyFieldString);
				displayPanel.add(characterMap[r][c]);		
			}
		}
	}
	@Override
	public void setCharacter(int row, int column, char character) throws CalcIndexOutOfRangeException {
		characterMap[row][column].setText(Character.toString(character)); 
	}

	@Override
	public void clearCharacter(int row, int column) throws CalcIndexOutOfRangeException {
		characterMap[row][column].setText(emptyFieldString); 
	}
	
	@Override
	public void clearAll() {
		for (int r = 0; r <= rowCount-1;  r++) {
			for (int c = 0; c <= colCount-1; c++) {
				characterMap[r][c].setText(emptyFieldString); 
			}
		}
		formulaArea.setText(null);
		formulaArea.setEditable(false);
		setFocus();
	}

	public void switchMode() {
		formulaArea.setText(null);
		if(calc.getMode()==ModeType.NORMAL)
			formulaArea.setEditable(false);
		else
			formulaArea.setEditable(true);
	}
}
