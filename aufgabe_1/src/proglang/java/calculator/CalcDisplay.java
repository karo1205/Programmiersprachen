package proglang.java.calculator;

import proglang.java.calculator.exception.CalcIndexOutOfRangeException;

/**
 * Implementation of the IDisplay-Interface.
 * 
 * @package   proglang.java.calculator
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 */

@SuppressWarnings("all")
public class CalcDisplay implements ICalcDisplay {
	
	private ICalcDisplayGUI calcDisplayGUI;
	private int displayRows;
	private int displayCols;
	/**
	 * main constructor, setting the display and the display size in rows and cols
	 * @param calcDisplayGUI
	 * @param rows
	 * @param cols
	 */
	public CalcDisplay(ICalcDisplayGUI calcDisplayGUI, int rows, int cols) {
		this.calcDisplayGUI = calcDisplayGUI;
		this.displayRows = rows;
		this.displayCols = cols;
		calcDisplayGUI.setDisplaySize(rows, cols);
	}
	
	// no 
	private CalcDisplay() {}
	
	@Override
	public void setChar(int row, int column, char character)
			throws CalcIndexOutOfRangeException {

		checkRowCol(row, column);

		System.out.println("Row: " + row + ", Columns: " + column + ". Char: " + character);
		calcDisplayGUI.setCharacter(row, column, character);
	}
	
	@Override
	public void setChar(long indexNumber, char character) throws CalcIndexOutOfRangeException {

		if (!isInIndexRange(indexNumber))
			throw new CalcIndexOutOfRangeException("Index number for display is out of range");
		
		setChar((int)indexNumber / displayCols, (int)indexNumber % displayCols, character);
	}

	@Override
	public void clearChar(int row, int column)
			throws CalcIndexOutOfRangeException {
		checkRowCol(row, column);
	}

	@Override
	public void clearAll() {
		calcDisplayGUI.clearAll();
	}
	
	@Override
	public int getRowCount() {
		return displayRows;
	}

	@Override
	public int getColCount() {
		return displayCols;
	}
	
	@Override
	public boolean isInIndexRange(long index) {
		if (index >= 0 && index < displayCols * displayRows)
			return true;
		return false;
	}
	
	private void checkRowCol(int row, int col)
			throws CalcIndexOutOfRangeException {
		if (row >= displayRows || row < 0)
			throw new CalcIndexOutOfRangeException("Row " + row + " is out of range");
		if (col >= displayCols || col < 0)
			throw new CalcIndexOutOfRangeException("Column " + col + " is out of range");
	}
}