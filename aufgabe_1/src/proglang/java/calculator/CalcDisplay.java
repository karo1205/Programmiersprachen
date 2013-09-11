package proglang.java.calculator;

import proglang.java.calculator.exception.CalcIndexOutOfRangeException;

/**
 * Implementation of the IDisplay-Interface.
 * 
 * @package   proglang.java.calculator
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 */

public class CalcDisplay implements ICalcDisplay {
	
	public CalcDisplay() {
//		this (ICalcDisplay.columns, ICalcDisplay.rows);
	}
	/**
	 * Not imlemented public. We will just deal with the default 4*64...
	 * @param columns
	 * @param rows
	 */
//	private CalcDisplay (int columns, int rows) {
//		this.columns = columns;
//		this.rows = rows;
//	}
	
	@Override
	public void setChar(int row, int column, char character)
			throws CalcIndexOutOfRangeException {

		checkRowCol(row, column);
		//TODO setCharacter
		System.out.println("Row: " + row + ", Columns: " + column + ". Char: " + character);
		
	}
	
	@Override
	public void setChar(long indexNumber, char character) throws CalcIndexOutOfRangeException {

		if (!isInIndexRange(indexNumber))
			throw new CalcIndexOutOfRangeException("Index number for display is out of range");
		
		setChar((int)indexNumber / ICalcDisplay.columns, (int)indexNumber % ICalcDisplay.columns, character);
//		checkRowCol(row, column);
	}

	@Override
	public void clearChar(int row, int column)
			throws CalcIndexOutOfRangeException {
		// TODO Auto-generated method stub
		checkRowCol(row, column);
	}

	@Override
	public void clearAll() {
		// TODO Auto-generated method stub
	}
	
	@Override
	public int getRowCount() {
		return rows;
	}

	@Override
	public int getColCount() {
		return columns;
	}
	
	@Override
	public boolean isInIndexRange(long index) {
		if (index >= 0 && index < ICalcDisplay.columns*ICalcDisplay.rows)
			return true;
		return false;
	}
	
	private void checkRowCol(int row, int col)
			throws CalcIndexOutOfRangeException {
		if (row >= ICalcDisplay.rows || row < 0)
			throw new CalcIndexOutOfRangeException("Row " + row + " is out of range");
		if (col >= ICalcDisplay.columns || col < 0)
			throw new CalcIndexOutOfRangeException("Column " + col + " is out of range");
	}
}
