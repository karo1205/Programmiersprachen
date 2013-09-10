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
	private int rows;
	private int columns;
	
	public CalcDisplay() {
		this (ICalcDisplay.columns, ICalcDisplay.rows);
	}
	
	public CalcDisplay (int columns, int rows) {
		this.columns = columns;
		this.rows = rows;
	}
	
	@Override
	public void setChar(int row, int column, char character)
			throws CalcIndexOutOfRangeException {
		// TODO Auto-generated method stub
		checkRowCol(row, column);
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
	private void checkRowCol(int row, int col)
			throws CalcIndexOutOfRangeException {
		if (row >= this.rows || row < 0)
			throw new CalcIndexOutOfRangeException("Row " + row + " is out of range");
		if (col >= this.columns || col < 0)
			throw new CalcIndexOutOfRangeException("Column " + col + " is out of range");
			
		
	}

}
