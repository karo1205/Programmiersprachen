package proglang.java.calculator;

import proglang.java.calculator.exception.CalcIndexOutOfRangeException;

/**
 * Interface for the Display-implementation of the Calculator<br>
 * The Display has n rows with m characters fields. Each field can hold exactly one character and is to be set indenpendently. 
 * The default size is 4 rows and 64 columns and are indexed beginning with 0. 
 * The index (0/0) belongs to the left bottom field
 * 
 * @package   proglang.java.calculator
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 */
public interface ICalcDisplay {
	static int rows = 4;
	static int columns = 64;
	
	/**
	 * Displays the given character at the given position. Is the position is out of range it will throw a CalcIndexOutOfRangeException
	 * @param row
	 * @param column
	 * @param character
	 * @throws CalcIndexOutOfRangeException
	 */
	public void setChar(int row, int column, char character) throws CalcIndexOutOfRangeException;

	/**
	 * Clears the field at the given position. Is the position is out of range it will throw a CalcIndexOutOfRangeException
	 * @param row
	 * @param column
	 * @throws CalcIndexOutOfRangeException
	 */
	public void clearChar(int row, int column) throws CalcIndexOutOfRangeException;

	/**
	 * Clears all fields
	 */
	public void clearAll();
}
