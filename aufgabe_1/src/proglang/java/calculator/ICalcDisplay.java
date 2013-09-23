package proglang.java.calculator;

import proglang.java.calculator.exception.CalcIndexOutOfRangeException;

/**
 * Interface for the Display-implementation of the Calculator<br>
 * The Display has n rows with m characters fields. Each field can hold exactly one character and is to be set independently. 
 * The default size is 4 rows and 64 columns and are indexed beginning with 0. 
 * The index (0/0) belongs to the right bottom field
 * 
 * @package   proglang.java.calculator
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 */
public interface ICalcDisplay {

	/**
	 * Displays the given character at the given position determined by the index unmber. <br>
	 * The upper left field has index number 0, the one to the right number 1 and so on until <i>rows</i>-1.<br>
	 * The rightmost field of the next row (if present) has index <i>rows</i> and so on.<br> 
	 * If the position is out of range it will throw a CalcIndexOutOfRangeException
	 * @param row
	 * @param column
	 * @param character
	 * @throws CalcIndexOutOfRangeException
	 */
	public void setChar(long indexNumber, char character) throws CalcIndexOutOfRangeException;

	/**
	 * Displays the given character at the given position.<br>
	 * If the position is out of range it will throw a CalcIndexOutOfRangeException
	 * @param row
	 * @param column
	 * @param character
	 * @throws CalcIndexOutOfRangeException
	 */
	public void setChar(int row, int column, char character) throws CalcIndexOutOfRangeException;

	/**
	 * Returns the number of rows of the display. Default-Value shall be four as defined in the interface
	 * @return count of rows
	 */
	public int getRowCount();

	/**
	 * Returns the number of columns of the display. Default-Value shall be four as defined in the interface
	 * @return count of columns
	 */
	public int getColCount();

	/**
	 * Checks if the given index is within the range of the display. <br>
	 * The index may range from 0 to <i>rows</i>*<i>columns</i>-1. 
	 * @param index to be checked
	 * @return true when within range, false otherwise
	 */
	public boolean isInIndexRange(long index);
	
	/**
	 * Clears the field at the given position. <br>
	 * If the position is out of range it will throw a CalcIndexOutOfRangeException
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