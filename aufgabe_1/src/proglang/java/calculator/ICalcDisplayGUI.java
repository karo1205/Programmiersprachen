package proglang.java.calculator;

import proglang.java.calculator.exception.CalcIndexOutOfRangeException;

/**
 * The DisplayGUI. Methods to be implemented by the GUI for the Display.
 * @package   proglang.java.calculator.operator
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 *
 */
public interface ICalcDisplayGUI {
	/**
	 * Sets the size of the display. Resizing will clear the display.
	 * @param rows
	 * @param cols
	 */
	public void setDisplaySize(int rows, int cols);
	
	/**
	 * Puts the given character to the given position. Upper left field has index (0/0).
	 * If the position is out of range it will throw a CalcIndexOutOfRangeException
	 * @param row
	 * @param column
	 * @param character
	 * @throws CalcIndexOutOfRangeException
	 */
	public void setCharacter(int row, int column, char character) throws CalcIndexOutOfRangeException;;

	/**
	 * Clears the field at the given position. <br>
	 * If the position is out of range it will throw a CalcIndexOutOfRangeException
	 * @param row
	 * @param column
	 * @throws CalcIndexOutOfRangeException
	 */
	public void clearCharacter(int row, int column) throws CalcIndexOutOfRangeException;
	
	/**
	 * Clears all fields
	 */
	public void clearAll();
	
}

