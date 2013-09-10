package proglang.java.calculator;

/**
 * ICalcInput List
 * Interface for the Input List-implementation of the Calculator<br>
 * The List only works on strings, that is, only strings are stored on it. That said the list itself do not distinguish nor check on certain data types.
 * Note that an element which is "pushed" is not necessarily the one which will be returned by a following getNextElement(). 
 * The list itself is just a String of characters which are parsed according to some rules.
 *  
 * @package   proglang.java.calculator
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 */
public interface ICalcInputList {
	
	/**
	 * Gets the next element from the input list. 
	 * @return next element
	 */
	String getNextElement();

	/**
	 * pushes the given element to the first place of the input list
	 * @param element
	 */
	void pushElement(String element);
	
	/**
	 * clears all entries from the list
	 */
	void clear ();
	
	/**
	 * returns the size of the input list
	 */
	long getListSize();
	
	/**
	 * returns a String-representation of the list. The element on top shall be the leftmost in the output (so next element to be accessed is left)
	 */
	String toString();
}
