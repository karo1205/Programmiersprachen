package proglang.java.calculator;
import proglang.java.calculator.exception.*;

/**
 * Interface for the Stack-implementation of the Calculator<br>
 * The Stack only works on strings, that is, only strings are stored on the stack. That said the stack itself do not distinguish nor check on certain data types.
 * It extends a regular stack by some random-access-functionality:<ul>
 * <li>reading the n'th element</li>
 * <li>deleting the n-th element</li>
 * </ul> 
 * 
 * @package   proglang.java.calculator
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 */
public interface ICalcStack {
	
	/**
	 * pushes the given element on top of the stack
	 * @param element
	 */
	void push (String element);
	
	/**
	 * Gets the topmost element of the stack and returns it.
	 * @return topmost element of the stack
	 * @throws CalcStackEmptyException
	 */
	String pop() throws CalcStackEmptyException;
	
	/**
	 * Gets the n'th element of the stack and returns it. The element of the stack is not altered in any way.  Index starts with 1 (top of stack)
	 * @return n'th element of the stack
	 * @param position which element to return
	 * @throws CalcIndexOutOfRangeException
	 */
	String read(Long position) throws CalcIndexOutOfRangeException;
	
	/**
	 * Deletes the n'th element of the stack and returns it. Index starts with 1 (top of stack)
	 * @throws CalcIndexOutOfRangeException
	 */
	void delete(Long position) throws CalcIndexOutOfRangeException;
	
	/**
	 * clears all entries from the stack
	 */
	void clear ();
	
	/**
	 * returns a String-representation of the stack. The element on top shall be the rightmost in the output. (so next element to be accessed is left)
	 */
	String toString();
}
