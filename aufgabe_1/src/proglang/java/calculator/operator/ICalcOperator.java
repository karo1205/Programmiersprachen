package proglang.java.calculator.operator;
import proglang.java.calculator.ICalcContext;
import proglang.java.calculator.exception.CalcException;

/**
 * Base Class for all operators of the Calculator.<br>
 * Basically an operator takes arguments from the stack and puts results to the stack (and sometimes the input list). 
 * 
 * @package   proglang.java.calculator.operator
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 */
public interface ICalcOperator {
	/**
	 * Executes the operator.<br>
	 * As the operator might (will) operate on Stack, List, and maybe Display, the main Calculator Context class is passed a a container for all of this classes.
	 * So the Calculator works as a kind of context to access them. 
	 * @param calc
	 * @throws CalcException
	 */
	void operate(ICalcContext calcContext) throws CalcException;
	
	/**
	 * Gets the name of the operator.<br>
	 * It is supposed, that the operator only have one character, like '+' or '#' ...
	 * @return name of the operator (always on character)
	 */
	public char getOperatorSign();
	
}
