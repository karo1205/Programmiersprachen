package proglang.java.calculator.operator;

import proglang.java.calculator.CalcUtil;
import proglang.java.calculator.ICalcContext;
import proglang.java.calculator.ICalcDisplay;
import proglang.java.calculator.ICalcStack;
import proglang.java.calculator.exception.CalcException;
import proglang.java.calculator.exception.CalcOperationException;

/**
 * 
* @package   proglang.java.calculator
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 *
 */
public class OutputOperator implements ICalcOperator {

	@Override
	public void operate(ICalcContext calcContext) throws CalcException {

		ICalcStack stack = calcContext.getStack();
		ICalcDisplay disp = calcContext.getDisplay();
		
		// TODO Not clear which one is which (ascii or index...)
		// I suppose, that the first operand taken from the stack, that is, the topmost in the stack, is the position, 
		// the second the character to be set
		long pos = CalcUtil.getLong(stack.pop());
		String characterStr = stack.pop();	// ASCII numeric
		
		if (!disp.isInIndexRange(pos))
			throw (new CalcOperationException("Display position out of range: " + pos));
		if (!CalcUtil.isAscii(characterStr))
			throw (new CalcOperationException("Not a printable ASCII-value in printable range (32-126): " + characterStr));

		char character = (char)Integer.parseInt(characterStr);
			// correct?
		disp.setChar(pos, character);
	}
	
	/**
	 * Is the given paramter in given range of the display?
	 * @param pos
	 * @return
	 */
	public char getOperatorSign() {
		return '$';
	}

}
