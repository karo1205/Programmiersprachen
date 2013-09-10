package proglang.java.calculator.operator;

import proglang.java.calculator.CalcUtil;
import proglang.java.calculator.ICalcContext;
import proglang.java.calculator.ICalcStack;
import proglang.java.calculator.exception.CalcException;

/**
 *  Deletes the topmost element n and also the n'th element of the stack.<br>
 *  Throws an exception if the topmost element is no positive number.
 * 
 * @package   proglang.java.calculator.operator
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 */
public class DeleteOperator implements ICalcOperator {

	@Override
	public void operate(ICalcContext calcContext) throws CalcException {
		ICalcStack stack = calcContext.getStack();
		
		long op1 = CalcUtil.getLong(stack.pop());
		if (op1 < 1)
			throw (new CalcException("First element must be positive."));

		stack.delete(op1-1);
	}

	public char getOperatorSign() {
		return '#';
	}

}
