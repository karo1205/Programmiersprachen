package proglang.java.calculator.operator;

import proglang.java.calculator.CalcUtil;
import proglang.java.calculator.ICalcContext;
import proglang.java.calculator.ICalcStack;
import proglang.java.calculator.exception.CalcException;

/**
 * Gets the two topmost elements of the stack and pushes a truth value onto it depending whether the second operand is smaller than the first  
 * 
 * @package   proglang.java.calculator.operator
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 */
public class SmallerThanOperator implements ICalcOperator {

	@Override
	public void operate(ICalcContext calcContext) throws CalcException {
		ICalcStack stack = calcContext.getStack();
		
		long opRight = CalcUtil.getLong(stack.pop());
		long opLeft = CalcUtil.getLong(stack.pop());
		if (opLeft < opRight)
			stack.push(CalcUtil.getBooleanString(true));
		else
			stack.push(CalcUtil.getBooleanString(false));


	}

	public char getOperatorSign() {
		return '<';
	}

}
