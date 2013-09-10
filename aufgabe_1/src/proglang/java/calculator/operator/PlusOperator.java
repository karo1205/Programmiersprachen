package proglang.java.calculator.operator;

import proglang.java.calculator.CalcUtil;
import proglang.java.calculator.ICalcContext;
import proglang.java.calculator.ICalcStack;
import proglang.java.calculator.exception.CalcException;

/**
 * Gets the two topmost elements of the stack and pushes the sum of the two onto it 
 * 
 * @package   proglang.java.calculator.operator
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 */
public class PlusOperator implements ICalcOperator {

	@Override
	public void operate(ICalcContext calcContext) throws CalcException {

		ICalcStack stack = calcContext.getStack();
		
		long op1 = CalcUtil.getLong(stack.pop());
		long op2 = CalcUtil.getLong(stack.pop());
		stack.push(Long.toString(op1 + op2));
	}
	
	public char getOperatorSign() {
		return '+';
	}

}
