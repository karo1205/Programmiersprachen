package proglang.java.calculator.operator;

import proglang.java.calculator.CalcUtil;
import proglang.java.calculator.ICalcContext;
import proglang.java.calculator.ICalcStack;
import proglang.java.calculator.exception.CalcException;

/**
 * Gets the topmost element of the stack and pushes its negation
 * 
 * @package   proglang.java.calculator.operator
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 */
public class NegationOperator implements ICalcOperator {

	@Override
	public void operate(ICalcContext calcContext) throws CalcException {
		ICalcStack stack = calcContext.getStack();
		
		long op1 = CalcUtil.getLong(stack.pop());
		stack.push(Long.toString(op1*(-1)));
	}
	
	public char getOperatorSign() {
		return '~';
	}

}
