package proglang.java.calculator.operator;

import proglang.java.calculator.CalcUtil;
import proglang.java.calculator.ICalcContext;
import proglang.java.calculator.ICalcStack;
import proglang.java.calculator.exception.CalcException;

/**
 * Gets the two topmost elements of the stack and pushes the logical or of them onto it 
 * 
 * @package   proglang.java.calculator.operator
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 */
public class OrOperator implements ICalcOperator {

	@Override
	public void operate(ICalcContext calcContext) throws CalcException {

		ICalcStack stack = calcContext.getStack();
		
		boolean op1 = CalcUtil.getBoolean(stack.pop());
		boolean op2 = CalcUtil.getBoolean(stack.pop());
		stack.push(CalcUtil.getBooleanString(op2 || op1));

	}

	public char getOperatorSign() {
		return '|';
	}

}
