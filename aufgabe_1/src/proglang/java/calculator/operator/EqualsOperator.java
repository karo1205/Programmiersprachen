package proglang.java.calculator.operator;

import proglang.java.calculator.CalcUtil;
import proglang.java.calculator.ICalcContext;
import proglang.java.calculator.ICalcStack;
import proglang.java.calculator.exception.CalcException;

/**
 * Gets the two topmost elements of the stack and pushes a truth value onto it depending whether the second operand equals to the first  
 * 
 * @package   proglang.java.calculator.operator
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 */
public class EqualsOperator implements ICalcOperator {

	// interpretation: just compare the two strings popped from stack...
	@Override
	public void operate(ICalcContext calcContext) throws CalcException {
		ICalcStack stack = calcContext.getStack();
		
		String op1 = stack.pop();
		String op2 = stack.pop();
		if (op1.equals(op2))
			stack.push(CalcUtil.getBooleanString(true));
		else
			stack.push(CalcUtil.getBooleanString(false));
	}
	
	public char getOperatorSign() {
		return '=';
	}


}
