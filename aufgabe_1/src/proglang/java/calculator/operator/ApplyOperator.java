package proglang.java.calculator.operator;

import proglang.java.calculator.CalcUtil;
import proglang.java.calculator.ICalcContext;
import proglang.java.calculator.ICalcInputList;
import proglang.java.calculator.ICalcStack;
import proglang.java.calculator.exception.CalcException;

/**
 *  Gets the topmost element of the stack. If it is a braced expression it will be put to the front of the input list 
 *  (with white spaces instead of the braces) so it will be handled next in queue<br>
 * 
 * @package   proglang.java.calculator.operator
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 */
public class ApplyOperator implements ICalcOperator {

	@Override
	public void operate(ICalcContext calcContext) throws CalcException {
		ICalcStack stack = calcContext.getStack();
		ICalcInputList calcList = calcContext.getInputList();
		
		String op1 = stack.pop();
		if (CalcUtil.isBracedExpression(op1)) {
			calcList.pushElement(" " + CalcUtil.getUnbracedExpression(op1) + " ");
		} else
			stack.push(op1);
	}

	public char getOperatorSign() {
		return '@';
	}

}
