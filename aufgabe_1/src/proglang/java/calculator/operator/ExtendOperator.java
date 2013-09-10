package proglang.java.calculator.operator;

import proglang.java.calculator.CalcUtil;
import proglang.java.calculator.ICalcContext;
import proglang.java.calculator.ICalcStack;
import proglang.java.calculator.exception.CalcException;
import proglang.java.calculator.exception.CalcOperationException;

/**
 * Takes the two topmost elements of the stack. One must be a non negative number n, one must be a braced expression.<br>
 * Result is, that the braced expression is extended by the character which is represented by the ASCII-value of n.
 * The expression is again surrounded by braces and pushed onto the stack.
 * 
 * @package   proglang.java.calculator.operator
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 *
 */
public class ExtendOperator implements ICalcOperator {

	private String asciiValue = null;
	private String bracedExp = null;
	@Override
	public void operate(ICalcContext calcContext) throws CalcException {
		ICalcStack stack = calcContext.getStack();
		
		String op1 = stack.pop();
		String op2 = stack.pop();
		
		if (CalcUtil.isAscii(op1)) {
			bracedExp = op2;
			asciiValue = op1;
		} else {
			if (CalcUtil.isAscii(op2)) {
				asciiValue = op2;
				bracedExp = op1;
			}
			else {
				if (!(CalcUtil.isBracedExpression(op2)))
					throw (new CalcOperationException("One expression must be a braced expression, the other one ASCII in printable range (32-126)"));			
			}
		}
		if (!CalcUtil.isBracedExpression(bracedExp))
			throw (new CalcOperationException("One expression must be a braced expression, the other one ASCII in printable range (32-126)"));
		
		bracedExp = CalcUtil.getUnbracedExpression(bracedExp);
		StringBuilder sb = new StringBuilder(bracedExp);
		char character = (char)Integer.parseInt(asciiValue);
		sb.append(character);
		
		stack.push("(" + sb.toString() + ")");
	}
	
	public char getOperatorSign() {
		return ';';
	}

}
