package proglang.java.calculator.operator;

import proglang.java.calculator.CalcUtil;
import proglang.java.calculator.ICalcContext;
import proglang.java.calculator.ICalcStack;
import proglang.java.calculator.exception.CalcException;
import proglang.java.calculator.exception.CalcOperationException;
/**
 * Takes the two topmost elements of the stack. One must be a non negative number n, one must be a braced expression.<br>
 * Result is the ASCII-code of the n'th element of the braced expression, which is pushed on the stac.k<br>
 * If there are less than n elements in the expression, -1 is put on the stack, the index starts with 0.
 * 
 * @package   proglang.java.calculator.operator
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 *
 */
public class ReadOperator implements ICalcOperator {

	String bracedExp = null; 
	long index = 0;

	@Override
	public void operate(ICalcContext calcContext) throws CalcException {
		ICalcStack stack = calcContext.getStack();

		setElements(calcContext);
		// no exception so far? Lets really operate
		
		if (bracedExp.length() < (index + 1)) {
			// out of range, just push -1
			stack.push("-1");
		} else {
			char c = bracedExp.charAt((int)index);
			stack.push(Integer.toString((int)c));
		}
	}
	
	/**
	 * Sets the elements used for this operation (index and braced expression)
	 * @param calcContext
	 * @throws CalcException
	 */
	void setElements(ICalcContext calcContext) throws CalcException {
		ICalcStack stack = calcContext.getStack();
		
		String op1 = stack.pop();
		String op2 = stack.pop();
		
		if (CalcUtil.isLong(op1)) 
		{
			index = CalcUtil.getLong(op1);
			bracedExp = op2;
		} else {
			if (CalcUtil.isLong(op2)) {
				index = CalcUtil.getLong(op2);
				bracedExp = op1;
			}
			else {
				if (!(CalcUtil.isBracedExpression(op2)))
					throw (new CalcOperationException("One expression must be a long value"));			
			}
		}
		
		if (index < 0)
			throw (new CalcOperationException("Index must be a positive long value"));
		if (!CalcUtil.isBracedExpression(bracedExp))
			throw (new CalcOperationException("One expression must be a braced expression"));

	}
	
	public char getOperatorSign() {
		return '?';
	}


}
