package proglang.java.calculator.operator;

import proglang.java.calculator.CalcUtil;
import proglang.java.calculator.ICalcContext;
import proglang.java.calculator.ICalcStack;
import proglang.java.calculator.exception.CalcException;

public class OutputOperator implements ICalcOperator {

	@Override
	public void operate(ICalcContext calcContext) throws CalcException {
		// TODO Auto-generated method stub
		ICalcStack stack = calcContext.getStack();
		
		String op1 = stack.pop();
		String op2 = stack.pop();
		
		if (CalcUtil.isLong(op1) && CalcUtil.isLong(op2)) {
			// TODO Not clear which one is which (ascii or index...)
		}
		
	}
	
	public char getOperatorSign() {
		return '$';
	}

}
