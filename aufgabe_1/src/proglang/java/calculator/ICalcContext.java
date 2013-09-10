package proglang.java.calculator;

import proglang.java.calculator.exception.CalcException;

/**
 * CalcException<br>
 * Base Class for all other exceptions of the Calculator
 * 
 * @package   proglang.java.calculator
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 */
public interface ICalcContext {
	
	ICalcStack getStack();
	void setStack (ICalcStack stack);
	
	ICalcInputList getInputList();
	void setInputList(ICalcInputList list);
	
	ICalcDisplay getDisplay();
	void setDisplay (ICalcDisplay display);
	public boolean isOperator(String s);
	public void operate(String s) throws CalcException;

}
