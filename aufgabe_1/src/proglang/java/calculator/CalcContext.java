package proglang.java.calculator;

import java.util.HashMap;
import java.util.Map;

import proglang.java.calculator.exception.CalcException;
import proglang.java.calculator.operator.*;

/**
 * The context stores references to all relevant data, that is, stack, list and display. The context is passed a sa paramter
 * to the operators, which in turn then can access those elements.<br>
 * Additionally the context holds the map of operators and provides functionality to call them.
 * 
 * @package   proglang.java.calculator
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 *
 */

public class CalcContext implements ICalcContext {

	private ICalcStack calcStack = null;
	private ICalcInputList calcInputList = null;
	private ICalcDisplay calcDisplay = null;
	private Map<String, ICalcOperator> operatorMap = new HashMap<String, ICalcOperator>();
	
	public CalcContext(ICalcStack stack, ICalcInputList list, ICalcDisplay display) {
		this.setDisplay(display);
		this.setInputList(list);
		this.setStack(stack);
		
		initOperatorMap();
	}
	
	// nono...
	private CalcContext() {
		
	}
	
	@Override
	public ICalcStack getStack() {
		return calcStack;
	}

	@Override
	public void setStack(ICalcStack stack) {
		calcStack = stack;
	}

	@Override
	public ICalcInputList getInputList() {
		return calcInputList;
	}

	@Override
	public void setInputList(ICalcInputList list) {
		calcInputList = list;
	}

	@Override
	public ICalcDisplay getDisplay() {
		return calcDisplay;
	}

	@Override
	public void setDisplay(ICalcDisplay display) {
		calcDisplay = display;
	}
	
	@Override
	public boolean isOperator(String s) {
		if (operatorMap.containsKey(s))
			return true;
		return false;
	}

	@Override
	public void operate(String s) throws CalcException {
		operatorMap.get(s).operate(this);
	}

	private void initOperatorMap() {
		// This is just hard coded...
		operatorMap.put("&", new AndOperator());			// tested
		operatorMap.put("@", new ApplyOperator());			// tested
		operatorMap.put(">", new BiggerThanOperator());		// tested
		operatorMap.put("!", new CopyOperator());			// tested
		operatorMap.put("#", new DeleteOperator());			// tested
		operatorMap.put("/", new DivOperator());			// tested
		operatorMap.put("=", new EqualsOperator());			// tested
		operatorMap.put(";", new ExtendOperator());			// tested
		operatorMap.put("-", new MinusOperator());			// tested
		operatorMap.put("%", new ModOperator());			// tested
		operatorMap.put("*", new MultiplyOperator());		// tested
		operatorMap.put("~", new NegationOperator());		// tested
		operatorMap.put("|", new OrOperator());				// tested
		operatorMap.put("$", new OutputOperator());			// tested
		operatorMap.put("+", new PlusOperator());			// tested
		operatorMap.put("?", new ReadOperator());			// tested
		operatorMap.put("<", new SmallerThanOperator());	// tested
	}
}
