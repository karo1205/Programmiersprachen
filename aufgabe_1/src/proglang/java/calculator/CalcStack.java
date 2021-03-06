package proglang.java.calculator;

import java.util.Iterator;
import java.util.Stack;

import proglang.java.calculator.exception.CalcIndexOutOfRangeException;
import proglang.java.calculator.exception.CalcStackEmptyException;

public class CalcStack implements ICalcStack {

	Stack<String> stack = new Stack<String>();
	
	@Override
	public void push(String element) {
		stack.push(element);
	}

	@Override
	public String pop() throws CalcStackEmptyException {
		try {
			return stack.pop();
		} catch (Exception e) {
			throw new CalcStackEmptyException("Stack is empty but operator would expect more parameters.");
		}
	}

	@Override
	public String read(Long position) throws CalcIndexOutOfRangeException {
		try {
			return stack.elementAt(getStackIndex(position));
		} catch (Exception e) {
			throw new CalcIndexOutOfRangeException("Random access in stack is out of range: Not possible to access the element at position " + position);
		}
	}

	@Override
	public void delete(Long position) throws CalcIndexOutOfRangeException {
		try {
			stack.remove(getStackIndex(position));
		} catch (Exception e) {
			throw new CalcIndexOutOfRangeException("Random access in stack is out of range: Not possible to delete the element at position " + position);
		}
	}

	@Override
	public void clear() {
		stack.clear();
	}
	
	/**
	 * The built in functions for the Java-stack take the index from 0 (oldest entry) to n-1 (top of stack)
	 * This function recomputes the index from 1 (top of stack) to n (oldest entry) 
	 * @param index
	 * @return index as the built in Stack sees it
	 */
	private int getStackIndex(Long index) {
		int count = stack.size();
		int calcStackIndex = index.intValue();
		
		return Math.abs(calcStackIndex - count);
	}
	
	public String toString() {
		StringBuilder sb = new StringBuilder();
		
		Iterator<String> iter = stack.iterator();

		while (iter.hasNext()){
		    sb.append(iter.next());
		    sb.append(" ");	// for readability...
		}
		return sb.toString();
	}

}
