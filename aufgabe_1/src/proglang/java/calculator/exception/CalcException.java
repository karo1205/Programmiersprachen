package proglang.java.calculator.exception;

/**
 * CalcException
 * Base Class for all other exceptions of the Calculator
 * 
 * @package   proglang.java.calculator.exception
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 */
@SuppressWarnings("serial")
public class CalcException extends Exception {
	
	public CalcException()
	{
		super();
	}

	public CalcException(String message)
	{
		super(message);
	}

}
