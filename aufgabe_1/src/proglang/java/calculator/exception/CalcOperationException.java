package proglang.java.calculator.exception;

/**
 * An operator failed. e.g. an long has been expected but a string returned...
 * 
 * @package   proglang.java.calculator.exception
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 */
@SuppressWarnings("serial")
public class CalcOperationException extends CalcException {
	public CalcOperationException()
	{
		super();
	}

	public CalcOperationException(String message)
	{
		super(message);
	}


}
