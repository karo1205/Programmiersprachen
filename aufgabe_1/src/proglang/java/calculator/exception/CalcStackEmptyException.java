package proglang.java.calculator.exception;

/**
 * CalcStackEmptyException
 * Stack is empty but some pop or the like is been done
 * 
 * @package   proglang.java.calculator.exception
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 */
@SuppressWarnings("serial")
public class CalcStackEmptyException extends CalcException {
	public CalcStackEmptyException()
	{
		super();
	}

	public CalcStackEmptyException(String message)
	{
		super(message);
	}


}
