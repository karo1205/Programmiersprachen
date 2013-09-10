package proglang.java.calculator.exception;

/**
 * CalcIndexOutOfRangeException
 * Some index is out of range. message shall give details.
 * 
 * @package   proglang.java.calculator.exception
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 */
@SuppressWarnings("serial")
public class CalcIndexOutOfRangeException extends CalcException {
	public CalcIndexOutOfRangeException()
	{
		super();
	}

	public CalcIndexOutOfRangeException(String message)
	{
		super(message);
	}

}
