package proglang.java.calculator.exception;

/**
 * Some parsing went wrong
 * 
 * @package   proglang.java.calculator.exception
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 */
@SuppressWarnings("serial")
public class CalcParseException extends CalcException {
	public CalcParseException()
	{
		super();
	}

	public CalcParseException(String message)
	{
		super(message);
	}

}
