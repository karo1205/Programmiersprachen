package proglang.java.calculator.exception;

/**
 * CalcInputListEmptyException
 * The input list is empty but some input is expected.
 * 
 * @package   proglang.java.calculator.exception
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 */
@SuppressWarnings("serial")
public class CalcInputListEmptyException extends CalcException {
	public CalcInputListEmptyException()
	{
		super();
	}

	public CalcInputListEmptyException(String message)
	{
		super(message);
	}

}
