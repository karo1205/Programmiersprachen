package proglang.java.calculator.exception;

/**
 * CalcCharOutOfRangeException
 *
 * @package   proglang.java.calculator.exception
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 */
@SuppressWarnings("serial")
public class CalcCharOutOfRangeException extends CalcException {
	public CalcCharOutOfRangeException()
	{
		super();
	}

	public CalcCharOutOfRangeException(String message)
	{
		super(message);
	}

}
