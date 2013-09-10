package proglang.java.calculator;
import proglang.java.calculator.exception.CalcParseException;

/**
 * ICalcParser
 * Interface for the Parser for the input list of the Calculator<br>
 * The parser just operates on strings. It will parse the given input string, return the next token of it and 
 *  
 * @package   proglang.java.calculator
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 */
public interface ICalcParser {
	/**
	 * Parses the given input-String<br>
	 * It will reduce the input list by the token returned.
	 * @param input
	 * @return the next token which has been identified
	 * @throws CalcParseException
	 */
	String parse (String input) throws CalcParseException;

}
