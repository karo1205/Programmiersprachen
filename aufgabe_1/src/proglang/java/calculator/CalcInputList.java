package proglang.java.calculator;

import proglang.java.calculator.exception.CalcParseException;

/**
 * List is implemented as a String. The next character to be fetched is the leftmost char in the String.  
 * @package   proglang.java.calculator
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller

 */

public class CalcInputList implements ICalcInputList {
	private String list;
	private static int intMinus = (int)'-';
	private static int intPlus = (int)'+';
	
	public CalcInputList () {
		list = new String();
	}
	
	/**
	 * Gets the next element from the input-list.<br>
	 * This is implemented as a very simple tokenizer checking the first read digit and
	 * trying to determine what the next token to return will be:<ul>
	 * <li>number</li>
	 * <li>an expression in form of "(...)" where the two braces are matching</li>
 	 * <li>separated by white space(s)</li>
	 * 
	 */
	@Override
	public String getNextElement() {
		try {
			
			if (list == null || list.length() == 0)
				return "";
			
			char nextChar = ' ';
	
			// read over trailing whitespaces
			while ((isWhiteSpace(nextChar)) && list.length() > 0) {
				nextChar = getNextChar();
			}
			
			if (nextChar == '(')
				return handleOpenBrace();
			if (isDigit(nextChar))
				return handleDigit(nextChar);
			if (((int)nextChar == intPlus) ||  ((int)nextChar == intMinus))
				return handleDigit(nextChar);
			
			return Character.toString(nextChar).trim();
		} catch (CalcParseException e) {
			// hmmm... what to do?
			return ("");
		}
	}

	boolean isWhiteSpace(char toCheck) {
		return (toCheck == ' ' || toCheck == '\t');
	}
	
	@Override
	public void pushElement(String element) {
		list = new StringBuilder(element).append(list).toString();
	}

	@Override
	public void clear() {
		list = new String();
	}
	
	@Override
	public long getListSize() {
		if (list != null)
			return list.length();
		else return 0;
	}


	/**
	 * handles the parsing of a digit. Would throw an exception if the end of list is reached without
	 * properly closed braces.
	 * @return
	 * @throws CalcParseException
	 */
	private String handleDigit(char firstDigit) {

		String toReturn = Character.toString(firstDigit);
		try {
			char nextChar = getNextChar();
			while (isDigit(nextChar)) {
				toReturn = toReturn.concat(Character.toString(nextChar));
				nextChar = getNextChar();
			}
			// the last char was no digit. put it back
			this.pushElement(Character.toString(nextChar));
			return toReturn;
			
		}  catch (CalcParseException e) {
			return toReturn;
		}
	}

	/**
	 * handles the parsing of a brace-expression. Would throw an exception if the end of list is reached without
	 * properly closed braces.
	 * @return
	 * @throws CalcParseException
	 */
	private String handleOpenBrace() throws CalcParseException {

		String toReturn = "(";
		int braceCount = 1;
		while (braceCount > 0) {
			char nextChar = getNextChar();
			toReturn = toReturn.concat(Character.toString(nextChar));

			if (nextChar == '(')
				braceCount ++;
			if (nextChar == ')') 
				braceCount --;
		}
		
		return toReturn;
	}
	
	/**
	 * Returns next character in the list. It also "consumes" the char from the list. Throws an exception if list is null or empty.
	 * 
	 * @return net character in list
	 * @throws CalcParseException
	 */
	private char getNextChar() throws CalcParseException {
		if (list == null || list.length() == 0)
			throw new CalcParseException ("nothing to read anymore...");
		
		char nextChar = list.charAt(0);
		list = list.substring(1);
		return nextChar;
	}
	
	private boolean isDigit(char d) {
		return 	(d <= '9') &&  (d >= '0');
	}

	public String toString() {
		return list;
	}
}
