package proglang.java.calculator;

import proglang.java.calculator.exception.CalcException;

/**
 * Some util functions needed by the Calculator-classes
 * 
 * @package   proglang.java.calculator
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 */
public class CalcUtil {

	// TODO ongoing to util-functions
	/**
	 * Checks if the given String is a long-value.
	 * @param toCheck
	 * @return true if long
	 */
	public static boolean isLong(String toCheck) {
		try{ 
			Long.parseLong(toCheck);
		} catch(Exception e){
			return false;
		}	
		return true;	
	}
	
	/**
	 * Checks if the given String is a ASCII in printable range, 
	 * thaqt is, in range [32..126] decimal.
	 * @param toCheck
	 * @return true if long
	 */
	public static boolean isAscii(String toCheck) {
		// TODO not sure if this is correct...
		try{ 
			int i = Integer.parseInt(toCheck);
			if (i > 31 && i < 127)
				return true;
		} catch(Exception e){
			return false;
		}	
		return false;
	}
	
	/**
	 * Returns a given String as a long-value.
	 * @param str
	 * @return long value of string
	 */
	public static long getLong(String str) {
		return Long.parseLong(str);
	}
	
	/**
	 * Returns the actual boolean value to a String representing it<ul>
	 * <li>true for "0"</li>
	 * <li>false for "1"</li></ul>
	 * @param str
	 * @return long value of string
	 */
	public static boolean getBoolean(String str) throws CalcException {
		long l = getLong(str);
		if (l == 0)
			return true;
		if (l == 1)
			return false;
		
		throw (new CalcException(str + " is not a boolean value"));
	}
	
	/**
	 * Returns a String representing a boolean value<ul>
	 * <li>"0" is for true</li>
	 * <li>"1" is for false</li></ul>
	 * @param str
	 * @return long value of string
	 */
	public static String getBooleanString(boolean bVal) {
		if (bVal) return "0";
		else return "1";
	}

	/**
	 * Checks if the given String is an expression starting with '(' and ending with ')'
	 * @param toCheck
	 * @return true if braced expression
	 */
	public static boolean isBracedExpression(String toCheck) {
		return toCheck != null && toCheck.startsWith("(") && toCheck.endsWith(")"); 
	}
	
	/**
	 * Returns the content of a braced expression or the expression itself if not braced
	 * @param ex
	 * @return content of a braced expression
	 */
	public static String getUnbracedExpression(String ex) {
		if (isBracedExpression(ex)) {
//				System.out.println("String mit Klammern: " + ex);
			String toReturn = ex.substring(1, ex.length()-1);
//				System.out.println("String ohne Klammern: " + toReturn);
			return toReturn;
		} else {
			return (ex);
		}
	}
}
