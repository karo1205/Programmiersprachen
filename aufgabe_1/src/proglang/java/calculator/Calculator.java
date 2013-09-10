package proglang.java.calculator;
import proglang.java.calculator.*;

import javax.swing.*;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.UnknownHostException;

/**
 * Main Class
 *
 * @package   proglang.java.calculator
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 */
class Calculator
{
	ICalcContext context = null;
	/**
	 * Calculator main method.
	 *
	 * @param args
	 * @return
	 */
	@SuppressWarnings("unused")
	public static void main(String[] args)
	{
		if (args.length > 1) {
			usage();
		}

		Calculator calc = new Calculator(args);
		// TODO start calc and let the input run
		// calc.run();
	}

	/**
	 * Prints usage instructions to a CLI and exits
	 *
	 * @return
	 */
	protected static void usage()
	{
		System.out.println("Usage: java -jar calculator.jar [INPUT_FILE]");
		System.exit(0);
	}
	
	private Calculator () {
		initCalculator();
	}
	
	public Calculator(String[] args)
	{
		this ();
		// if there are args, this is meant to be an input file
		// TODO
	}
	
	/**
	 * Initializes all data used by the calculator 
	 */
	private void initCalculator() {
		
		context = new CalcContext(new CalcStack(), new CalcInputList(), new CalcDisplay());
		
		// TODO remove test for dist
		test();
	}

	/**
	 * Test function.
	 */
	private void test() {
		System.out.println("Demo Aufgabe nach aufgabe 1");
		run ("1(9)(9~)(4!5#2+#@)@");
		System.out.println("-------------------------------------\n\n");
		context.getStack().clear();
		context.getInputList().clear();
		
//		System.out.println("Erweitern");
//		run ("(123ab)65;");
//		context.getStack().clear();
//		context.getInputList().clear();
//		run ("65(123ab);");
//		context.getStack().clear();
//		context.getInputList().clear();
//		run ("(123ab)11;");
//		context.getStack().clear();
//		context.getInputList().clear();
//		run ("122 a;");
//		context.getStack().clear();
//		context.getInputList().clear();

		
		System.out.println("Auslesen");
		run ("2?");
		context.getStack().clear();
		context.getInputList().clear();
		System.out.println("-------------------------------------\n\n");

//		run ("4(12345)?");
//		context.getStack().clear();
//		context.getInputList().clear();
//		System.out.println("-------------------------------------\n\n");
//		
//		run ("(12345)8?");
//		context.getStack().clear();
//		context.getInputList().clear();
//		System.out.println("-------------------------------------\n\n");
//		
//		run ("12345 8?");
//		context.getStack().clear();
//		context.getInputList().clear();
//
		System.out.println("-------------------------------------\n\n");
}
	/**
	 * Main computation loop. Note that this loop does not clear input list or stack, so that
	 * previously done (pre)computations are used as well.
	 * @param toCompute
	 */
	private void run(String toCompute) {
		try {
			context.getInputList().pushElement(toCompute);
			// And then run trough them
			printCalcIntern();
			String s = context.getInputList().getNextElement();
	
			while (s.length() != 0) {
				if (context.isOperator(s)) {
					context.operate(s);
				} else { 
					context.getStack().push(s);
				}
				printCalcIntern();
				s=context.getInputList().getNextElement();
			}
		} catch (Exception e) {
			// TODO exception handling!!!
			//show something...
			e.printStackTrace();
		}
	}
	
	/**
	 * Prints the internals of the calculator, that is, stack and input list to stdout.
	 * The output is in the same form as in the Aufgabe-sheet
	 */
	private void printCalcIntern() {
		System.out.println(context.getStack().toString() + "^" + context.getInputList().toString());
	}
}
