package proglang.java.calculator;
import proglang.java.calculator.*;

import javax.swing.*;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.IOException;
import java.io.StringReader;
import java.net.InetSocketAddress;
import java.net.UnknownHostException;

/**
 * Main Class
 *
 * @package   proglang.java.calculator
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 */
class Calculator implements ActionListener
{
	private ICalcContext context = null;
	private CalculatorGUI cGui;
	private final String defaultMessage = "Enter formula and press Enter";
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
		
		cGui = new CalculatorGUI(this);
		context = new CalcContext(new CalcStack(), new CalcInputList(), new CalcDisplay(cGui));
		cGui.setVisible(true);
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

		
//		System.out.println("Auslesen");
//		run ("2?");
//		context.getStack().clear();
//		context.getInputList().clear();
//		System.out.println("-------------------------------------\n\n");

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
		System.out.println("Ausgabe");
		for (int i = 0; i < 4; i++) {
			int pos = i + 64*i;
			String index = Integer.toString(pos);
			String o = new String("65 " + index + "$");
			run (o);
		}
		
		run ("65 0$");
		run ("47 63$");
		run ("76 64$");
		run ("33 125$");
		run ("99 217$");
		run ("100 -1$");
		context.getStack().clear();
		context.getInputList().clear();
		System.out.println("-------------------------------------\n\n");

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
			// show something in GUI?
			e.printStackTrace();
			cGui.setCommentLineText(e.getMessage());

		}
	}
	
	/**
	 * Reads each line from the input field and runs it
	 */
	private void runInputField() {
		// TODO run from input field
		String text = cGui.getFormulaText();
		String[] lines = text.split("\n");
		for (int i = 0; i<lines.length; i++) {
			run(lines[i]);
		}
	}
	
	/**
	 * 
	 * Prints the internals of the calculator, that is, stack and input list to stdout.
	 * The output is in the same form as in the Aufgabe-sheet
	 */
	private void printCalcIntern() {
		System.out.println(context.getStack().toString() + "^" + context.getInputList().toString());
	}
	
	/**
	 * Clears everything: list, stack and display
	 */
	private void clearAll() {
		context.getStack().clear();
		context.getInputList().clear();
		cGui.clearAll();
		cGui.setCommentLineText(defaultMessage);
	}
	
	public void actionPerformed(ActionEvent e) {
        if ("enter".equals(e.getActionCommand())) {
        	runInputField();
        } else {
        	clearAll();
        }
    }
}
