package proglang.java.calculator;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * Main Class
 *
 * @package   proglang.java.calculator
 * @author    Robert Kapeller <rkapeller@gmail.com>
 * @copyright 2013 Robert Kapeller
 */
@SuppressWarnings("all")
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
	}
	
	/**
	 * Initializes all data used by the calculator 
	 */
	private void initCalculator() {
		
		cGui = new CalculatorGUI(this);
		context = new CalcContext(new CalcStack(), new CalcInputList(), new CalcDisplay(cGui));
		cGui.setVisible(true);
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
			e.printStackTrace();
			cGui.setCommentLineText(e.getMessage());

		} finally {
			System.out.println("-----------------");
		}
	}
	
	/**
	 * Reads each line from the input field and runs it
	 */
	private void runInputField() {
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
