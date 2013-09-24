package proglang.java.calculator;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;

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
	// display size
	private final int calcRows = 4;
	private final int calcCols = 64;
	
	private ICalcContext context = null;
	private CalculatorGUI cGui;
	private final String defaultMessage = "Enter formula and press Enter";
	private String inputBuffer;
	enum ModeType { NORMAL, CONT };
	ModeType mode;
	private int openCount;

	/**
	 * Calculator main method.
	 *
	 * @param args
	 * @return
	 */
	public static void main(String[] args)
	{
		if (args.length > 0) {
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
		System.out.println("Usage: java -jar calculator.jar");
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
		context = new CalcContext(new CalcStack(), new CalcInputList(), new CalcDisplay(cGui, calcRows, calcCols));
		cGui.setVisible(true);
		clearAll();
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
		this.mode = mode.NORMAL;
		resetInputBuffer();
	}
	
	public void actionPerformed(ActionEvent e) {
        if ("clear".equals(e.getActionCommand())) {
        	clearAll();
        	cGui.setFocus();
        }
    }

	private void resetInputBuffer() {
		inputBuffer = "";
		openCount = 0;
	}

	public void keyPressed(KeyEvent ev, String completeInput) {
		char key = ev.getKeyChar();
		System.out.println("key pressed: '"+key+"'");
		if(Character.isISOControl(key)) {
			System.out.println("Meta-Key ignored");
		} else {
			if(mode == mode.NORMAL ) {
				if(Character.isDigit(key)) {
					inputBuffer+=key;
				} else if(key=='(') {
					if(inputBuffer.startsWith("(")) {
						inputBuffer+="(";
					} else {
						run(inputBuffer);
						inputBuffer = "(";
					}
					openCount++;
				} else if(key==')') {
					switch(openCount) {
						case 0:
							run(inputBuffer);
							run(")");
							resetInputBuffer();
							break;
						case 1:
							run(inputBuffer+")");
							resetInputBuffer();
							break;
						default:
							inputBuffer+=")";
								openCount--;
					}
				} else if (openCount > 0) {
					inputBuffer += key;
				} else if(key=='"') {
					run(inputBuffer);
					resetInputBuffer();
					mode = mode.CONT;
					cGui.switchMode();
				} else {
					run(inputBuffer+key);
					resetInputBuffer();
				}
			} else {
				if(key=='\'') {
					completeInput = completeInput.replace("\"", "");	// remove the "
					run("("+completeInput+")");
					mode = mode.NORMAL;
					cGui.switchMode();
				}
			}
		}
		if(ev.getKeyCode()==KeyEvent.VK_ENTER) {
			run("("+completeInput+")");
		}
		System.out.println("inputbuffer: '"+inputBuffer+"'");
	}

	public ModeType getMode() {
		return mode;
	}
}
