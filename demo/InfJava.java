
import java.io.*;
import java.util.Scanner;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;

public class InfJava
{

    public static void main(String[] args)
    {
        Scanner scanner = new Scanner(System.in);

        try {
	    while(scanner.hasNextLine()) {
                System.out.print(parseFile(winStreamFromFile(scanner.nextLine()), "java"));
            }
        } catch( IOException e ) {
            System.out.println();
        }
    }

    private static ANTLRInputStream streamFromFile(String filename) throws IOException
    {
        return new ANTLRFileStream(filename);
    }

    private static ANTLRInputStream winStreamFromFile(String filename) throws IOException
    {
	return new WindowsFileInputStream(filename);
    }
    
    private static String parseFile(ANTLRInputStream instream, String name) throws IOException
    {
        StringBuilder out;
        switch(name.toLowerCase()) {
        case "java":
	    JavaLexer javalexer = new JavaLexer( instream );
            JavaParser javaparser = new JavaParser( new CommonTokenStream(javalexer) );
            ParserRuleContext tree = javaparser.compilationUnit();
	    ParseTreeWalker walker = new ParseTreeWalker();

	    out = new StringBuilder();
            out.append("[");

	    JavaBaseListener listener = (new JavaBaseListener()
                {
                    boolean first = true;
                    public void enterEveryRule(ParserRuleContext ctx) {
	    		if( ctx.getChildCount() == 1 &&
	    		    !(ctx.getChild(0) instanceof TerminalNodeImpl) ) {
	    		    ParserRuleContext child = (ParserRuleContext) ctx.getChild(0);
	    		    if( ctx.getStart().getStartIndex() == child.getStart().getStartIndex() &&
	    			ctx.getStop().getStopIndex() == child.getStop().getStopIndex() )
	    			return;
	    		}
			
                        if( first )
                            first = false;
                        else
                            out.append(",");

			// add 1 to start, emacs starts at 1
			int start = ctx.getStart().getStartIndex() + 1;
			// add an extra to stop, emacs wants exclusive
			int stop = ctx.getStop().getStopIndex() + 2;
			
			out.append("[\"" +
                                   JavaParser.ruleNames[ctx.getRuleIndex()] + "\"," +
                                   start + "," +
                                   stop + "]");
			
                    }
                });

	    walker.walk(listener, tree);
	    
            out.append("]\n");
            return out.toString();
        default:
            // make recoverable
            throw new RuntimeException("Unknown grammar " + name);
        }
    }
    
}
