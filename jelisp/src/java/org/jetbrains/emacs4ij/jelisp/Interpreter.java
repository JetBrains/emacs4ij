package org.jetbrains.emacs4ij.jelisp;

import com.sun.org.apache.xml.internal.resolver.helpers.PublicId;

import java.io.*;

/**
 * Created by IntelliJ IDEA.
 * User: ekaterina.polishchuk
 * Date: 7/7/11
 * Time: 5:24 PM
 * To change this template use File | Settings | File Templates.
 */
public class Interpreter {

    public static void main (String[] args) {
        System.out.println(args.length);

        if (args.length == 0) {
            //no code
            return;
        }
        Reader lispCode;
        if (args.length == 1) { //it may be lisp code or file name
            File f = new File(args[0]);
            try {
                lispCode = new BufferedReader(new FileReader(f));
            } catch (FileNotFoundException e) {
                //it must be lisp code
                lispCode = new StringReader(args[0]);
            }
        } else { // it is a lisp code. Concatenate string arguments into string
            StringBuilder stringBuilder = new StringBuilder();
            for (String arg: args) {
                stringBuilder.append(arg).append(" ");
            }
            lispCode = new StringReader(stringBuilder.toString());
        }

        /*Parser parser = new Parser();
        /*StringBuilder stackTrace = parser.parse(lispCode);
        if (stackTrace == null) {
            //there were parse errors
            //TODO: output stackTrace
            return;
        }
        Evaluator.evaluate(parser.getMyProgram(), parser.getMyEnvironment());  */
    }
}
