package com.jetbrains.emacs4ij.jlisp;

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
        if (args.length == 0) {
            //no code
            return;
        }
        Reader lispCode = null;
        if (args.length == 1) { //it may be lisp code or file name
            File f = new File(args[0]);
            try {
                lispCode = new BufferedReader(new FileReader(f));
            } catch (FileNotFoundException e) {
                //it must be lisp code
                lispCode = new StringReader(args[0]);
            }
        } else {
            StringBuilder stringBuilder = new StringBuilder();
            for (String arg: args) {
                stringBuilder.append(arg).append(" ");
            }
            lispCode = new StringReader(stringBuilder.toString());
        }
        if (lispCode == null) {
            //code read error
            return;
        }
        LispProgram lispProgram = Parser.parse(lispCode);
        if (lispProgram == null) {
            //there were parse errors
            return;
        }
        Evaluator.evaluate(lispProgram);
    }
}
