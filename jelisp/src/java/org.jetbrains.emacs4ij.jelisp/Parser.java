package org.jetbrains.emacs4ij.jelisp;

import java.io.IOException;
import java.io.Reader;

/**
 * Created by IntelliJ IDEA.
 * User: ekaterina.polishchuk
 * Date: 7/8/11
 * Time: 12:59 PM
 *
 * this is a parser for lisp program
 */
public class Parser {

    private String readLispObject (Reader lispCode) {
        String
        while (true) {

        }
        try {
            int symbol = lispCode.read();
            if (symbol == -1)
                return null;

            char c = (char)
            if (c == -1) {
                return  -1;
            }
        } catch (IOException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
    }

    public static LispProgram parse(Reader lispCode) {



        return null;

    }
}
