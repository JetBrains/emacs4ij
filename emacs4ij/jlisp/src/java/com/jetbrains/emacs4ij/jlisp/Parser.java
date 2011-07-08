package com.jetbrains.emacs4ij.jlisp;

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


    private char readChar (Reader lispCode) {
        try {

            char c = (char) lispCode.read();
        } catch (IOException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
    }

    public static LispProgram parse(Reader lispCode) {



        return null;

    }
}
