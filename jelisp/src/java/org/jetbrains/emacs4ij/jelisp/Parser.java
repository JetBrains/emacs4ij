package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.MissingClosingBracketException;
import org.jetbrains.emacs4ij.jelisp.exception.ReadFinishedException;

import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA.
 * User: ekaterina.polishchuk
 * Date: 7/8/11
 * Time: 12:59 PM
 *
 * this is a parser for lisp program
 */

public class Parser {

    private Reader myLispCode;
    private LispObject myProgram;
    private Environment myEnvironment = new Environment();
    private StringBuilder myStackTrace = null;

    private boolean myAsIs;

    public LispObject getMyProgram() {
        return myProgram;
    }

    public Environment getMyEnvironment() {
        return myEnvironment;
    }

    private char read () throws LispException {
        int symbol = -1;
        try {
            symbol = myLispCode.read();
        } catch (IOException e) {
            try {
                myLispCode.close();
            } catch (IOException e1) {
                e1.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
            }
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
            throw new LispException("The input Reader is corrupted. Parsing aborted.");
        }
        if (symbol == -1)
            throw new ReadFinishedException();
        return (char)symbol;
    }

    public ArrayList<LispObject> parseList() throws LispException {
        ArrayList<LispObject> list = null;
        String element = "";
        while (true) {
            try {
                char c = read();
                if (c == ')') {
                    //TODO: check for empty list
                    return list;
                }
                if ((c == ' ') || (c == '\n')) {
                    if (element != "") {
                        //TODO: recognize the element
                    }
                    continue;
                }
                element += c;
            } catch (ReadFinishedException e) {
                throw new MissingClosingBracketException();
            }
        }
    }

    public StringBuilder parse(Reader lispCode) {
        myLispCode = lispCode;

        // parse :)

        return myStackTrace;
    }
}
