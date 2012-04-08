package org.jetbrains.emacs4ij.jelisp.parser;

import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.exception.ReadException;
import org.jetbrains.emacs4ij.jelisp.parser.exception.EndOfLineException;
import org.jetbrains.emacs4ij.jelisp.parser.exception.ParserException;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.Observable;
import java.util.Observer;

/**
 * Created by IntelliJ IDEA.
 * User: Kate
 * Date: 22.07.11
 * Time: 20:30
 * To change this template use File | Settings | File Templates.
 */
public class ForwardMultilineParser implements Observer {
    private ForwardParser myForwardParser = new ForwardParser();
    private BufferedReader myReader;
    private final String myFilename;
    private int myLine = 0;

    public ForwardMultilineParser(BufferedReader reader, String filename) {
        myReader = reader;
        myFilename = filename;
        myForwardParser.addObserver(this);
    }

    public LispObject parse (String firstLine, int index) throws ParserException {
        try {
            myLine = index;
            return myForwardParser.parseLine(firstLine);
        } catch (ParserException e) {
            throw new ParserException(myFilename, myLine, e.getMessage(), myForwardParser.getCode());
        }
    }

    public void update(Observable o, Object arg) {
        try {
            String nextLine = myReader.readLine();
            if (nextLine == null)
                if (arg instanceof ParserException)
                    throw (ParserException) arg;
                else throw new EndOfLineException();
            myLine++;
            myForwardParser.append(nextLine);
        } catch (IOException e) {
            if (arg instanceof ParserException) {
                throw (ParserException) arg;
            }
            throw new ReadException(myReader.toString());
        }
    }
    
    public int getLine() {
        return myLine;
    }
}
