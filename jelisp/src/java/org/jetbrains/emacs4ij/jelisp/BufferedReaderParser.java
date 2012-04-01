package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.exception.EndOfLineException;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.ReadException;

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
public class BufferedReaderParser implements Observer {
    private ForwardParser myForwardParser = new ForwardParser();
    private BufferedReader myReader;
    private int myLine = 0;

    public BufferedReaderParser (BufferedReader reader) {
        myReader = reader;
        myForwardParser.addObserver(this);
    }

    public LispObject parse (String firstLine) {
        try {
            myLine = 0;
            return myForwardParser.parseLine(firstLine);
        } catch (LispException e) {
            System.out.println("line " + myLine + ": " + e.getMessage());
            return null;
        }
    }

    public void update(Observable o, Object arg) {
        try {
            String nextLine = myReader.readLine();
            myLine++;
            if (nextLine == null)
                if (arg instanceof LispException)
                    throw (LispException) arg;
                else throw new EndOfLineException();
            myForwardParser.append(nextLine);
        } catch (IOException e) {
            if (arg instanceof LispException) {
                throw (LispException) arg;
            }
            throw new ReadException(myReader.toString());
        }
    }
    
    public int getLine() {
        return myLine;
    }
}
