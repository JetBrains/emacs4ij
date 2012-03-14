package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.exception.EndOfLineException;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;

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
    int lines = 0;

    public BufferedReaderParser (BufferedReader reader) {
        myReader = reader;
        myForwardParser.addObserver(this);
    }

    public LispObject parse (String firstLine) {
        try {
            lines = 0;
//            if (firstLine.contains("defmacro when "))
//                System.out.print(1);
            return myForwardParser.parseLine(firstLine);
        } catch (LispException e) {
            System.out.println("line " + lines + ": " + e.getMessage());
            return null;
        }
    }

    public void update(Observable o, Object arg) {
        try {
            String nextLine = myReader.readLine();
            lines++;
            if (nextLine == null)
                if (arg instanceof LispException)
                    throw (LispException) arg;
                else
                    throw new EndOfLineException();

            myForwardParser.append(nextLine);

        } catch (IOException e) {
            if (arg instanceof LispException) {
                throw (LispException) arg;
            }
            throw new RuntimeException("Error while reading " + myReader.toString());
        }
    }
    
    public int getLines() {
        return lines;
    }
}
