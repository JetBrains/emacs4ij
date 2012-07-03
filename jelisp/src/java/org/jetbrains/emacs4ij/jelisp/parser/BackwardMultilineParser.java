package org.jetbrains.emacs4ij.jelisp.parser;

import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.parser.exception.EndOfFileException;
import org.jetbrains.emacs4ij.jelisp.parser.exception.ParserException;

import java.util.Arrays;
import java.util.List;
import java.util.Observable;
import java.util.Observer;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 14/02/12
 * Time: 00:08
 * To change this template use File | Settings | File Templates.
 */
public final class BackwardMultilineParser implements Observer {
    private BackwardParser myBackwardParser = new BackwardParser();
    private List<String> myReader;
    private int myIndex;

    public BackwardMultilineParser (String... code) {
        myReader = Arrays.asList(code);
        myBackwardParser.addObserver(this);
    }

    public LispObject parse (int line, int column) {
        try {
            myIndex = line;
            LispObject result = null;
            while (result == null) {
                result = myBackwardParser.parseLine(myReader.get(line), column);
            }
           return result;
        } catch (ParserException e) {
            System.out.println(e.getMessage());
            return null;
        } catch (ArrayIndexOutOfBoundsException e) {
            throw new EndOfFileException();
        }
    }

    public void update(Observable o, Object arg) {
        String nextLine;
        try {
            nextLine = myReader.get(--myIndex);
        } catch (IndexOutOfBoundsException e) {
            nextLine = null;
        }
        if (nextLine == null)
            if (arg instanceof LispException)
                throw (LispException) arg;
            else
                throw new EndOfFileException();
        myBackwardParser.append(nextLine);
    }
}