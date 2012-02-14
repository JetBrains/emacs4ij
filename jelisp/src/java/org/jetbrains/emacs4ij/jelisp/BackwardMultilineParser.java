package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.LObject;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;

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
public class BackwardMultilineParser implements Observer {
    private BackwardParser myBackwardParser = new BackwardParser();
    private List<String> myReader;
    private int myIndex;

    public BackwardMultilineParser (String... code) {
        myReader = Arrays.asList(code);
        myBackwardParser.addObserver(this);
    }

    public LObject parse (int line, int column) {
        try {
            myIndex = line;
            LObject result = null;
            while (result == null) {
                result = myBackwardParser.parseLine(myReader.get(line), column);
            }
           return result;
        } catch (LispException e) {
            System.out.println(e.getMessage());
            return null;
        } catch (ArrayIndexOutOfBoundsException e) {
            throw new LispException("preceding-sexp: End of file during parsing");
        }
    }

    public void update(Observable o, Object arg) {
        //myIndex++;
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
                throw new LispException("preceding-sexp: End of file during parsing");
        myBackwardParser.append(nextLine);
    }
}