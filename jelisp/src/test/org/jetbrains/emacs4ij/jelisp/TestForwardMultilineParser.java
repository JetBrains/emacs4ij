package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.parser.BaseForwardMultilineParser;
import org.jetbrains.emacs4ij.jelisp.parser.exception.ParserException;

import java.io.BufferedReader;
import java.io.IOException;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 5/23/12
 * Time: 12:39 PM
 * To change this template use File | Settings | File Templates.
 */
public class TestForwardMultilineParser extends BaseForwardMultilineParser {
    private BufferedReader myReader;

    public TestForwardMultilineParser (BufferedReader reader, String filename) {
        myReader = reader;
        myFilename = filename;
        myForwardParser.addObserver(this);
    }

    public LispObject parse (String firstLine) throws ParserException {
        try {
            return myForwardParser.parseLine(firstLine, 0);
        } catch (ParserException e) {
            return processException(e);
        }
    }

    @Override
    protected void gotoOffset(long offset) throws IOException {
        return;
    }

    @Override
    protected String readLine() throws IOException {
        return myReader.readLine();
    }

    @Override
    protected String readerInfo() {
        return myReader.toString();
    }
}
