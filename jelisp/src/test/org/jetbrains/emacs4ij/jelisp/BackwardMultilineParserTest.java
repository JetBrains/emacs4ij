package org.jetbrains.emacs4ij.jelisp;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.elisp.LObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.junit.Test;

import java.io.IOException;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 14/02/12
 * Time: 00:17
 * To change this template use File | Settings | File Templates.
 */
public class BackwardMultilineParserTest {
    @Test
    public void testParseWrongString()  {
        String[] s = new String[] {"\"one", "two\""};
        LObject r  = new BackwardMultilineParser(s).parse(1, 1);
        Assert.assertEquals(new LispSymbol("tw"), r);
    }

    @Test
    public void testParseString() {
        String[] s = new String[] {"\"one", "\"two\""};
        LObject lispObject = new BackwardMultilineParser(s).parse(1, 4);
        Assert.assertEquals(new LispString("two"), lispObject);
    }

    @Test
    public void testParseList() throws IOException {
        String[] s = new String[] {"(defun test (", ") (message", "\"test\")", ")"};
        LObject lispObject = new BackwardMultilineParser(s).parse(3, 0);
        Assert.assertEquals(LispList.list(new LispSymbol("defun"), new LispSymbol("test"), LispList.list(), LispList.list(new LispSymbol("message"), new LispString("test"))), lispObject);
    }

    @Test
    public void testParseQuotedList() throws IOException {
        String[] s = new String[] {"'(defun test (", ") (message", "\"test\")", ")"};
        LObject lispObject = new BackwardMultilineParser(s).parse(3, 0);
        LispList expected = LispList.list(new LispSymbol("quote"),
                LispList.list(new LispSymbol("defun"), new LispSymbol("test"), LispList.list(), LispList.list(new LispSymbol("message"), new LispString("test"))));
        Assert.assertEquals(expected, lispObject);
    }
}
