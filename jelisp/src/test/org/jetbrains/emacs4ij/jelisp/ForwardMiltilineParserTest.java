package org.jetbrains.emacs4ij.jelisp;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.junit.Test;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;

/**
 * Created by IntelliJ IDEA.
 * User: Kate
 * Date: 25.07.11
 * Time: 15:14
 * To change this template use File | Settings | File Templates.
 */
public class ForwardMiltilineParserTest {
    @Test
    public void testParseSplitString() throws IOException {
        StringReader r = new StringReader ("\"one\ntwo\"");
        BufferedReader br = new BufferedReader(r);
        TestForwardMultilineParser bufferedReaderParser = new TestForwardMultilineParser(br, "test");
        String s;
        s = br.readLine();
        LispObject lispObject = bufferedReaderParser.parse(s, 0);
        Assert.assertEquals(new LispString("one\ntwo"), lispObject);
    }

    @Test
    public void testParseList() throws IOException {
        BufferedReader br = new BufferedReader(new StringReader("(defun test (\n) \"doc\ndoc\ndoc()\" (message\n\"test\")\n)"));
        TestForwardMultilineParser bufferedReaderParser = new TestForwardMultilineParser(br, "test");
        String s;
        s = br.readLine();
        LispObject lispObject = bufferedReaderParser.parse(s, 0);
        Assert.assertEquals(LispList.list(new LispSymbol("defun"), new LispSymbol("test"), LispList.list(),
                new LispString("doc\ndoc\ndoc()"), LispList.list(new LispSymbol("message"), new LispString("test"))),
                lispObject);
    }

    @Test
    public void testParseStringWithManyDefs() throws IOException {
        StringReader r = new StringReader("(defvar problems)       (defvar \nqlist) a");
        BufferedReader br = new BufferedReader(r);
        TestForwardMultilineParser p = new TestForwardMultilineParser(br, "test");
        String s;
        s = br.readLine();
        LispObject lispObject = p.parse(s, 0);
        Assert.assertEquals(LispList.list(new LispSymbol("defvar"), new LispSymbol("problems")), lispObject);
        lispObject = p.parseNext();
        Assert.assertEquals(LispList.list(new LispSymbol("defvar"), new LispSymbol("qlist")), lispObject);
        lispObject = p.parseNext();
        Assert.assertEquals(new LispSymbol("a"), lispObject);
        Assert.assertTrue(p.isFinished());
    }

    @Test
    public void testParseDotOnNewLine() throws IOException {
        StringReader r = new StringReader("(\n1\n\t.\n 2\n)");
        BufferedReader br = new BufferedReader(r);
        TestForwardMultilineParser p = new TestForwardMultilineParser(br, "test");
        String s;
        s = br.readLine();
        LispObject lispObject = p.parse(s, 0);
        Assert.assertEquals(LispList.cons(new LispInteger(1), new LispInteger(2)), lispObject);
    }
}
