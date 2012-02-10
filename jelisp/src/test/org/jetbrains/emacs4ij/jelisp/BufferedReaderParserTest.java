package org.jetbrains.emacs4ij.jelisp;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.elisp.LObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
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
public class BufferedReaderParserTest {
    @Test
    public void testParseSplitString() throws IOException {
        StringReader r = new StringReader ("\"one\ntwo\"");
        BufferedReader br = new BufferedReader(r);
        BufferedReaderParser bufferedReaderParser = new BufferedReaderParser(br);
        String s;
        try {
            s = br.readLine();
        } catch (IOException e) {
            throw e;
        }
        LObject lispObject = bufferedReaderParser.parse(s);
        Assert.assertEquals(new LispString("one\ntwo"), lispObject);
    }

    @Test
    public void testParseList() throws IOException {
        BufferedReader br = new BufferedReader(new StringReader("(defun test (\n) (message\n\"test\")\n)"));
        BufferedReaderParser bufferedReaderParser = new BufferedReaderParser(br);
        String s;
        try {
            s = br.readLine();
        } catch (IOException e) {
            throw e;
        }
        LObject lispObject = bufferedReaderParser.parse(s);
        Assert.assertEquals(LispList.list(new LispSymbol("defun"), new LispSymbol("test"), LispList.list(), LispList.list(new LispSymbol("message"), new LispString("test"))), lispObject);
    }
}
