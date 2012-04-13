package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSyntaxTable;
import org.junit.Assert;
import org.junit.Test;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/12/12
 * Time: 3:06 PM
 * To change this template use File | Settings | File Templates.
 */
public class SyntaxTableTest extends BaseSubroutineTest {
    @Test
    public void testElispModeSyntaxTable () {
        Assert.assertNotNull(evaluateString("emacs-lisp-mode-syntax-table"));
    }

    @Test
    public void testStandardSyntaxTable () {
        LispSyntaxTable standard = SyntaxTable.getStandardSyntaxTable();
        for (char i = 0; i < 128; i++) {
            Assert.assertNotSame(LispSymbol.ourNil, standard.getCharSyntax(i));
        }
    }

    @Test
    public void testStandardSyntaxTableLength () {
        LispSyntaxTable standard = SyntaxTable.getStandardSyntaxTable();
        Assert.assertEquals(LispInteger.MAX_CHAR, standard.length());
    }

    @Test
    public void testStringSyntax() {
        Assert.assertEquals(LispList.list(new LispInteger(0)), evaluateString("(string-to-syntax \"-\")"));
    }

    @Test
    public void testStringSyntax0() {
        Assert.assertEquals(LispList.list(new LispInteger(0)), evaluateString("(string-to-syntax \" \")"));
    }

    @Test
    public void testStringSyntax1() {
        Assert.assertEquals(LispList.list(new LispInteger(1)), evaluateString("(string-to-syntax \". \")"));
    }

    @Test
    public void testStringSyntax2() {
        Assert.assertEquals(LispList.list(new LispInteger(65537)), evaluateString("(string-to-syntax \". 1\")"));
    }

    @Test
    public void testStringSyntax3() {
        Assert.assertEquals(LispList.list(new LispInteger(4784129)), evaluateString("(string-to-syntax \". 14n\")"));
    }

    @Test
    public void testStringSyntaxList() {
        Assert.assertEquals(LispList.cons(new LispInteger(5), new LispInteger(36)), evaluateString("(string-to-syntax \")$\")"));
    }

    @Test
    public void testLongStringSyntaxList() {
        Assert.assertEquals(LispList.cons(new LispInteger(524288), new LispInteger(41)), evaluateString("(string-to-syntax \" )$45\")"));
    }

    @Test
    public void testLongStringSyntaxList2() {
        Assert.assertEquals(LispList.cons(new LispInteger(524288), new LispInteger(41)), evaluateString("(string-to-syntax \" )$45. \")"));
    }
}
