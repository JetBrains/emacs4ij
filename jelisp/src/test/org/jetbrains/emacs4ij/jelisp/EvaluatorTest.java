package org.jetbrains.emacs4ij.jelisp;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispNumber;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/13/11
 * Time: 2:23 PM
 * To change this template use File | Settings | File Templates.
 */
public class EvaluatorTest {

    Environment environment = new Environment(null);

    private LispObject evaluateString (String lispCode) throws LispException {
        Parser p = new Parser();
        return Evaluator.evaluate(p.parseLine(lispCode), environment);
    }

    @Test
    public void testEvaluateInteger () {
        LispObject lispObject = Evaluator.evaluate(new LispInteger(5), new Environment(null));
        Assert.assertEquals(new LispInteger(5), lispObject);
    }

    @Test
    public void testEvaluateString () {
        LispObject lispObject = Evaluator.evaluate(new LispString("test"), new Environment(null));
        Assert.assertEquals(new LispString("test"), lispObject);
    }

    @Test
    public void testPlus() throws LispException {
        LispObject lispObject = evaluateString("(+ 2 2)");
        Assert.assertEquals(new LispInteger(4), lispObject);
    }

    @Test
    public void testInnerLists() throws LispException {
        LispObject lispObject = evaluateString("(+ 2 (+ 2 3))");
        Assert.assertEquals(new LispInteger(7), lispObject);
    }

    @Test
    public void testQuote () {
        LispObject lispObject = evaluateString("'5");
        Assert.assertEquals(new LispInteger(5), lispObject);
    }

    @Test
    public void testSetVar() throws LispException {
        LispObject value = evaluateString("(set 'var 5)");
        Assert.assertEquals("set return value assertion", new LispInteger(5), value);

        LispObject lispObject = evaluateString("var");
        Assert.assertEquals(new LispInteger(5), lispObject);
    }
}
