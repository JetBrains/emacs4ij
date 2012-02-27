package org.jetbrains.emacs4ij.jelisp.subroutine;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.ForwardParser;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.TestSetup;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 26/02/12
 * Time: 18:22
 * To change this template use File | Settings | File Templates.
 */
public class BuiltinArithmeticTest {
    private CustomEnvironment environment;

    @BeforeClass
    public static void runBeforeClass() {
        TestSetup.runBeforeClass();
    }

    @Before
    public void setUp() throws Exception {
        GlobalEnvironment.INSTANCE.clearRecorded();
        environment = new CustomEnvironment(GlobalEnvironment.INSTANCE);
    }

    private LObject evaluateString (String lispCode) throws LispException {
        ForwardParser forwardParser = new ForwardParser();
        LObject object = forwardParser.parseLine(lispCode);
        return object.evaluate(environment);
    }

    @Test
    public void testPlusInteger() throws LispException {
        LObject lispObject = evaluateString("(+ 2 2)");
        Assert.assertEquals(new LispInteger(4), lispObject);
    }

    @Test
    public void testPlusFloat () {
        LObject lispObject = evaluateString("(+ 2 2.0)");
        Assert.assertEquals(new LispFloat(4), lispObject);
    }

    @Test
    public void testPlusSimple () {
        LispNumber n = BuiltinArithmetic.plus(new LispInteger(5), new LispFloat(6.6));
        Assert.assertEquals(new LispFloat(11.6), n);
    }

    @Test
    public void testPlusMarker () {
        try {
            BuiltinArithmetic.plus(new LispMarker(), new LispMarker());
        } catch (Exception e) {
            Assert.assertEquals("'(error \"Marker does not point anywhere\")", TestSetup.getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }

    @Test
    public void testMore() {
        LispSymbol more = BuiltinArithmetic.more(new LispInteger(5), new LispFloat(1.3));
        Assert.assertEquals(LispSymbol.ourT, more);
    }

    @Test
    public void testMultiplySimple () {
        LispNumber n = BuiltinArithmetic.multiply(new LispInteger(5), new LispFloat(2.0));
        Assert.assertEquals(new LispFloat(10), n);
    }

    @Test
    public void testPlusEmpty () {
        LObject lispObject = evaluateString("(+)");
        Assert.assertEquals(new LispInteger(0), lispObject);
    }

    @Test
    public void testMultiply() throws Exception {
        LObject LObject = evaluateString("(* 2 2)");
        Assert.assertEquals(new LispInteger(4), LObject);
    }

    @Test
    public void testNumOrMarkersEqual() {
        LObject r = evaluateString("(= 1 2)");
        Assert.assertEquals(LispSymbol.ourNil, r);
        r = evaluateString("(= 1 1.0)");
        Assert.assertEquals(LispSymbol.ourT, r);
    }

    @Test
    public void testNumOrMarkersNilMarker() {
        try {
            evaluateString("(= 1 (make-marker))");
        } catch (Exception e) {
            Assert.assertEquals("'(error \"Marker does not point anywhere\")", TestSetup.getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }

    @Test
    public void testNumOrMarkersWrongArg() {
        try {
            evaluateString("(= 1 'a)");
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument number-or-marker-p a)", TestSetup.getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }

    @Test
    public void testNumOrMarkersNotEqual() {
        LObject r = evaluateString("(/= 1 2)");
        Assert.assertEquals(LispSymbol.ourT, r);
        r = evaluateString("(/= 1 1.0)");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }

    @Test
    public void testNumOrMarkersLessOrEqual() {
        LObject r = evaluateString("(<= 1 2)");
        Assert.assertEquals(LispSymbol.ourT, r);
        r = evaluateString("(<= 1 1.0)");
        Assert.assertEquals(LispSymbol.ourT, r);
        r = evaluateString("(<= 2 1.0)");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }

    @Test
    public void testMoreOrEqual() {
        LispSymbol more = BuiltinArithmetic.moreOrEqual(new LispInteger(5), new LispFloat(1.3));
        Assert.assertEquals(LispSymbol.ourT, more);
        more = BuiltinArithmetic.moreOrEqual(new LispInteger(5), new LispFloat(5));
        Assert.assertEquals(LispSymbol.ourT, more);
        more = BuiltinArithmetic.moreOrEqual(new LispInteger(3), new LispFloat(4));
        Assert.assertEquals(LispSymbol.ourNil, more);
    }

    @Test
    public void testMinusInteger() throws LispException {
        LObject lispObject = evaluateString("(- 2 1)");
        Assert.assertEquals(new LispInteger(1), lispObject);
    }

    @Test
    public void testMinusOne() throws LispException {
        LObject lispObject = evaluateString("(- 1)");
        Assert.assertEquals(new LispInteger(-1), lispObject);
    }

    @Test
    public void testMinusZero() throws LispException {
        LObject lispObject = evaluateString("(-)");
        Assert.assertEquals(new LispInteger(0), lispObject);
    }

    @Test
    public void testMinusFloat () {
        LObject lispObject = evaluateString("(- 2 2.0)");
        Assert.assertEquals(new LispFloat(0.0), lispObject);
    }

    @Test
    public void testMinusSimple () {
        LispNumber n = BuiltinArithmetic.minus(new LispInteger(5), new LispFloat(6.8), new LispInteger(1));
        Assert.assertEquals(new LispFloat(-2.8), n);
    }

    @Test
    public void testMinusMarker () {
        try {
            BuiltinArithmetic.minus(new LispMarker(), new LispMarker());
        } catch (Exception e) {
            Assert.assertEquals("'(error \"Marker does not point anywhere\")", TestSetup.getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }
    
    @Test
    public void testLogAnd() {
        LObject r = evaluateString("(logand)");
        Assert.assertEquals(new LispInteger(-1), r);
        r = evaluateString("(logand 0)");
        Assert.assertEquals(new LispInteger(0), r);
        r = evaluateString("(logand 1)");
        Assert.assertEquals(new LispInteger(1), r);
        r = evaluateString("(logand 12 20)");
        Assert.assertEquals(new LispInteger(4), r);
    }

    @Test
    public void testLogNot() {
        LObject r = evaluateString("(lognot 0)");
        Assert.assertEquals(new LispInteger(-1), r);
        r = evaluateString("(lognot 1)");
        Assert.assertEquals(new LispInteger(-2), r);
        r = evaluateString("(lognot -1)");
        Assert.assertEquals(new LispInteger(0), r);
    }
    
}
