package org.jetbrains.emacs4ij.jelisp.subroutine;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.junit.Test;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 5/11/12
 * Time: 1:30 PM
 * To change this template use File | Settings | File Templates.
 */
public class TextPropertiesTest extends BaseSubroutineTest {
    @Test
    public void testPropertize() throws Exception {
        LispObject s = evaluateString("(setq s (propertize \"hello\" 1 2 3 4))");
        Assert.assertEquals("#(\"hello\" 0 5 (3 4 1 2))", s.toString());
    }

    @Test
    public void testPutTextProperty() throws Exception {
        evaluateString("(setq s (propertize \"hello\" 1 2 3 4))");
        LispObject n = evaluateString("(put-text-property 1 3 'test 5 s)");
        Assert.assertEquals(LispSymbol.ourNil, n);
        Assert.assertEquals("#(\"hello\" 0 1 (3 4 1 2) 1 3 (3 4 1 2 test 5) 3 5 (3 4 1 2))", evaluateString("s").toString());
    }

    @Test
    public void testAddTextProperties() throws Exception {
        evaluateString("(setq s (propertize \"hello\" 1 2 3 4))");
        Assert.assertEquals(LispSymbol.ourT, evaluateString("(add-text-properties 0 2 '(\"test\" 1) s)"));
        Assert.assertEquals("#(\"hello\" 0 2 (3 4 1 2 \"test\" 1) 2 5 (3 4 1 2))", evaluateString("s").toString());
    }

    @Test
    public void testSetTextProperties () {
        evaluateString("(setq s (propertize \"hello\" 1 2 3 4))");
        Assert.assertEquals(LispSymbol.ourT, evaluateString("(set-text-properties 0 5 '(3 4 1 2) s)"));
        Assert.assertEquals("#(\"hello\" 0 5 (1 2 3 4))", evaluateString("s").toString());

        Assert.assertEquals(LispSymbol.ourT, evaluateString("(set-text-properties 3 5 '(5 6) s)"));
        Assert.assertEquals("#(\"hello\" 0 3 (1 2 3 4) 3 5 (5 6))", evaluateString("s").toString());
    }

    @Test
    public void testSetNilTextProperties () {
        evaluateString("(setq s (propertize \"hello\" 1 2 3 4))");
        Assert.assertEquals(LispSymbol.ourT, evaluateString("(set-text-properties 0 5 nil s)"));
        Assert.assertEquals("\"hello\"", evaluateString("s").toString());

        Assert.assertEquals(LispSymbol.ourNil, evaluateString("(set-text-properties 0 1 nil \"q\")"));
    }

    @Test
    public void testRemoveTextProperties () {
        evaluateString("(setq s (propertize \"hello\" 1 2 3 4))");
        Assert.assertEquals(LispSymbol.ourNil, evaluateString("(remove-text-properties 1 2 nil s)"));
        Assert.assertEquals("#(\"hello\" 0 5 (3 4 1 2))", evaluateString("s").toString());

        Assert.assertEquals(LispSymbol.ourNil, evaluateString("(remove-text-properties 1 2 3 s)"));
        Assert.assertEquals("#(\"hello\" 0 5 (3 4 1 2))", evaluateString("s").toString());

        Assert.assertEquals(LispSymbol.ourT, evaluateString("(remove-text-properties 1 2 '(3) s)"));
        Assert.assertEquals("#(\"hello\" 0 1 (3 4 1 2) 1 2 (1 2) 2 5 (3 4 1 2))", evaluateString("s").toString());

        Assert.assertEquals(LispSymbol.ourT, evaluateString("(remove-text-properties 1 2 '(1) s)"));
        Assert.assertEquals("#(\"hello\" 0 1 (3 4 1 2) 2 5 (3 4 1 2))", evaluateString("s").toString());
    }

    @Test
    public void testRemoveListOfTextProperties () {
        evaluateString("(setq s (propertize \"hello\" 1 2 3 4))");
        Assert.assertEquals(LispSymbol.ourNil, evaluateString("(remove-list-of-text-properties 1 2 nil s)"));
        Assert.assertEquals("#(\"hello\" 0 5 (3 4 1 2))", evaluateString("s").toString());

        Assert.assertEquals(LispSymbol.ourNil, evaluateString("(remove-list-of-text-properties 1 2 3 s)"));
        Assert.assertEquals("#(\"hello\" 0 5 (3 4 1 2))", evaluateString("s").toString());

        Assert.assertEquals(LispSymbol.ourT, evaluateString("(remove-list-of-text-properties 1 2 '(3) s)"));
        Assert.assertEquals("#(\"hello\" 0 1 (3 4 1 2) 1 2 (1 2) 2 5 (3 4 1 2))", evaluateString("s").toString());
    }

    @Test
    public void testRemoveListOfTextPropertiesMix () {
        evaluateString("(setq s (propertize \"hello\" 1 2 3 4))");
        Assert.assertEquals(LispSymbol.ourT, evaluateString("(remove-list-of-text-properties 1 2 '(3 1) s)"));
        Assert.assertEquals("#(\"hello\" 0 1 (3 4 1 2) 2 5 (3 4 1 2))", evaluateString("s").toString());
    }

    @Test
    public void testRemoveListOfTextNotSetProperties () {
        evaluateString("(setq s (propertize \"hello\" 1 2 3 4))");
        Assert.assertEquals(LispSymbol.ourNil, evaluateString("(remove-list-of-text-properties 1 2 '(9 8 6) s)"));
        Assert.assertEquals("#(\"hello\" 0 5 (3 4 1 2))", evaluateString("s").toString());
    }

}


