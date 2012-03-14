package org.jetbrains.emacs4ij.jelisp.subroutine;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.TestSetup;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/6/12
 * Time: 5:39 PM
 * To change this template use File | Settings | File Templates.
 */
public class BuiltinsSequenceTest extends BaseSubroutineTest{
    @Test
    public void testLength() throws Exception {
        LispObject r = evaluateString("(length '(1 2 3))");
        Assert.assertEquals(new LispInteger(3), r);
        r = evaluateString("(length ())");
        Assert.assertEquals(new LispInteger(0), r);
        r = evaluateString("(length \"foobar\")");
        Assert.assertEquals(new LispInteger(6), r);
        r = evaluateString("(length [1 2 3])");
        Assert.assertEquals(new LispInteger(3), r);
        //todo: (length (make-bool-vector 5 nil)) ⇒ 5
    }
    
    @Test
    public void testAppend() {
        LispObject r = evaluateString("(append \"h\" [1] \"llo\")");
        Assert.assertEquals(LispList.testList(new LispInteger(104), new LispInteger(1), new LispString("llo")), r);
        r = evaluateString("(append '() 'a)");
        Assert.assertEquals(new LispSymbol("a"), r);
        r = evaluateString("(append nil nil nil nil)");
        Assert.assertEquals(LispSymbol.ourNil, r);
        r = evaluateString("(append)");
        Assert.assertEquals(LispSymbol.ourNil, r);
        
        r = evaluateString("(append '(+ 2 3) '(+ 2 3 nil))");
        Assert.assertEquals(LispList.list(new LispSymbol("+"), new LispInteger(2), new LispInteger(3),
                new LispSymbol("+"), new LispInteger(2), new LispInteger(3), LispSymbol.ourNil), r);
    }
    
    @Test
    public void testMapCar() {
        LispObject r = evaluateString("(mapcar '+ \"hi\")");
        Assert.assertEquals(LispList.list(new LispInteger(104), new LispInteger(105)), r);
        r = evaluateString("(mapcar '+ nil)");
        Assert.assertEquals(LispSymbol.ourNil, r);
        r = evaluateString("(mapcar '+ '(1 2))");
        Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(2)), r);
        r = evaluateString("(mapcar '+ '[1 2])");
        Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(2)), r);
    }

    @Test
    public void testMapCarListException() {
        try {
            evaluateString("(mapcar '+ '(1 . 2))");
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument listp 2)", TestSetup.getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }

    @Test
    public void testMapCarFunctionException() {
        try {
            evaluateString("(mapcar 'quote '(1 . 2))");
        } catch (Exception e) {
            Assert.assertEquals("'(invalid-function quote)", TestSetup.getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }
    
    @Test
    public void testMapCarLambda() {
        LispObject r = evaluateString("(mapcar '(lambda (a) (+ 1 a)) '(1 2 3))");
        Assert.assertEquals(LispList.list(new LispInteger(2), new LispInteger(3), new LispInteger(4)), r);
    }
    
    @Test
    public void testConcat() {
        LispObject r = evaluateString("(concat)");
        Assert.assertEquals(new LispString(""), r);
        r = evaluateString("(concat nil nil)");
        Assert.assertEquals(new LispString(""), r);
        r = evaluateString("(concat '() nil)");
        Assert.assertEquals(new LispString(""), r);
        r = evaluateString("(concat \"hello\")");
        Assert.assertEquals(new LispString("hello"), r);
        r = evaluateString("(concat '(1 2) '[3 4] \"hello\")");
        Assert.assertEquals(new LispString("^A^B^C^Dhello"), r);
        r = evaluateString("(concat '(0))");
        Assert.assertEquals(new LispString("^@"), r);
    }

    @Test
    public void testConcatSpecial() {
        LispObject r = evaluateString("(concat '(127))");
        Assert.assertEquals(new LispString("^?"), r);
        r = evaluateString("(concat '(160))");
        Assert.assertEquals(new LispString("_"), r);
    }

    @Test
    public void testConcatOctal() {
        LispObject r = evaluateString("(concat '(128))");
        Assert.assertEquals(new LispString("\\200"), r);
        r = evaluateString("(concat '(159))");
        Assert.assertEquals(new LispString("\\237"), r);
    }

    @Test
    public void testConcatLetters() {
        LispObject r = evaluateString("(concat '(97))");
        Assert.assertEquals(new LispString("a"), r);
        r = evaluateString("(concat '(32))");
        Assert.assertEquals(new LispString(" "), r);
        r = evaluateString("(concat '(31))");
        Assert.assertEquals(new LispString("^_"), r);
        r = evaluateString("(concat '(64))");
        Assert.assertEquals(new LispString("@"), r);
    }

    @Test
    public void testConcatWrongChar() {
        try {
            evaluateString("(concat '(\"hello\"))");
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument characterp \"hello\")", TestSetup.getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }

    @Test
    public void testConcatWrongChar2() {
        try {
            evaluateString("(concat '(134217825))");
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument characterp 134217825)", TestSetup.getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }

    @Test
    public void testConcatWrongChar3() {
        try {
            evaluateString("(concat '(4194401))");
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument characterp 4194401)", TestSetup.getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }
    
    @Test
    public void testConcatWrongChar4() {
        try {
            evaluateString("(concat '(4194304))");
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument characterp 4194304)", TestSetup.getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }
    
    @Test
    public void testConcatLimit() {
        LispObject r = evaluateString("(concat '(4194303))");
//        Assert.assertEquals(new LispString("ÿ"), r);
    }
    
    @Test
    public void testVConcat() {
        LispObject r = evaluateString("(vconcat '(1 2) '[3 4] \"hello\")");
        Assert.assertEquals("[1 2 3 4 104 101 108 108 111]", r.toString());
        r = evaluateString("(vconcat '(1.6 \"hi\") nil '[3 4] \"hello\")");
        Assert.assertEquals("[1.6 \"hi\" 3 4 104 101 108 108 111]", r.toString());
    }

    @Test
    public void testMapConCatWrong() {
        try {
            evaluateString("(mapconcat '1+ '[9 8 7] \" \")");
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument sequencep 10)", TestSetup.getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }

    @Test
    public void testMapConCat() {
        LispObject r = evaluateString("(mapconcat 'identity '[\"9\" \"8\" \"7\"] \" \")");
        Assert.assertEquals(new LispString("9 8 7"), r);
        r = evaluateString("(mapconcat 'identity '(nil nil) \" \")");
        Assert.assertEquals(new LispString(" "), r);
    }
    
}
