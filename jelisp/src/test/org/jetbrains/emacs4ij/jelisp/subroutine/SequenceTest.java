package org.jetbrains.emacs4ij.jelisp.subroutine;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.JelispTestCase;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.elisp.LispVector;
import org.junit.Ignore;
import org.junit.Test;

public class SequenceTest extends JelispTestCase {
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
      Assert.assertEquals("'(wrong-type-argument listp 2)", getCauseMsg(e));
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

  @Ignore
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
      Assert.assertEquals("'(wrong-type-argument characterp \"hello\")", getCauseMsg(e));
      return;
    }
    Assert.fail();
  }

  @Test
  public void testConcatWrongChar2() {
    try {
      evaluateString("(concat '(134217825))");
    } catch (Exception e) {
      Assert.assertEquals("'(wrong-type-argument characterp 134217825)", getCauseMsg(e));
      return;
    }
    Assert.fail();
  }

  @Test
  public void testConcatWrongChar3() {
    try {
      evaluateString("(concat '(4194401))");
    } catch (Exception e) {
      Assert.assertEquals("'(wrong-type-argument characterp 4194401)", getCauseMsg(e));
      return;
    }
    Assert.fail();
  }

  @Test
  public void testConcatWrongChar4() {
    try {
      evaluateString("(concat '(4194304))");
    } catch (Exception e) {
      Assert.assertEquals("'(wrong-type-argument characterp 4194304)", getCauseMsg(e));
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
      Assert.assertEquals("'(wrong-type-argument sequencep 10)", getCauseMsg(e));
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

  @Test
  public void testMapC() {
    evaluateString("(defun f (n) (message \"%d\" n))");
    LispObject sequence = evaluateString("(mapc 'f '(1 2))");
    Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(2)), sequence);
    sequence = evaluateString("(mapc 'f '())");
    Assert.assertEquals(LispSymbol.ourNil, sequence);
  }

  @Test
  public void testMapCar2() {
    evaluateString("(defun f (n) (message \"%d\" n))");
    LispObject sequence = evaluateString("(setq seq '(1 2))");
    LispObject mapping = evaluateString("(mapcar 'f seq)");
    Assert.assertEquals(LispList.list(new LispString("1"), new LispString("2")), mapping);
    Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(2)), sequence);
  }

  @Test
  public void testDeleteFromString() {
    evaluateString("(setq s \"hello\")");
    evaluateString("(setq c (delete ?l s))");
    Assert.assertEquals(new LispString("heo"), evaluateString("c"));
    Assert.assertEquals(new LispString("hello"), evaluateString("s"));

    Assert.assertEquals(new LispString("hello"), evaluateString("(delete ?a s)"));
    Assert.assertEquals(new LispString("hello"), evaluateString("(delete \"h\" s)"));
  }

  @Test
  public void testDeleteFromVector() {
    evaluateString("(setq v '[a b c d])");
    evaluateString("(setq w (delete 'a v))");
    Assert.assertEquals(new LispVector(new LispSymbol("b"), new LispSymbol("c"), new LispSymbol("d")), evaluateString("w"));
    Assert.assertEquals(new LispVector(new LispSymbol("a"), new LispSymbol("b"), new LispSymbol("c"), new LispSymbol("d")), evaluateString("v"));
  }

  @Test
  public void testDeleteFromList1() {
    evaluateString("(setq a '(1 2 3 2 4))");
    LispList expected = LispList.list(new LispInteger(1), new LispInteger(3), new LispInteger(4));
    Assert.assertEquals(expected, evaluateString("(delete 2 a)"));
    Assert.assertEquals(expected, evaluateString("a"));
  }

  @Test
  public void testDeleteFromList2() {
    evaluateString("(setq a '(2 3 2 4))");
    Assert.assertEquals(LispList.list(new LispInteger(3), new LispInteger(4)), evaluateString("(delete 2 a)"));
    Assert.assertEquals(LispList.list(new LispInteger(2), new LispInteger(3), new LispInteger(4)), evaluateString("a"));
  }

  @Test
  public void testDeleteFromList3() {
    evaluateString("(setq a '(1 2))");
    LispList expected = LispList.list(new LispInteger(1));
    Assert.assertEquals(expected, evaluateString("(delete 2 a)"));
    Assert.assertEquals(expected, evaluateString("a"));
  }

  @Test
  public void testDeleteFromList4() {
    evaluateString("(setq a '(1 2))");
    Assert.assertEquals(LispList.list(new LispInteger(2)), evaluateString("(delete 1 a)"));
    Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(2)), evaluateString("a"));
  }

  @Test
  public void testDeleteFromList5() {
    evaluateString("(setq a '(1 . 2))");
    Assert.assertEquals(new LispInteger(2), evaluateString("(delete 1 a)"));
    Assert.assertEquals(LispList.cons(new LispInteger(1), new LispInteger(2)), evaluateString("a"));
  }

  @Test
  public void testDeleteFromList6() {
    evaluateString("(setq a '(1 . 2))");
    LispList expected = LispList.cons(new LispInteger(1), new LispInteger(2));
    Assert.assertEquals(expected, evaluateString("(delete 2 a)"));
    Assert.assertEquals(expected, evaluateString("a"));
  }

  @Test
  public void testDeleteFromList7() {
    evaluateString("(setq a '(1 2 1 . 2))");
    LispList expected = LispList.testList(new LispInteger(1), new LispInteger(1), new LispInteger(2));
    Assert.assertEquals(expected, evaluateString("(delete 2 a)"));
    Assert.assertEquals(expected, evaluateString("a"));
  }

  @Test
  public void test(){
    evaluateString("(setq selem '(\"abc\" . \"w\"))");
    LispObject list = evaluateString("(if (numberp (car selem)) (list (car selem)) (mapcar 'identity (car selem)))");
    Assert.assertEquals(LispList.list(new LispInteger(97), new LispInteger(98), new LispInteger(99)), list);
  }
}
