package org.jetbrains.emacs4ij.jelisp.subroutine;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.JelispTestCase;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.junit.Test;

public class TextPropertiesTest extends JelispTestCase {
  @Test
  public void testPropertize() {
    LispObject s = evaluateString("(setq s (propertize \"hello\" 1 2 3 4))");
    Assert.assertEquals("#(\"hello\" 0 5 (3 4 1 2))", s.toString());
  }

  @Test
  public void testPutTextProperty() {
    evaluateString("(setq s (propertize \"hello\" 1 2 3 4))");
    LispObject n = evaluateString("(put-text-property 1 3 'test 5 s)");
    Assert.assertEquals(LispSymbol.NIL, n);
    Assert.assertEquals("#(\"hello\" 0 1 (3 4 1 2) 1 3 (3 4 1 2 test 5) 3 5 (3 4 1 2))", evaluateString("s").toString());
  }

  @Test
  public void testAddTextProperties() {
    evaluateString("(setq s (propertize \"hello\" 1 2 3 4))");
    Assert.assertEquals(LispSymbol.T, evaluateString("(add-text-properties 0 2 '(\"test\" 1) s)"));
    Assert.assertEquals("#(\"hello\" 0 2 (3 4 1 2 \"test\" 1) 2 5 (3 4 1 2))", evaluateString("s").toString());
  }

  @Test
  public void testAddTextPropertiesWhenEmpty()  {
    evaluateString("(setq s \"hello\")");
    Assert.assertEquals(LispSymbol.T, evaluateString("(add-text-properties 0 2 '(\"test\" 1) s)"));
    Assert.assertEquals("#(\"hello\" 0 2 (\"test\" 1))", evaluateString("s").toString());
  }

  @Test
  public void testSetTextPropertiesWhenEmpty()  {
    evaluateString("(setq s \"hello\")");
    Assert.assertEquals(LispSymbol.T, evaluateString("(set-text-properties 0 2 '(\"test\" 1) s)"));
    Assert.assertEquals("#(\"hello\" 0 2 (\"test\" 1))", evaluateString("s").toString());
  }

  @Test
  public void testRemoveTextPropertiesWhenEmpty()  {
    evaluateString("(setq s \"hello\")");
    Assert.assertEquals(LispSymbol.NIL, evaluateString("(remove-text-properties 0 2 '(\"test\" 1) s)"));
    Assert.assertEquals("\"hello\"", evaluateString("s").toString());
  }

  @Test
  public void testSetTextProperties () {
    evaluateString("(setq s (propertize \"hello\" 1 2 3 4))");
    Assert.assertEquals(LispSymbol.T, evaluateString("(set-text-properties 0 5 '(3 4 1 2) s)"));
    Assert.assertEquals("#(\"hello\" 0 5 (1 2 3 4))", evaluateString("s").toString());

    Assert.assertEquals(LispSymbol.T, evaluateString("(set-text-properties 3 5 '(5 6) s)"));
    Assert.assertEquals("#(\"hello\" 0 3 (1 2 3 4) 3 5 (5 6))", evaluateString("s").toString());
  }

  @Test
  public void testSetNilTextProperties () {
    evaluateString("(setq s (propertize \"hello\" 1 2 3 4))");
    Assert.assertEquals(LispSymbol.T, evaluateString("(set-text-properties 0 5 nil s)"));
    Assert.assertEquals("\"hello\"", evaluateString("s").toString());

    Assert.assertEquals(LispSymbol.NIL, evaluateString("(set-text-properties 0 1 nil \"q\")"));
  }

  @Test
  public void testRemoveTextProperties () {
    evaluateString("(setq s (propertize \"hello\" 1 2 3 4))");
    Assert.assertEquals(LispSymbol.NIL, evaluateString("(remove-text-properties 1 2 nil s)"));
    Assert.assertEquals("#(\"hello\" 0 5 (3 4 1 2))", evaluateString("s").toString());

    Assert.assertEquals(LispSymbol.NIL, evaluateString("(remove-text-properties 1 2 3 s)"));
    Assert.assertEquals("#(\"hello\" 0 5 (3 4 1 2))", evaluateString("s").toString());

    Assert.assertEquals(LispSymbol.T, evaluateString("(remove-text-properties 1 2 '(3) s)"));
    Assert.assertEquals("#(\"hello\" 0 1 (3 4 1 2) 1 2 (1 2) 2 5 (3 4 1 2))", evaluateString("s").toString());

    Assert.assertEquals(LispSymbol.T, evaluateString("(remove-text-properties 1 2 '(1) s)"));
    Assert.assertEquals("#(\"hello\" 0 1 (3 4 1 2) 2 5 (3 4 1 2))", evaluateString("s").toString());
  }

  @Test
  public void testRemoveListOfTextProperties () {
    evaluateString("(setq s (propertize \"hello\" 1 2 3 4))");
    Assert.assertEquals(LispSymbol.NIL, evaluateString("(remove-list-of-text-properties 1 2 nil s)"));
    Assert.assertEquals("#(\"hello\" 0 5 (3 4 1 2))", evaluateString("s").toString());

    Assert.assertEquals(LispSymbol.NIL, evaluateString("(remove-list-of-text-properties 1 2 3 s)"));
    Assert.assertEquals("#(\"hello\" 0 5 (3 4 1 2))", evaluateString("s").toString());

    Assert.assertEquals(LispSymbol.T, evaluateString("(remove-list-of-text-properties 1 2 '(3) s)"));
    Assert.assertEquals("#(\"hello\" 0 1 (3 4 1 2) 1 2 (1 2) 2 5 (3 4 1 2))", evaluateString("s").toString());
  }

  @Test
  public void testRemoveListOfTextPropertiesMix () {
    evaluateString("(setq s (propertize \"hello\" 1 2 3 4))");
    Assert.assertEquals(LispSymbol.T, evaluateString("(remove-list-of-text-properties 1 2 '(3 1) s)"));
    Assert.assertEquals("#(\"hello\" 0 1 (3 4 1 2) 2 5 (3 4 1 2))", evaluateString("s").toString());
  }

  @Test
  public void testRemoveListOfTextNotSetProperties () {
    evaluateString("(setq s (propertize \"hello\" 1 2 3 4))");
    Assert.assertEquals(LispSymbol.NIL, evaluateString("(remove-list-of-text-properties 1 2 '(9 8 6) s)"));
    Assert.assertEquals("#(\"hello\" 0 5 (3 4 1 2))", evaluateString("s").toString());
  }

  @Test
  public void testGetCharProperty() {
    evaluateString("(setq s \"hello world\")");

  }

}


