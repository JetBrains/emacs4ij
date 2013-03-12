package org.jetbrains.emacs4ij.jelisp.subroutine;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.JelispTestCase;
import org.jetbrains.emacs4ij.jelisp.elisp.LispFloat;
import org.jetbrains.emacs4ij.jelisp.elisp.LispHashTable;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.junit.Ignore;
import org.junit.Test;

public class HashTableTest extends JelispTestCase {
  @Test
  public void testMakeHashTableDefault() {
    LispObject hashTable = evaluateString("(make-hash-table)");
    Assert.assertEquals(new LispHashTable("eql", 65, new LispFloat(0.8), LispSymbol.ourNil, 1.5), hashTable);
    Assert.assertEquals("#s(hash-table size 65 test eql rehash-size 1.5 rehash-threshold 0.8 data ())",
        hashTable.toString());
  }

  @Test
  public void testMakeHashTableTestOk() {
    LispObject hashTable = evaluateString("(make-hash-table :test 'eq)");
    Assert.assertEquals(new LispHashTable("eq", 65, new LispFloat(0.8), LispSymbol.ourNil, 1.5), hashTable);
  }

  @Test
  public void testMakeHashTableTestNotSet() {
    try {
      evaluateString("(make-hash-table :test)");
    } catch (Exception e) {
      Assert.assertEquals("(error \"Invalid argument list\" :test)", getCauseMsg(e));
      return;
    }
    Assert.fail();
  }

  @Test
  public void testMakeHashTableTestSetTwice() {
    try {
      evaluateString("(make-hash-table :test 'eq :test 'eql)");
    } catch (Exception e) {
      Assert.assertEquals("(error \"Invalid argument list\" :test)", getCauseMsg(e));
      return;
    }
    Assert.fail();
  }

  @Test
  public void testMakeHashTableTestNotSymbol() {
    try {
      evaluateString("(make-hash-table :test 5 :test 'eql)");
    } catch (Exception e) {
      Assert.assertEquals("'(wrong-type-argument symbolp 5)", getCauseMsg(e));
      return;
    }
    Assert.fail();
  }

  @Test
  public void testMakeHashTableTestOdd() {
    try {
      evaluateString("(make-hash-table :test 'eq :test)");
    } catch (Exception e) {
      Assert.assertEquals("(error \"Invalid argument list\" :test)", getCauseMsg(e));
      return;
    }
    Assert.fail();
  }

  @Test
  public void testMakeHashTableNotDefinedKeyword() {
    try {
      evaluateString("(make-hash-table :test 'eq :test1 5)");
    } catch (Exception e) {
      Assert.assertEquals("(error \"Invalid argument list\" :test1)", getCauseMsg(e));
      return;
    }
    Assert.fail();
  }

  @Test
  public void testMakeHashTableNotDefinedAction() {
    try {
      evaluateString("(make-hash-table :test 'a)");
    } catch (Exception e) {
      Assert.assertEquals("(error \"Invalid hash table test\" a)", getCauseMsg(e));
      return;
    }
    Assert.fail();
  }

  @Test
  public void testMakeHashTableAll() {
    LispObject hashTable = evaluateString("(make-hash-table :rehash-threshold 0.5 :test 'eq :size 10 :rehash-size 6.2 :weakness 'key)");
    Assert.assertEquals(new LispHashTable("eq", 10, new LispFloat(0.5), new LispSymbol("key"), 6.2), hashTable);
  }

  @Test
  public void testMakeHashTableSizeNotInt() {
    try {
      evaluateString("(make-hash-table :size 10.0)");
    } catch (Exception e) {
      Assert.assertEquals("(error \"Invalid hash table size\" 10.0)", getCauseMsg(e));
      return;
    }
    Assert.fail();
  }

  @Test
  public void testMakeHash() {
    LispObject hashTable = evaluateString("(makehash)");
    Assert.assertEquals(new LispHashTable("eql", 65, new LispFloat(0.8), LispSymbol.ourNil, 1.5), hashTable);
    Assert.assertEquals("#s(hash-table size 65 test eql rehash-size 1.5 rehash-threshold 0.8 data ())",
        hashTable.toString());

    hashTable = evaluateString("(makehash 'eq)");
    Assert.assertEquals(new LispHashTable("eq", 65, new LispFloat(0.8), LispSymbol.ourNil, 1.5), hashTable);
    Assert.assertEquals("#s(hash-table size 65 test eq rehash-size 1.5 rehash-threshold 0.8 data ())",
        hashTable.toString());
  }

  @Test
  public void testMapHash() {
    evaluateString("(setq table (makehash))");
    evaluateString("(puthash 'a 1 table)");
    LispObject table = evaluateString("table");
    LispHashTable expected = new LispHashTable("eql", 65, new LispFloat(0.8), LispSymbol.ourNil, 1.5);
    expected.put(new LispSymbol("a"), new LispInteger(1));
    Assert.assertEquals(expected, table);

    evaluateString("(defun f (symbol int) (message \"%s\" symbol) (+ int 1))");
    Assert.assertEquals(LispSymbol.ourNil, evaluateString("(maphash 'f table)"));
  }

  @Test
  public void testMapHashWrongNumOfArgs() {
    try {
      evaluateString("(makehash :size 10.0)");
    } catch (Exception e) {
      Assert.assertEquals("'(wrong-number-of-arguments (lambda (test) (make-hash-table :test (or test (quote eql)))) 2)",
          getCauseMsg(e));
      return;
    }
    Assert.fail();
  }

  @Test
  public void testMapHashWrongTest() {
    try {
      evaluateString("(makehash 'a)");
    } catch (Exception e) {
      Assert.assertEquals("(error \"Invalid hash table test\" a)", getCauseMsg(e));
      return;
    }
    Assert.fail();

  }

  @Test
  public void testMakeHashTableIntThreshold() {
    try {
      evaluateString("(make-hash-table :rehash-threshold 1)");
    } catch (Exception e) {
      Assert.assertEquals("(error \"Invalid hash table rehash threshold\" 1)", getCauseMsg(e));
      return;
    }
    Assert.fail();
  }

  @Test
  public void testMakeHashTableFloatThreshold() {
    evaluateString("(setq h (make-hash-table :rehash-threshold 1.0))");
    LispObject hashTable = evaluateString("h");
    Assert.assertEquals(new LispHashTable("eql", 65, new LispFloat(1.0), LispSymbol.ourNil, 1.5), hashTable);
  }

  @Test
  public void testHashMapCapacity() {
    evaluateString("(setq table (make-hash-table :size 1))");
    evaluateString("(puthash 'a 1 table)");
    LispObject table = evaluateString("table");
    LispHashTable expected = new LispHashTable("eql", 1, new LispFloat(0.8), LispSymbol.ourNil, 1.5);
    expected.put(new LispSymbol("a"), new LispInteger(1));
    Assert.assertEquals(expected, table);

    evaluateString("(puthash 'b 1 table)");
    table = evaluateString("table");
    expected = new LispHashTable("eql", 2, new LispFloat(0.8), LispSymbol.ourNil, 1.5);
    expected.put(new LispSymbol("a"), new LispInteger(1));
    expected.put(new LispSymbol("b"), new LispInteger(1));
    Assert.assertEquals(expected, table);
  }

  @Ignore
  @Test
  public void testReadFromPrintNotation() {
    evaluateString("(setq table #s(hash-table test eql rehash-size 1.5 size 65 data (key1 1 2 \"two\") rehash-threshold 0.8))");
    LispObject table = evaluateString("table");
    LispHashTable expected = new LispHashTable("eql", 65, new LispFloat(0.8), LispSymbol.ourNil, 1.5);
    expected.put(new LispSymbol("key1"), new LispInteger(1));
    expected.put(new LispInteger(2), new LispString("two"));
    Assert.assertEquals(expected, table);
  }
}
