package org.jetbrains.emacs4ij.jelisp.subroutine;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.TestSetup;
import org.jetbrains.emacs4ij.jelisp.elisp.LispHashTable;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.junit.Test;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 5/12/12
 * Time: 11:56 AM
 * To change this template use File | Settings | File Templates.
 */
public class HashTableTest extends BaseSubroutineTest {
    @Test
    public void testMakeHashTableDefault() {
        LispObject hashTable = evaluateString("(make-hash-table)");
        Assert.assertEquals(new LispHashTable(LispHashTable.EqualityMethod.EQL, 65, (float)0.8, LispSymbol.ourNil, 1.5), hashTable);
        Assert.assertEquals("#s(hash-table size 65 test eql rehash-size 1.5 rehash-threshold 0.8 data ())",
                hashTable.toString());
    }

    @Test
    public void testMakeHashTableTestOk() {
        LispObject hashTable = evaluateString("(make-hash-table :test 'eq)");
        Assert.assertEquals(new LispHashTable(LispHashTable.EqualityMethod.EQ, 65, (float)0.8, LispSymbol.ourNil, 1.5), hashTable);
    }

    @Test
    public void testMakeHashTableTestNotSet() {
        try {
            evaluateString("(make-hash-table :test)");
        } catch (Exception e) {
            Assert.assertEquals("(error \"Invalid argument list\" :test)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testMakeHashTableTestSetTwice() {
        try {
            evaluateString("(make-hash-table :test 'eq :test 'eql)");
        } catch (Exception e) {
            Assert.assertEquals("(error \"Invalid argument list\" :test)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testMakeHashTableTestNotSymbol() {
        try {
            evaluateString("(make-hash-table :test 5 :test 'eql)");
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument symbolp 5)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testMakeHashTableTestOdd() {
        try {
            evaluateString("(make-hash-table :test 'eq :test)");
        } catch (Exception e) {
            Assert.assertEquals("(error \"Invalid argument list\" :test)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testMakeHashTableNotDefinedKeyword() {
        try {
            evaluateString("(make-hash-table :test 'eq :test1 5)");
        } catch (Exception e) {
            Assert.assertEquals("(error \"Invalid argument list\" :test1)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testMakeHashTableNotDefinedAction() {
        try {
            evaluateString("(make-hash-table :test 'a)");
        } catch (Exception e) {
            Assert.assertEquals("(error \"Invalid hash table test\" a)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testMakeHashTableAll() {
        LispObject hashTable = evaluateString("(make-hash-table :rehash-threshold 0.5 :test 'eq :size 10 :rehash-size 6.2 :weakness 'key)");
        Assert.assertEquals(new LispHashTable(LispHashTable.EqualityMethod.EQ, 10, (float)0.5, new LispSymbol("key"), 6.2), hashTable);
    }

    @Test
    public void testMakeHashTableSizeNotInt() {
        try {
            evaluateString("(make-hash-table :size 10.0)");
        } catch (Exception e) {
            Assert.assertEquals("(error \"Invalid hash table size\" 10.0)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testMakeHash() {
        LispObject hashTable = evaluateString("(makehash)");
        Assert.assertEquals(new LispHashTable(LispHashTable.EqualityMethod.EQL, 65, (float)0.8, LispSymbol.ourNil, 1.5), hashTable);
        Assert.assertEquals("#s(hash-table size 65 test eql rehash-size 1.5 rehash-threshold 0.8 data ())",
                hashTable.toString());

        hashTable = evaluateString("(makehash 'eq)");
        Assert.assertEquals(new LispHashTable(LispHashTable.EqualityMethod.EQ, 65, (float)0.8, LispSymbol.ourNil, 1.5), hashTable);
        Assert.assertEquals("#s(hash-table size 65 test eq rehash-size 1.5 rehash-threshold 0.8 data ())",
                hashTable.toString());
    }

    @Test
    public void testMapHash() {
        evaluateString("(setq table (makehash))");
        evaluateString("(puthash 'a 1 table)");
        LispObject table = evaluateString("table");
        LispHashTable expected = new LispHashTable(LispHashTable.EqualityMethod.EQL, 65, (float) 0.8, LispSymbol.ourNil, 1.5);
        expected.put(new LispSymbol("a"), new LispInteger(1));
        Assert.assertEquals(expected, table);

        evaluateString("(defun f (symbol int) (message \"%s\" symbol) (+ int 1))");
        Assert.assertEquals(LispSymbol.ourNil, evaluateString("(maphash 'f table)"));
    }
}
