package org.jetbrains.emacs4ij.jelisp.subroutine;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.TestSetup;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/16/12
 * Time: 12:07 PM
 * To change this template use File | Settings | File Templates.
 */
public class BuiltinsKeyTest extends BaseSubroutineTest {
    @Test
    public void testKeymapP() throws Exception {
        LispObject r = evaluateString("(keymapp 5)");
        Assert.assertEquals(LispSymbol.ourNil, r);
        r = evaluateString("(keymapp '(keymap))");
        Assert.assertEquals(LispSymbol.ourT, r);
        r = evaluateString("(keymapp nil)");
        Assert.assertEquals(LispSymbol.ourNil, r);

        evaluateString("(setq k)");
        evaluateString("(fset 'k '(keymap))");
        r = evaluateString("(symbol-function 'k)");
        Assert.assertEquals(LispList.list(new LispSymbol("keymap")), r);
        r = evaluateString("(keymapp 'k)");
        Assert.assertEquals(LispSymbol.ourT, r);
    }

    @Test
    public void testMakeSparseKeymap() throws Exception {
        LispObject r = evaluateString("(setq k (make-sparse-keymap))");
        Assert.assertEquals(LispList.list(new LispSymbol("keymap")), r);
        r = evaluateString("k");
        Assert.assertEquals(LispList.list(new LispSymbol("keymap")), r);
    }

    @Test
    public void testMakeKeymap() throws Exception {
        LispObject r = evaluateString("(make-keymap 5)");
        String expected = "(keymap #^[nil nil keymap nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] 5)";
        Assert.assertEquals(expected, r.toString());
    }

    @Test
    public void testCopyKeymap() throws Exception {
    //todo
    }

    @Test
    public void testKeymapParent() throws Exception {
        evaluateString("(setq k (make-sparse-keymap))");
        LispObject r = evaluateString("(keymap-parent k)");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }

    @Test
    public void testSetKeymapParent() throws Exception {
        evaluateString("(setq k (make-sparse-keymap))");
        LispObject r = evaluateString("(set-keymap-parent k '(keymap))");
        Assert.assertEquals(LispList.list(new LispSymbol("keymap")), r);
        r = evaluateString("(keymap-parent k)");
        Assert.assertEquals(LispList.list(new LispSymbol("keymap")), r);
        r = evaluateString("k");
        Assert.assertEquals(LispList.list(new LispSymbol("keymap"), new LispSymbol("keymap")), r);
    }
    
    @Test
    public void testClLoopLet() {
        evaluateString("(setq loop-for-sets '((ch (aref --cl-vec-- --cl-idx--))))");
        evaluateString("(setq arg0 (nreverse loop-for-sets))");
        evaluateString("(setq arg1 '(quote setq))");
        evaluateString("(setq arg2 nil)");
        GlobalEnvironment.INSTANCE.findAndRegisterEmacsForm("cl-loop-let", "/lisp/emacs-lisp/cl.el", GlobalEnvironment.SymbolType.FUN);
        LispObject r = evaluateString("(cl-loop-let arg0 arg1 arg2)");
        Assert.assertEquals("(#<subr let*> ((ch (aref --cl-vec-- --cl-idx--))) quote setq)", r.toString());
    }

    @Test
    public void testKbdMacro() {
        LispObject r = evaluateString("(kbd \"C-x\")");
        Assert.assertEquals(new LispString("^X"), r);
    }

    @Test
    public void testCurrentGlobalKeyMap() {
        System.out.println(evaluateString("(current-global-map)").toString());
    }
    
    @Test
    public void testEventConvertListNil() {
        LispObject r = evaluateString("(event-convert-list \"C-x\")");
        Assert.assertEquals(LispSymbol.ourNil, r);
        r = evaluateString("(event-convert-list 5)");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }

    @Test
    public void testEventConvertList() {
        LispObject r = evaluateString("(event-convert-list '(C ?s))");
        Assert.assertEquals(new LispInteger(19), r);
        r = evaluateString("(event-convert-list '(C M S alt ?s))");
        Assert.assertEquals(new LispInteger(171966483), r);
    }

    @Test
    public void testEventConvertListWrong() {
        try {
            evaluateString("(event-convert-list '(C ?s ?D ?g))");
        } catch (Exception e) {
            Assert.assertEquals("(error \"Two bases given in one event\")", TestSetup.getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }
    
    @Test
    public void testKeyDescription() {
        LispObject r = evaluateString("(key-description '[1 2 3] '[6 7])");
        Assert.assertEquals(new LispString("C-f C-g C-a C-b C-c"), r);
    }

    @Test
    public void testDefineKey() {
        evaluateString("(defvar km (make-sparse-keymap))");
        LispObject r = evaluateString("(define-key km \"\\C-d\" 'forward-char)");
        Assert.assertEquals(new LispSymbol("forward-char"), r);
        LispObject keyMap = evaluateString("km");
        Assert.assertEquals("(keymap (92 keymap (67 keymap (45 keymap (100 . #<subr forward-char>)))))", keyMap.toString());
    }

    @Test
    public void testDefineKeyTwo() {
        evaluateString("(defvar km (make-sparse-keymap))");
        evaluateString("(define-key km \"\\C-d\" 'forward-char)");
        LispObject r = evaluateString("(define-key km \"M-d\" 'forward-char)");
        Assert.assertEquals(new LispSymbol("forward-char"), r);
        LispObject keyMap = evaluateString("km");
        Assert.assertEquals("(keymap (77 keymap (45 keymap (100 . #<subr forward-char>))) (92 keymap (67 keymap (45 keymap (100 . #<subr forward-char>)))))", keyMap.toString());
    }

    @Test
    public void testDefineKeyInKeyMap() {
        evaluateString("(defvar km (make-keymap))");
        evaluateString("(define-key km \"C-d\" 'forward-char)");
        LispObject km = evaluateString("km");
        String expected = "(keymap #^[nil nil keymap " +
                "#^^[3 0 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil (keymap (45 keymap (100 . #<subr forward-char>))) nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] #^^[1 0 #^^[2 0 " +
                "#^^[3 0 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil (keymap (45 keymap (100 . #<subr forward-char>))) nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil])";
        Assert.assertEquals(expected, km.toString());
    }

    @Test
    public void testGlobalSetKey() {
        LispObject r = evaluateString("(global-set-key \"\\C-q\" 'forward-char)");
        Assert.assertEquals(new LispSymbol("forward-char"), r);
    }
    
    @Test
    public void testGlobalMap() {
        LispObject r = evaluateString("global-map");
        System.out.println(r.toString());
        r = evaluateString("ctl-x-map");
        System.out.println(r.toString());
    }
}
