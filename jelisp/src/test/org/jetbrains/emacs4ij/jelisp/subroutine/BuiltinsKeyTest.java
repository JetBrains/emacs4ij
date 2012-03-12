package org.jetbrains.emacs4ij.jelisp.subroutine;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.LObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
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
        LObject r = evaluateString("(keymapp 5)");
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
        LObject r = evaluateString("(setq k (make-sparse-keymap))");
        Assert.assertEquals(LispList.list(new LispSymbol("keymap")), r);
        r = evaluateString("k");
        Assert.assertEquals(LispList.list(new LispSymbol("keymap")), r);
    }

    @Test
    public void testMakeKeymap() throws Exception {
        LObject r = evaluateString("(make-keymap 5)");
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
        LObject r = evaluateString("(keymap-parent k)");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }

    @Test
    public void testSetKeymapParent() throws Exception {
        evaluateString("(setq k (make-sparse-keymap))");
        LObject r = evaluateString("(set-keymap-parent k '(keymap))");
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
        LObject r = evaluateString("(cl-loop-let arg0 arg1 arg2)");
        Assert.assertEquals("(#<subr let*> ((ch (aref --cl-vec-- --cl-idx--))) quote setq)", r.toString());
    }

    @Test
    public void testKbdMacro() {
        LObject r = evaluateString("(kbd \"C-x\")");
        Assert.assertEquals(new LispString("^X"), r);
    }

    @Test
    public void testCurrentGlobalKeyMap() {
        System.out.println(evaluateString("(current-global-map)").toString());
    }
    
    @Test
    public void test1() {
        evaluateString("(event-convert-list)");
    }
    
    @Test
    public void testKeyDescription() {
        LObject r = evaluateString("(key-description '[1 2 3] '[6 7])");
        Assert.assertEquals(new LispString("C-f C-g C-a C-b C-c"), r);
    }
    
    @Test
    public void testGlobalSetKey() {
        LObject r = evaluateString("(global-set-key \"\\C-q\" 'forward-char)");
        Assert.assertEquals(new LispSymbol("forward-char"), r);
    }
    
    @Test
    public void testGlobalMap() {
        LObject r = evaluateString("global-map");
        System.out.println(r.toString());
        r = evaluateString("ctl-x-map");
        System.out.println(r.toString());
    }
}
