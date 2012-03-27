package org.jetbrains.emacs4ij;

import com.intellij.testFramework.fixtures.CodeInsightFixtureTestCase;
import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.ForwardParser;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinsKey;
import org.junit.Before;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/19/12
 * Time: 3:19 PM
 * To change this template use File | Settings | File Templates.
 */
public class KeymapTest extends CodeInsightFixtureTestCase {
    private Environment myEnvironment;

    @Before
    public void setUp() throws Exception {
        TestSetup.setGlobalEnv();
        super.setUp();
        GlobalEnvironment.initialize(new KeymapCreator(), new BufferCreator(), new IdeProvider());
        myEnvironment = new CustomEnvironment(GlobalEnvironment.INSTANCE);
    }

    private LispObject evaluateString (String lispCode) throws LispException {
        ForwardParser forwardParser = new ForwardParser();
        return forwardParser.parseLine(lispCode).evaluate(myEnvironment);
    }

    @Test
    public void testKeymapP() throws Exception {
        evaluateString("(setq k (make-sparse-keymap))");
        Assert.assertEquals(LispSymbol.ourT, evaluateString("(keymapp k)"));
        evaluateString("(setq m 1)");
        evaluateString("(fset 'm (make-sparse-keymap))");
        Assert.assertEquals(LispSymbol.ourT, evaluateString("(keymapp 'm)"));
    }

    @Test
    public void testMakeKeymap() throws Exception {
        LispObject r = evaluateString("(make-keymap 'a)");
        Assert.assertEquals("a", r.toString());
    }

    @Test
    public void testCopyKeymap() throws Exception {
        LispSymbol k1 = (LispSymbol) evaluateString("(defvar k1 (make-keymap))");
        LispSymbol k2 = (LispSymbol) evaluateString("(defvar k2 (copy-keymap k1))");
        Assert.assertEquals(k1.getValue(), k2.getValue());
        evaluateString("(set-keymap-parent k1 (make-sparse-keymap))");
        LispObject parent = evaluateString("(keymap-parent k1)");
        Assert.assertEquals(BuiltinsKey.makeSparseKeymap(null), parent);
        parent = evaluateString("(keymap-parent k2)");
        Assert.assertEquals(LispSymbol.ourNil, parent);
    }

    @Test
    public void testCopyKeymapAsSymbolFunction() throws Exception {
        evaluateString("(defvar k1 1)");
        evaluateString("(fset 'k1 (make-keymap))");
        LispSymbol k1 = (LispSymbol) evaluateString("'k1");
        LispSymbol k2 = (LispSymbol) evaluateString("(defvar k2 (copy-keymap 'k1))");
        Assert.assertEquals(k1.getFunction(), k2.getValue());
    }

    @Test
    public void testKeymapParent() throws Exception {
        evaluateString("(setq k (make-sparse-keymap))");
        LispObject r = evaluateString("(keymap-parent k)");
        Assert.assertEquals(LispSymbol.ourNil, r);

        evaluateString("(setq k (make-keymap))");
        r = evaluateString("(keymap-parent k)");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }

    @Test
    public void testSetKeymapParentSparseTwice() throws Exception {
        evaluateString("(setq k (make-sparse-keymap))");
        evaluateString("(set-keymap-parent k (make-sparse-keymap))");
        LispKeymap sparseKeymap = (LispKeymap) evaluateString("(set-keymap-parent k (make-sparse-keymap))");
        LispKeymap keymap = (LispKeymap) evaluateString("k");
        Assert.assertEquals(sparseKeymap, keymap.getParent());
    }

    @Test
    public void testSetKeymapParentSparse() throws Exception {
        evaluateString("(setq k (make-sparse-keymap))");
        LispObject parent = evaluateString("(set-keymap-parent k (make-sparse-keymap))");
        LispObject r = evaluateString("(keymap-parent k)");
        Assert.assertEquals(parent, r);
    }

    @Test
    public void testSetKeymapParentAsSymbol () {
        evaluateString("(defvar parent 1)");
        LispObject parent = evaluateString("(fset 'parent (make-sparse-keymap))");
        evaluateString("(defvar km (make-sparse-keymap))");
        evaluateString("(set-keymap-parent km 'parent)");
        LispKeymap keymap = (LispKeymap) evaluateString("km");
        Assert.assertEquals(parent, keymap.getParent());
    }

    @Test
    public void testDefineCtrlKey() {
        evaluateString("(setq p (make-sparse-keymap))");
        evaluateString("(define-key p \"\\C-d\" 'a)");
        LispKeymap keymap = (LispKeymap) evaluateString("p");
        Assert.assertEquals(new LispSymbol("a"), keymap.getKeyBinding(new LispString("\\C-d")));
    }

    @Test
    public void testParentWithParent () {
        evaluateString("(setq k (make-sparse-keymap))");
        evaluateString("(setq p (make-sparse-keymap))");
        evaluateString("(define-key p \"3\" 'forward-char)");
        evaluateString("(define-key k \"1\" 'forward-char)");
        evaluateString("(define-key k \"2\" 'forward-char)");
        evaluateString("(set-keymap-parent p (make-sparse-keymap))");
        evaluateString("(set-keymap-parent k p)");
        LispKeymap keymap = (LispKeymap) evaluateString("k");
        LispKeymap parent = (LispKeymap) evaluateString("p");
        Assert.assertEquals(parent, keymap.getParent());
    }

    @Test
    public void testCyclicParents() {
        evaluateString("(setq k (make-sparse-keymap))");
        evaluateString("(setq p1 (make-sparse-keymap))");
        evaluateString("(setq p2 (make-sparse-keymap))");
        evaluateString("(set-keymap-parent k p1)");
        evaluateString("(set-keymap-parent p1 p2)");
        try {
            evaluateString("(set-keymap-parent p2 k)");
        } catch (Exception e) {
            Assert.assertEquals("(error \"Cyclic keymap inheritance\")", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testCyclicParentsSelf() {
        evaluateString("(setq k (make-sparse-keymap))");
        try {
            evaluateString("(set-keymap-parent k k)");
        } catch (Exception e) {
            Assert.assertEquals("(error \"Cyclic keymap inheritance\")", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testCyclicParentsCdr() {
        evaluateString("(setq k (make-sparse-keymap))");
        evaluateString("(setq p1 (make-sparse-keymap))");
        evaluateString("(setq p2 (make-sparse-keymap))");
        evaluateString("(set-keymap-parent k p1)");
        evaluateString("(set-keymap-parent p1 p2)");
        try {
            evaluateString("(set-keymap-parent p1 k)");
        } catch (Exception e) {
            Assert.assertEquals("(error \"Cyclic keymap inheritance\")", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }


//    @Test
//    public void testClLoopLet() {
//        evaluateString("(setq loop-for-sets '((ch (aref --cl-vec-- --cl-idx--))))");
//        evaluateString("(setq arg0 (nreverse loop-for-sets))");
//        evaluateString("(setq arg1 '(quote setq))");
//        evaluateString("(setq arg2 nil)");
//        GlobalEnvironment.INSTANCE.findAndRegisterEmacsForm("cl-loop-let", "/lisp/emacs-lisp/cl.el", GlobalEnvironment.SymbolType.FUN);
//        LispObject r = evaluateString("(cl-loop-let arg0 arg1 arg2)");
//        Assert.assertEquals("(let* ((ch (aref --cl-vec-- --cl-idx--))) quote setq)", r.toString());
//    }
//
//    @Test
//    public void testKbdMacro() {
//        LispObject r = evaluateString("(kbd \"C-x\")");
//        Assert.assertEquals(new LispString("^X"), r);
//        r = evaluateString("(kbd \"\\C-x\")");
//        Assert.assertEquals(new LispString("^X"), r);
//        r = evaluateString("(kbd \"\\C-x\\t\")");
//        Assert.assertEquals(new LispString("^X"), r);
//        r = evaluateString("(kbd \"^x\")");
//        Assert.assertEquals(new LispString("^X"), r);
//        Assert.assertEquals(1, ((LispSequence)r).length());
//    }

    @Test
    public void testCurrentGlobalKeyMap() {
        System.out.println(evaluateString("(current-global-map)").toString());
    }

//    @Test
//    public void testEventConvertListNil() {
//        LispObject r = evaluateString("(event-convert-list \"C-x\")");
//        Assert.assertEquals(LispSymbol.ourNil, r);
//        r = evaluateString("(event-convert-list 5)");
//        Assert.assertEquals(LispSymbol.ourNil, r);
//    }
//
//    @Test
//    public void testEventConvertList() {
//        LispObject r = evaluateString("(event-convert-list '(C ?s))");
//        Assert.assertEquals(new LispInteger(19), r);
//        r = evaluateString("(event-convert-list '(C M S alt ?s))");
//        Assert.assertEquals(new LispInteger(171966483), r);
//    }
//
//    @Test
//    public void testEventConvertListWords() {
//        LispObject r = evaluateString("(event-convert-list '(control meta ?a))");
//        Assert.assertEquals(new LispInteger(134217729), r);
//        r = evaluateString("(event-convert-list '(control super f1))");
//        Assert.assertEquals(new LispSymbol("C-s-f1"), r);
//    }
//
//    @Test
//    public void testEventConvertListWrong() {
//        try {
//            evaluateString("(event-convert-list '(C ?s ?D ?g))");
//        } catch (Exception e) {
//            Assert.assertEquals("(error \"Two bases given in one event\")", TestSetup.getCause(e));
//            return;
//        }
//        Assert.fail();
//    }

    @Test
    public void testKeyDescription() {
        LispObject r = evaluateString("(key-description '[1 2 3] '[6 7])");
        Assert.assertEquals(new LispString("C-f C-g C-a C-b C-c"), r);
    }

    //todo
    /*
     (single-key-description ?\C-x)
               ⇒ "C-x"
          (key-description "\C-x \M-y \n \t \r \f123")
               ⇒ "C-x SPC M-y SPC C-j SPC TAB SPC RET SPC C-l 1 2 3"
          (single-key-description 'delete)
               ⇒ "<delete>"
          (single-key-description 'C-mouse-1)
               ⇒ "<C-mouse-1>"
          (single-key-description 'C-mouse-1 t)
               ⇒ "C-mouse-1"

          (key-description [?\M-3 delete])
               ⇒ "M-3 <delete>"
          (key-description [delete] "\M-3")
               ⇒ "M-3 <delete>"
     */

    @Test
    public void testDefineKeySimple() {
        evaluateString("(defvar km (make-sparse-keymap))");
        LispObject r = evaluateString("(define-key km \"d\" 'forward-char)");
        Assert.assertEquals(new LispSymbol("forward-char"), r);
        Assert.assertEquals(new LispSymbol("forward-char"), evaluateString("(lookup-key km \"d\")"));
    }

    @Test
    public void testDefineKeyInKeyMap() {
        evaluateString("(defvar km (make-keymap))");
        evaluateString("(define-key km \"C-d\" 'a)");
        LispObject a = evaluateString("(lookup-key km \"C-d\")");
        Assert.assertEquals(new LispSymbol("a"), a);
    }

    @Test
    public void testDefineKeyWrong() {
        try {
            evaluateString("(define-key (current-global-map) \"C-a\" 'backward-char)");
        } catch (Exception e) {
            Assert.assertEquals("(error \"Key sequence C - a starts with non-prefix key C\")", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
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

    @Test
    public void testDefinePrefixCommand () {
        evaluateString("(setq cmd 1)");
        evaluateString("(defvar mapvar 5)");
        evaluateString("(defvar name \"name\")");
        LispObject cmd = evaluateString("(define-prefix-command 'cmd 'mapvar 'name)");
        Assert.assertTrue(cmd instanceof LispSymbol);
        Assert.assertEquals(new LispInteger(1), ((LispSymbol) cmd).getValue());
        LispKeymap keymap = (LispKeymap) evaluateString("mapvar");
        Assert.assertEquals(((LispSymbol) cmd).getFunction(), keymap);
        Assert.assertTrue(keymap.isEmpty());
        Assert.assertEquals("name", keymap.getName());
        Assert.assertNull(keymap.getParent());
    }

    @Test
    public void testDefinePrefixCommandNoOpt () {
        evaluateString("(setq cmd 1)");
        LispObject cmd = evaluateString("(define-prefix-command 'cmd)");
        LispKeymap km = BuiltinsKey.makeSparseKeymap(null);
        Assert.assertTrue(cmd instanceof LispSymbol);
        Assert.assertEquals(km, ((LispSymbol) cmd).getValue());
        Assert.assertEquals(km, ((LispSymbol) cmd).getFunction());
    }

    @Test
    public void testPrefixKeymap() {
        evaluateString("(setq km (make-sparse-keymap))");
        evaluateString("(setq p (make-sparse-keymap))");
        evaluateString("(define-key p \"\\M-g\" 'a)");
        evaluateString("(define-key km \"\\C-q\" p)");
        LispObject a = evaluateString("(lookup-key km \"\\C-q\\M-g\")");
        Assert.assertEquals(a, new LispSymbol("a"));
    }

}