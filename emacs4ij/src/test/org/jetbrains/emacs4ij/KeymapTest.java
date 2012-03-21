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
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinsList;
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
        GlobalEnvironment.initialize(new EmacsKeymapManagerImpl(), new BufferCreator(), new IdeProvider());
        myEnvironment = new CustomEnvironment(GlobalEnvironment.INSTANCE);
    }

    private LispObject evaluateString (String lispCode) throws LispException {
        ForwardParser forwardParser = new ForwardParser();
        return forwardParser.parseLine(lispCode).evaluate(myEnvironment);
    }

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
        LispSymbol k1 = (LispSymbol) evaluateString("(defvar k1 (make-keymap))");
        LispSymbol k2 = (LispSymbol) evaluateString("(defvar k2 (copy-keymap k1))");
        Assert.assertEquals(k1.getValue(), k2.getValue());
        evaluateString("(set-keymap-parent k1 '(keymap))");
        LispObject parent = evaluateString("(keymap-parent k1)");
        Assert.assertEquals(LispList.list(new LispSymbol("keymap")), parent);
        parent = evaluateString("(keymap-parent k2)");
        Assert.assertEquals(LispSymbol.ourNil, parent);
    }

    @Test
    public void testKeymapParent() throws Exception {
        evaluateString("(setq k (make-sparse-keymap))");
        LispObject r = evaluateString("(keymap-parent k)");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }

    @Test
    public void testSetKeymapParentSparse() throws Exception {
        evaluateString("(setq k (make-sparse-keymap))");
        LispObject r = evaluateString("(set-keymap-parent k '(keymap))");
        Assert.assertEquals(LispList.list(new LispSymbol("keymap")), r);
        r = evaluateString("(keymap-parent k)");
        Assert.assertEquals(LispList.list(new LispSymbol("keymap")), r);
        r = evaluateString("k");
        Assert.assertEquals(LispList.list(new LispSymbol("keymap"), new LispSymbol("keymap")), r);
    }

    @Test
    public void testSetKeymapParent() throws Exception {
        evaluateString("(setq k (make-keymap))");
        LispObject r = evaluateString("(set-keymap-parent k '(keymap))");
        Assert.assertEquals(LispList.list(new LispSymbol("keymap")), r);
        r = evaluateString("(keymap-parent k)");
        Assert.assertEquals(LispList.list(new LispSymbol("keymap")), r);
        r = evaluateString("k");
        String expected = "(keymap #^[nil nil keymap nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] keymap)";
        Assert.assertEquals(expected, r.toString());
    }

    @Test
    public void testSetKeymapParentAsSymbol () {
        evaluateString("(defvar parent 1)");
        evaluateString("(fset 'parent (make-sparse-keymap))");
        evaluateString("(defvar km (make-sparse-keymap))");
        evaluateString("(set-keymap-parent km 'parent)");
        LispObject km = evaluateString("km");
        System.out.println(km.toString());
        Assert.assertEquals(LispList.list(new LispSymbol("keymap"), new LispSymbol("keymap")), km);
    }
    
//    @Test
//    public void testClLoopLet() {
//        evaluateString("(setq loop-for-sets '((ch (aref --cl-vec-- --cl-idx--))))");
//        evaluateString("(setq arg0 (nreverse loop-for-sets))");
//        evaluateString("(setq arg1 '(quote setq))");
//        evaluateString("(setq arg2 nil)");
//        GlobalEnvironment.INSTANCE.findAndRegisterEmacsForm("cl-loop-let", "/lisp/emacs-lisp/cl.el", GlobalEnvironment.SymbolType.FUN);
//        LispObject r = evaluateString("(cl-loop-let arg0 arg1 arg2)");
//        Assert.assertEquals("(#<subr let*> ((ch (aref --cl-vec-- --cl-idx--))) quote setq)", r.toString());
//    }

//    @Ignore
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
    public void testEventConvertListWords() {
        LispObject r = evaluateString("(event-convert-list '(control meta ?a))");
        Assert.assertEquals(new LispInteger(134217729), r);
        r = evaluateString("(event-convert-list '(control super f1))");
        Assert.assertEquals(new LispSymbol("C-s-f1"), r);
    }

    @Test
    public void testEventConvertListWrong() {
        try {
            evaluateString("(event-convert-list '(C ?s ?D ?g))");
        } catch (Exception e) {
            Assert.assertEquals("(error \"Two bases given in one event\")", org.jetbrains.emacs4ij.jelisp.TestSetup.getCause(e).getMessage());
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
    public void testDefineKeySimple() {
        evaluateString("(defvar km (make-sparse-keymap))");
        LispObject r = evaluateString("(define-key km \"d\" 'forward-char)");
        Assert.assertEquals(new LispSymbol("forward-char"), r);
        LispObject keyMap = evaluateString("km");
        Assert.assertEquals("(keymap (100 . #<subr forward-char>))", keyMap.toString());
    }

    @Test
    public void testDefineKeySimple2() {
        evaluateString("(defvar km (make-sparse-keymap))");
        LispObject r = evaluateString("(define-key km \"Cd\" 'forward-char)");
        Assert.assertEquals(new LispSymbol("forward-char"), r);
        LispObject keyMap = evaluateString("km");
        Assert.assertEquals("(keymap (67 keymap (100 . #<subr forward-char>)))", keyMap.toString());
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
    public void testDefineKeyWrong() {
        try {
            evaluateString("(define-key (current-global-map) \"C-a\" 'backward-char)");
        } catch (Exception e) {
            Assert.assertEquals("(error \"Key sequence C - a starts with non-prefix key C\")", org.jetbrains.emacs4ij.jelisp.TestSetup.getCause(e).getMessage());
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
        LispObject keymap = evaluateString("mapvar");
        Assert.assertEquals(((LispSymbol) cmd).getFunction(), keymap);
        LispKeymap km = BuiltinsKey.makeSparseKeymap(new LispSymbol("name"));
        Assert.assertEquals(km, keymap);
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
    public void testKeymapType() {
        Assert.assertEquals(LispSymbol.ourT, BuiltinsList.consp(BuiltinsKey.makeSparseKeymap(new LispSymbol("name"))));
    }

}
