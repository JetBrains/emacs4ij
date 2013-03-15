package org.jetbrains.emacs4ij.ide;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispKeymap;
import org.jetbrains.emacs4ij.jelisp.subroutine.Key;

public class KeymapTest extends IdeTestCase {
  public void testKeymapP() throws Exception {
    evaluateString("(setq k (make-sparse-keymap))");
    Assert.assertEquals(LispSymbol.T, evaluateString("(keymapp k)"));
    evaluateString("(setq m 1)");
    evaluateString("(fset 'm (make-sparse-keymap))");
    Assert.assertEquals(LispSymbol.T, evaluateString("(keymapp 'm)"));
  }

  public void testMakeKeymap() throws Exception {
    LispObject r = evaluateString("(make-keymap 'a)");
    Assert.assertEquals("a", r.toString());
  }

  public void testCopyKeymap() throws Exception {
    LispSymbol k1 = (LispSymbol) evaluateString("(defvar k1 (make-keymap))");
    LispSymbol k2 = (LispSymbol) evaluateString("(defvar k2 (copy-keymap k1))");
    Assert.assertEquals(k1.getValue(), k2.getValue());
    evaluateString("(set-keymap-parent k1 (make-sparse-keymap))");
    LispObject parent = evaluateString("(keymap-parent k1)");
    Assert.assertEquals(Key.makeSparseKeymap(null), parent);
    parent = evaluateString("(keymap-parent k2)");
    Assert.assertEquals(LispSymbol.NIL, parent);
  }

  public void testCopyKeymapAsSymbolFunction() throws Exception {
    evaluateString("(defvar k1 1)");
    evaluateString("(fset 'k1 (make-keymap))");
    LispSymbol k1 = (LispSymbol) evaluateString("'k1");
    LispSymbol k2 = (LispSymbol) evaluateString("(defvar k2 (copy-keymap 'k1))");
    Assert.assertEquals(k1.getFunction(), k2.getValue());
  }

  public void testKeymapParent() throws Exception {
    evaluateString("(setq k (make-sparse-keymap))");
    LispObject r = evaluateString("(keymap-parent k)");
    Assert.assertEquals(LispSymbol.NIL, r);

    evaluateString("(setq k (make-keymap))");
    r = evaluateString("(keymap-parent k)");
    Assert.assertEquals(LispSymbol.NIL, r);
  }

  public void testSetKeymapParentSparseTwice() throws Exception {
    evaluateString("(setq k (make-sparse-keymap))");
    evaluateString("(set-keymap-parent k (make-sparse-keymap))");
    LispKeymap sparseKeymap = (LispKeymap) evaluateString("(set-keymap-parent k (make-sparse-keymap))");
    LispKeymap keymap = (LispKeymap) evaluateString("k");
    Assert.assertEquals(sparseKeymap, keymap.getParent());
  }

  public void testSetKeymapParentSparse() throws Exception {
    evaluateString("(setq k (make-sparse-keymap))");
    LispObject parent = evaluateString("(set-keymap-parent k (make-sparse-keymap))");
    LispObject r = evaluateString("(keymap-parent k)");
    Assert.assertEquals(parent, r);
  }

  public void testSetKeymapParentAsSymbol () {
    evaluateString("(defvar parent 1)");
    LispObject parent = evaluateString("(fset 'parent (make-sparse-keymap))");
    evaluateString("(defvar km (make-sparse-keymap))");
    evaluateString("(set-keymap-parent km 'parent)");
    LispKeymap keymap = (LispKeymap) evaluateString("km");
    Assert.assertEquals(parent, keymap.getParent());
  }

  public void testDefineCtrlKey() {
    evaluateString("(setq p (make-sparse-keymap))");
    evaluateString("(define-key p \"\\C-d\" 'a)");
    LispKeymap keymap = (LispKeymap) evaluateString("p");
    Assert.assertEquals(new LispSymbol("a"), keymap.getKeyBinding(new LispString("\\C-d")));
  }

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

  public void testCyclicParents() {
    evaluateString("(setq k (make-sparse-keymap))");
    evaluateString("(setq p1 (make-sparse-keymap))");
    evaluateString("(setq p2 (make-sparse-keymap))");
    evaluateString("(set-keymap-parent k p1)");
    evaluateString("(set-keymap-parent p1 p2)");
    try {
      evaluateString("(set-keymap-parent p2 k)");
    } catch (Exception e) {
      Assert.assertEquals("(error \"Cyclic keymap inheritance\")", getCauseMsg(e));
      return;
    }
    Assert.fail();
  }

  public void testCyclicParentsSelf() {
    evaluateString("(setq k (make-sparse-keymap))");
    try {
      evaluateString("(set-keymap-parent k k)");
    } catch (Exception e) {
      Assert.assertEquals("(error \"Cyclic keymap inheritance\")", getCauseMsg(e));
      return;
    }
    Assert.fail();
  }

  public void testCyclicParentsCdr() {
    evaluateString("(setq k (make-sparse-keymap))");
    evaluateString("(setq p1 (make-sparse-keymap))");
    evaluateString("(setq p2 (make-sparse-keymap))");
    evaluateString("(set-keymap-parent k p1)");
    evaluateString("(set-keymap-parent p1 p2)");
    try {
      evaluateString("(set-keymap-parent p1 k)");
    } catch (Exception e) {
      Assert.assertEquals("(error \"Cyclic keymap inheritance\")", getCauseMsg(e));
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
  public void testKbdMacro() {
    LispObject r = evaluateString("(kbd \"C-x\")");
    System.out.println(r.toString());
//        Assert.assertEquals(new LispString("^X"), r);
    r = evaluateString("(kbd \"\\C-x\")");
    System.out.println(r.toString());
//        Assert.assertEquals(new LispString("^X"), r);
    r = evaluateString("(kbd \"\\C-x\\t\")"); //
    System.out.println(r.toString());
//        Assert.assertEquals(new LispString("^X"), r);
    r = evaluateString("(kbd \"^x\")");
    System.out.println(r.toString());
//        Assert.assertEquals(new LispString("^X"), r);
//        Assert.assertEquals(1, ((LispSequence)r).length());


    r = evaluateString("(kbd \"C-x\")"); // ⇒ "\C-x"
    System.out.println("\\C-x" + " =?= " + r.toString());
    r = evaluateString("(kbd \"C-x C-f\")"); // ⇒ "\C-x\C-f"
    System.out.println("\\C-x\\C-f" + " =?= " + r.toString());
    r = evaluateString("(kbd \"C-x 4 C-f\")"); // ⇒ "\C-x4\C-f"
    System.out.println("\\C-x4\\C-f" + " =?= " + r.toString());
    r = evaluateString("(kbd \"X\")"); // ⇒ "X"
    System.out.println("X" + " =?= " + r.toString());
    r = evaluateString("(kbd \"RET\")"); // ⇒ "\^M"
    System.out.println("\\^M" + " =?= " + r.toString());
    r = evaluateString("(kbd \"C-c SPC\")"); // ⇒ "\C-c "
    System.out.println("\\C-c " + " =?= " + r.toString());
    r = evaluateString("(kbd \"<f1> SPC\")"); // ⇒ [f1 32]
    System.out.println("[f1 32]" + " =?= " + r.toString());
    r = evaluateString("(kbd \"C-M-<down>\")"); // ⇒ [C-M-down]
    System.out.println("[C-M-down]" + " =?= " + r.toString());

  }

  public void testCurrentGlobalKeyMap() {
    System.out.println(evaluateString("(current-global-map)").toString());
  }

//    public void testEventConvertListNil() {
//        LispObject r = evaluateString("(event-convert-list \"C-x\")");
//        Assert.assertEquals(LispSymbol.NIL, r);
//        r = evaluateString("(event-convert-list 5)");
//        Assert.assertEquals(LispSymbol.NIL, r);
//    }
//
//    public void testEventConvertList() {
//        LispObject r = evaluateString("(event-convert-list '(C ?s))");
//        Assert.assertEquals(new LispInteger(19), r);
//        r = evaluateString("(event-convert-list '(C M S alt ?s))");
//        Assert.assertEquals(new LispInteger(171966483), r);
//    }
//
//    public void testEventConvertListWords() {
//        LispObject r = evaluateString("(event-convert-list '(control meta ?a))");
//        Assert.assertEquals(new LispInteger(134217729), r);
//        r = evaluateString("(event-convert-list '(control super f1))");
//        Assert.assertEquals(new LispSymbol("C-s-f1"), r);
//    }
//
//    public void testEventConvertListWrong() {
//        try {
//            evaluateString("(event-convert-list '(C ?s ?D ?g))");
//        } catch (Exception e) {
//            Assert.assertEquals("(error \"Two bases given in one event\")", TestSetup.getCause(e));
//            return;
//        }
//        Assert.fail();
//    }

//    //todo implement single-key-description
//    public void testKeyDescription() {
//        LispObject r = evaluateString("(key-description '[1 2 3] '[6 7])");
//        Assert.assertEquals(new LispString("C-f C-g C-a C-b C-c"), r);
//    }

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

  public void testDefineKeySimple() {
    evaluateString("(defvar km (make-sparse-keymap))");
    LispObject r = evaluateString("(define-key km \"d\" 'forward-char)");
    Assert.assertEquals(new LispSymbol("forward-char"), r);
    Assert.assertEquals(new LispSymbol("forward-char"), evaluateString("(lookup-key km \"d\")"));
  }

  public void testDefineKeyInKeyMap() {
    evaluateString("(defvar km (make-keymap))");
    evaluateString("(define-key km \"\\C-d\" 'a)");
    LispObject a = evaluateString("(lookup-key km \"\\C-d\")");
    Assert.assertEquals(new LispSymbol("a"), a);
  }

  public void testDefineKeyWrong() {
    Assert.assertEquals(new LispSymbol("a"), evaluateString("(define-key (current-global-map) \"c-a\" 'a)"));
    IdeaKeymap global = (IdeaKeymap) evaluateString("global-map");
    IdeaKeymap keymap1 = (IdeaKeymap) global.getKeyBinding(new LispString("c"));
    IdeaKeymap keymap2 = (IdeaKeymap) keymap1.getKeyBinding(new LispString("-"));
    Assert.assertEquals (new LispSymbol("a"), keymap2.getKeyBinding(new LispString("a")));
  }

  //todo: fix "K character not implemented"
  public void testGlobalSetKey() {
    LispObject r = evaluateString("(global-set-key \"\\C-q\" 'forward-char)");
    Assert.assertEquals(new LispSymbol("forward-char"), r);
  }

  public void testGlobalMap() {
    LispObject r = evaluateString("global-map");
    System.out.println(r.toString());
    r = evaluateString("ctl-x-map");
    System.out.println(r.toString());
  }

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

  public void testDefinePrefixCommandNoOpt () {
    evaluateString("(setq cmd 1)");
    LispObject cmd = evaluateString("(define-prefix-command 'cmd)");
    LispKeymap km = Key.makeSparseKeymap(null);
    Assert.assertTrue(cmd instanceof LispSymbol);
    Assert.assertEquals(km, ((LispSymbol) cmd).getValue());
    Assert.assertEquals(km, ((LispSymbol) cmd).getFunction());
  }

  public void testPrefixKeymap() {
    evaluateString("(setq km (make-sparse-keymap))");
    evaluateString("(setq p (make-sparse-keymap))");
    evaluateString("(define-key p \"\\M-g\" 'a)");
    evaluateString("(define-key km \"\\C-q\" p)");
    LispObject a = evaluateString("(lookup-key km \"\\C-q\\M-g\")");
    Assert.assertEquals(a, new LispSymbol("a"));
  }

  public void testPrefixKeymapSymbol() {
    evaluateString("(setq km (make-sparse-keymap))");
    evaluateString("(fset 'p (make-sparse-keymap))");
    evaluateString("(define-key 'p \"\\M-g\" 'a)");
    evaluateString("(define-key km \"\\C-q\" 'p)");
    LispObject a = evaluateString("(lookup-key km \"\\C-q\\M-g\")");
    Assert.assertEquals(a, new LispSymbol("a"));
  }

  public void testSymbolFunctionSymbolKeymap() {
    evaluateString("(setq p1 1)");
    evaluateString("(fset 'p1 (make-sparse-keymap))");
    evaluateString("(setq p2 2)");
    evaluateString("(fset 'p2 (make-sparse-keymap))");
    evaluateString("(setq pp 'p1)");
    evaluateString("(fset 'pp 'p2)");
    evaluateString("(defvar k (make-sparse-keymap))");
    evaluateString("(define-key k \"s\" 'pp)");
    evaluateString("(define-key k \"sk\" 'f)");
    LispObject f1 = evaluateString("(symbol-function 'p1)"); // empty
    LispKeymap expected = Key.makeSparseKeymap(null);
    Assert.assertEquals(expected, f1);
    expected.defineKey(new LispSymbol("f"), new LispString("k"));
    LispObject f2 = evaluateString("(symbol-function 'p2)"); //set
    Assert.assertEquals(expected, f2);
    LispSymbol pp = (LispSymbol) evaluateString("'pp");
    LispKeymap k = (LispKeymap) evaluateString("k");
    Assert.assertEquals(pp, k.getKeyBinding(new LispString("s")));
  }

  public void testAnotherEscShortcut() {
    evaluateString("(setq map (make-sparse-keymap))");
    Assert.assertEquals(new LispSymbol("indent-sexp"), evaluateString("(define-key map \"\\e\\C-q\" 'indent-sexp)"));
    LispKeymap keymap = (LispKeymap) evaluateString("map");
    Assert.assertEquals(new LispSymbol("indent-sexp"), keymap.getKeyBinding(new LispString("\\e\\C-q")));
  }

  public void testDefineListKey() {
    evaluateString("(setq km1 (make-sparse-keymap))");
    evaluateString("(define-key km1 \"\\C-a\" 'a)");
    evaluateString("(setq km2 (make-sparse-keymap))");
    evaluateString("(define-key km2 \"\\C-b\" '(km1 . \"\\C-a\"))");
    Assert.assertEquals(LispList.cons(new LispSymbol("km1"), new LispString("\\C-a")),
        evaluateString("(lookup-key km2 \"\\C-b\")"));
  }

  public void testDefineListStringFirstKey() {
    evaluateString("(setq km1 (make-sparse-keymap))");
    evaluateString("(define-key km1 \"\\C-a\" '(\"anna\" 1 2))");
    Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(2)),
        evaluateString("(lookup-key km1 \"\\C-a\")"));
  }

  /* todo
 (setq km (make-sparse-keymap))
 (define-key km "\C-b" '((keymap (97 . a)) . 97))
 (lookup-key km "\C-b") => returns a
  */
}