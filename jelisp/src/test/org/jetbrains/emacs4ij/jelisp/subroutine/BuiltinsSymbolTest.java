package org.jetbrains.emacs4ij.jelisp.subroutine;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.TestSetup;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 9/26/11
 * Time: 4:39 PM
 * To change this template use File | Settings | File Templates.
 */
public class BuiltinsSymbolTest extends BaseSubroutineTest {
    @Test
    public void testSymbolFunction () {
        LispObject lispObject = evaluateString("(symbol-function '+)");
        Assert.assertEquals(new LispString("#<subr +>"), new LispString(lispObject.toString()));
    }

    @Test (expected = VoidVariableException.class)
    public void testSymbolFunctionVoidVar() {
        evaluateString("(symbol-function a)");
    }

    @Test
    public void testSymbolFunctionVoidFunNonExistentSymbol() {
        try {
            evaluateString("(symbol-function 'a)");
        } catch (Exception e) {
            Assert.assertEquals("'(void-function a)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testSymbolFunctionVoidFunExistingSymbol() {
        try {
            evaluateString("(defvar a 10)");
            evaluateString("(symbol-function 'a)");
        } catch (Exception e) {
            Assert.assertEquals("'(void-function a)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test (expected = WrongTypeArgumentException.class)
    public void testSymbolFunctionWrongArg() {
        evaluateString("(symbol-function 5)");
    }

    @Test
    public void testGet() throws Exception {
        evaluateString("(set 'a 5)");
        LispObject LispObject = evaluateString("(get 'a 'p2)");
        Assert.assertEquals(LispSymbol.ourNil, LispObject);
    }

    @Test
    public void testPut() throws Exception {
        evaluateString("(set 'a 5)");
        evaluateString("(put 'a 'p1 'v1)");
        LispObject lispObject = evaluateString("(get 'a 'p1)");
        Assert.assertEquals(new LispSymbol("v1"), lispObject);
    }

    @Test
    public void testSymbolValueInteger() {
        evaluateString("(set 'a 5)");
        LispObject lispObject = evaluateString("(symbol-value 'a)");
        Assert.assertEquals(new LispInteger(5), lispObject);
    }

    @Test
    public void testSymbolValueVoid() {
        try {
            evaluateString("(symbol-value 'a)");
        } catch (Exception e) {
            Assert.assertEquals("'(void-variable a)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testDocumentationProperty_MyVar () {
        evaluateString("(defvar a 1 \"doc\")");
        LispObject doc = evaluateString("(documentation-property 'a 'variable-documentation)");
        Assert.assertEquals(new LispString("doc"), doc);
    }

    @Test
    public void testDocumentationProperty_GlobalVar () {
        LispObject doc = evaluateString("(documentation-property 'load-history 'variable-documentation)");
        LispString trueDoc = new LispString("Alist mapping loaded file names to symbols and features.\n" +
                "Each alist element should be a list (FILE-NAME ENTRIES...), where\n" +
                "FILE-NAME is the name of a file that has been loaded into Emacs.\n" +
                "The file name is absolute and true (i.e. it doesn't contain symlinks).\n" +
                "As an exception, one of the alist elements may have FILE-NAME nil,\n" +
                "for symbols and features not associated with any file.\n" +
                "\n" +
                "The remaining ENTRIES in the alist element describe the functions and\n" +
                "variables defined in that file, the features provided, and the\n" +
                "features required.  Each entry has the form `(provide . FEATURE)',\n" +
                "`(require . FEATURE)', `(defun . FUNCTION)', `(autoload . SYMBOL)',\n" +
                "`(defface . SYMBOL)', or `(t . SYMBOL)'.  In addition, an entry `(t\n" +
                ". SYMBOL)' may precede an entry `(defun . FUNCTION)', and means that\n" +
                "SYMBOL was an autoload before this file redefined it as a function.\n" +
                "\n" +
                "During preloading, the file name recorded is relative to the main Lisp\n" +
                "directory.  These file names are converted to absolute at startup.");

        Assert.assertEquals(trueDoc, doc);
    }

    @Test
    public void testDocumentationProperty_GlobalVar1 () {
        LispObject doc = evaluateString("(documentation-property 'current-load-list 'variable-documentation)");
        LispString trueDoc = new LispString("Used for internal purposes by `load'.");
        Assert.assertEquals(trueDoc, doc);
    }

    @Test
    public void testDocumentationProperty_GlobalVar2 () {
        evaluateString("global-mark-ring");
        LispObject doc = evaluateString("(documentation-property 'global-mark-ring 'variable-documentation)");
        LispString trueDoc = new LispString("The list of saved global marks, most recent first.");
        Assert.assertEquals(trueDoc, doc);
    }

    @Test
    public void testDocumentationProperty_GlobalVar3 () {
        evaluateString("global-mark-ring-max");
        LispObject doc = evaluateString("(documentation-property 'global-mark-ring-max 'variable-documentation)");
        LispString trueDoc = new LispString("Maximum size of global mark ring.  \\\nStart discarding off end if gets this big.");
        Assert.assertEquals(trueDoc, doc);
    }

    @Test
    public void testDocumentationProperty_GlobalVar4 () {
        LispObject doc = evaluateString("(documentation-property 'executing-kbd-macro 'variable-documentation)");
        LispString trueDoc = new LispString("Currently executing keyboard macro (string or vector).\n"+
                "This is nil when not executing a keyboard macro.");
        Assert.assertEquals(trueDoc, doc);
    }

    @Test
    public void testDocumentationProperty_Negative () {
        evaluateString("transient-mark-mode");
        LispObject doc = evaluateString("(documentation-property 'transient-mark-mode 'variable-documentation)");
        String trueDoc = "*Non-nil if Transient Mark mode is enabled.\n" +
                "See the command `transient-mark-mode' for a description of this minor mode.\n" +
                "\n" +
                "Non-nil also enables highlighting of the region whenever the mark is active.\n" +
                "The variable `highlight-nonselected-windows' controls whether to highlight\n" +
                "all windows or just the selected window.";
        Assert.assertTrue(doc instanceof LispString);
        Assert.assertTrue(((LispString)doc).getData().contains(trueDoc));
    }

    @Test
    public void testDocumentation_Builtin() {
        LispObject doc = evaluateString("(documentation-property 'if 'function-documentation)");
        Assert.assertEquals(LispSymbol.ourNil, doc);
        doc = evaluateString("(get 'if 'function-documentation)");
        Assert.assertEquals(LispSymbol.ourNil, doc);
        doc = evaluateString("(documentation 'if)");
        LispString trueDoc = new LispString("If COND yields non-nil, do THEN, else do ELSE...\n" +
                "Returns the value of THEN or the value of the last of the ELSE's.\n" +
                "THEN must be one expression, but ELSE... can be zero or more expressions.\n" +
                "If COND yields nil, and there are no ELSE's, the value is nil.\n" +
                "\n" +
                "(fn COND THEN ELSE...)");

        Assert.assertEquals(trueDoc, doc);

        doc = evaluateString("(documentation (symbol-function 'if))");
        Assert.assertEquals(trueDoc, doc);
    }

    @Test
    public void testDocumentation_Macro () {
        evaluateString("(defmacro m () \"doc\")");
        LispObject doc = evaluateString("(documentation 'm)");
        Assert.assertEquals(new LispString("doc"), doc);
        doc = evaluateString("(documentation (symbol-function 'm))");
        Assert.assertEquals(new LispString("doc"), doc);
    }

    @Test
    public void testDocumentation_CustomFun() {
        evaluateString("(defun f () \"doc\")");
        LispObject doc = evaluateString("(documentation-property 'f 'function-documentation)");
        Assert.assertEquals(LispSymbol.ourNil, doc);
        doc = evaluateString("(get 'f 'function-documentation)");
        Assert.assertEquals(LispSymbol.ourNil, doc);
        doc = evaluateString("(documentation 'f)");
        Assert.assertEquals(new LispString("doc"), doc);
    }

    @Test
    public void testResetLambdaDocumentation() {
        evaluateString("(defun a () \"doc1\")");
        LispObject doc = evaluateString("(documentation 'a)");
        org.junit.Assert.assertEquals(new LispString("doc1"), doc);
        evaluateString("(put 'a 'function-documentation \"doc2\")");
        doc = evaluateString("(documentation 'a)");
        org.junit.Assert.assertEquals(new LispString("doc2"), doc);

        //fun staff:
        LispObject f = evaluateString("(symbol-function 'a)");
        Assert.assertEquals("(lambda nil \"doc1\")", f.toString());
    }

    @Test
    public void testFunctionDocumentationNil () {
        evaluateString("(defun a () \"doc\" 2)");
        LispObject doc = evaluateString("(documentation-property 'a 'function-documentation)");
        Assert.assertEquals(LispSymbol.ourNil, doc);
    }

    @Test
    public void testDocumentationNoDef () {
        try {
            evaluateString("(documentation 'a)");
        } catch (Exception e) {
            Assert.assertEquals("'(void-function a)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testDocumentationString () {
        evaluateString("(defun a () \"doc\" 2)");
        LispObject doc = evaluateString("(documentation 'a)");
        org.junit.Assert.assertEquals(new LispString("doc"), doc);
    }

    @Test
    public void testDocumentationProperty () {
        evaluateString("(defun a () 2)");
        evaluateString("(put 'a 'function-documentation \"doc\")");
        LispObject doc = evaluateString("(documentation 'a)");
        org.junit.Assert.assertEquals(new LispString("doc"), doc);
        doc = evaluateString("(documentation-property 'a 'function-documentation)");
        org.junit.Assert.assertEquals(new LispString("doc"), doc);
    }

    @Test
    public void testDocumentationPropertyFun () {
        evaluateString("(defvar a 2)");
        evaluateString("(put 'a 'function-documentation \"doc\")");
        LispObject doc = evaluateString("(documentation 'a)");
        org.junit.Assert.assertEquals(new LispString("doc"), doc);
    }

    @Test
    public void testDefaultValueDefvar () {
        evaluateString("(defvar a 1)");
        LispObject r = evaluateString("(default-value 'a)");
        Assert.assertEquals(new LispInteger(1), r);
    }

    @Test
    public void testDefaultValueSetq () {
        evaluateString("(setq a 1)");
        LispObject r = evaluateString("(default-value 'a)");
        Assert.assertEquals(new LispInteger(1), r);
    }

    @Test
    public void testSetDefault() {
        LispObject r = evaluateString("(set-default 'a 1)");
        Assert.assertEquals(new LispInteger(1), r);
        r = evaluateString("(default-value 'a)");
        Assert.assertEquals(new LispInteger(1), r);
        r = evaluateString("a");
        Assert.assertEquals(new LispInteger(1), r);
    }

    @Test
    public void testSetDefaultIndirect() {
        LispObject r = evaluateString("(set-default (car '(a b c)) 1)");
        Assert.assertEquals(new LispInteger(1), r);
        r = evaluateString("(default-value 'a)");
        Assert.assertEquals(new LispInteger(1), r);
        r = evaluateString("a");
        Assert.assertEquals(new LispInteger(1), r);
    }

    @Test
    public void testPropertyForUndefinedSymbol() {
        evaluateString("(put 'q 'prop 1)");
        LispObject r = evaluateString("(get 'q 'prop)");
        Assert.assertEquals(new LispInteger(1), r);
        try {
            evaluateString("q");
        } catch (Exception e) {
            Assert.assertEquals("'(void-variable q)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testCustomInitReset() {
        evaluateString("(put 'q 'prop 1)");
        LispObject r = evaluateString("(get 'q 'prop)");
        Assert.assertEquals(new LispInteger(1), r);
        evaluateString("(custom-initialize-reset 'q 2)");
        r = evaluateString("q");
        Assert.assertEquals(new LispInteger(2), r);
        r = evaluateString("(get 'q 'prop)");
        Assert.assertEquals(new LispInteger(1), r);
    }

    @Test
    public void testDefCustom() {
        LispObject r = evaluateString("(defcustom a 5 \"doc\")");
        LispObject b = evaluateString("(documentation-property 'a 'variable-documentation)");
        Assert.assertEquals(new LispString("doc"), b);
    }

    @Test
    public void testCustomDeclareVariable() {
        evaluateString("(custom-declare-variable 'b 10 \"doc\")");
        LispObject b = evaluateString("b");
        Assert.assertNotNull(b);
        b = evaluateString("(documentation-property 'b 'variable-documentation)");
        Assert.assertEquals(new LispString("doc"), b);
    }

    @Test
    public void testInteractiveForm() {
        LispObject r = evaluateString("(interactive-form 5)");
        Assert.assertEquals(LispSymbol.ourNil, r);
        r = evaluateString("(interactive-form 'forward-char)");
        Assert.assertEquals(LispList.list(new LispSymbol("interactive"), new LispString("")), r);
        r = evaluateString("(interactive-form 'switch-to-buffer)");
        Assert.assertEquals(LispList.list(new LispSymbol("interactive"), new LispString("BSwitch to buffer")), r);
        r = evaluateString("(interactive-form '(lambda () (+ 6 3) (interactive \"f\")))");
        Assert.assertEquals(LispList.list(new LispSymbol("interactive"), new LispString("f")), r);
    }
}
