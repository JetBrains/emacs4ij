package org.jetbrains.emacs4ij.jelisp.subroutine;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.Parser;
import org.jetbrains.emacs4ij.jelisp.elisp.LObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 9/26/11
 * Time: 4:39 PM
 * To change this template use File | Settings | File Templates.
 */
public class BuiltinsSymbolTest {

    private Environment environment;

    @BeforeClass
    public static void runBeforeClass() {
        GlobalEnvironment.ourEmacsSource = "/home/kate/Downloads/emacs 23.2a/emacs-23.2";
        GlobalEnvironment.ourEmacsPath = "/usr/share/emacs/23.2";
        GlobalEnvironment.initialize(null, null);
        GlobalEnvironment.getInstance().startRecording();
    }

    @Before
    public void setUp() throws Exception {
        GlobalEnvironment.getInstance().clearRecorded();
        environment = new Environment(GlobalEnvironment.getInstance());
    }

    private LObject evaluateString (String lispCode) throws LispException {
        Parser parser = new Parser();
        return parser.parseLine(lispCode).evaluate(environment);
    }

    private Throwable getCause (Throwable e) {
        if (e.getCause() == null)
            return e;
        return getCause(e.getCause());
    }

    @Test
    public void testSymbolFunction () {
        LObject lispObject = evaluateString("(symbol-function '+)");
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
            Throwable q = getCause(e);
            if (!(q instanceof VoidFunctionException))
                org.junit.Assert.fail(q.getLocalizedMessage());
        }
    }

    @Test
    public void testSymbolFunctionVoidFunExistingSymbol() {
        try {
            evaluateString("(defvar a 10)");
            evaluateString("(symbol-function 'a)");
        } catch (Exception e) {
            Throwable q = getCause(e);
            if (!(q instanceof VoidFunctionException))
                org.junit.Assert.fail(q.getLocalizedMessage());
        }
    }

    @Test (expected = WrongTypeArgumentException.class)
    public void testSymbolFunctionWrongArg() {
        evaluateString("(symbol-function 5)");
    }

    @Test
    public void testGet() throws Exception {
        evaluateString("(set 'a 5)");
        LObject LObject = evaluateString("(get 'a 'p2)");
        Assert.assertEquals(LispSymbol.ourNil, LObject);
    }

    @Test
    public void testPut() throws Exception {
        evaluateString("(set 'a 5)");
        evaluateString("(put 'a 'p1 'v1)");
        LObject lispObject = evaluateString("(get 'a 'p1)");
        Assert.assertEquals(new LispSymbol("v1"), lispObject);
    }

    @Test
    public void testSymbolValueInteger() {
        evaluateString("(set 'a 5)");
        LObject lispObject = evaluateString("(symbol-value 'a)");
        Assert.assertEquals(new LispInteger(5), lispObject);
    }

    @Test
    public void testSymbolValueVoid() {
        try {
            evaluateString("(symbol-value 'a)");
        } catch (Exception e) {
            Assert.assertTrue(getCause(e) instanceof VoidVariableException);
            return;
        }
        Assert.assertTrue(false);
    }

    @Test
    public void testDocumentationProperty_MyVar () {
        evaluateString("(defvar a 1 \"doc\")");
        LObject doc = evaluateString("(documentation-property 'a 'variable-documentation)");
        Assert.assertEquals(new LispString("doc"), doc);
    }

    @Test
    public void testDocumentationProperty_GlobalVar () {
        LObject doc = evaluateString("(documentation-property 'load-history 'variable-documentation)");
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
        doc = evaluateString("(get 'load-history 'variable-documentation)");
        Assert.assertEquals(new LispInteger(550505), doc);
    }

    @Test
    public void testDocumentationProperty_Negative () {
        LObject doc = evaluateString("(documentation-property 'transient-mark-mode 'variable-documentation)");
        LObject trueDoc = new LispString("*Non-nil if Transient Mark mode is enabled.\n" +
                "See the command `transient-mark-mode' for a description of this minor mode.\n" +
                "\n" +
                "Non-nil also enables highlighting of the region whenever the mark is active.\n" +
                "The variable `highlight-nonselected-windows' controls whether to highlight\n" +
                "all windows or just the selected window.\n" +
                "\n" +
                "If the value is `lambda', that enables Transient Mark mode temporarily.\n" +
                "After any subsequent action that would normally deactivate the mark\n" +
                "(such as buffer modification), Transient Mark mode is turned off.\n" +
                "\n" +
                "If the value is (only . OLDVAL), that enables Transient Mark mode\n" +
                "temporarily.  After any subsequent point motion command that is not\n" +
                "shift-translated, or any other action that would normally deactivate\n" +
                "the mark (such as buffer modification), the value of\n" +
                "`transient-mark-mode' is set to OLDVAL.");
        Assert.assertEquals(trueDoc, doc);
        doc = evaluateString("(get 'transient-mark-mode 'variable-documentation)");
        Assert.assertEquals(new LispInteger(-2109012), doc);
    }

    @Test
    public void testDocumentation_Builtin() {
        LObject doc = evaluateString("(documentation-property 'if 'function-documentation)");
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
        LObject doc = evaluateString("(documentation 'm)");
        Assert.assertEquals(new LispString("doc"), doc);
        doc = evaluateString("(documentation (symbol-function 'm))");
        Assert.assertEquals(new LispString("doc"), doc);
    }

    @Test
    public void testDocumentation_CustomFun() {
        evaluateString("(defun f () \"doc\")");
        LObject doc = evaluateString("(documentation-property 'f 'function-documentation)");
        Assert.assertEquals(LispSymbol.ourNil, doc);
        doc = evaluateString("(get 'f 'function-documentation)");
        Assert.assertEquals(LispSymbol.ourNil, doc);
        doc = evaluateString("(documentation 'f)");
        Assert.assertEquals(new LispString("doc"), doc);
    }

       @Test
    public void testResetLambdaDocumentation() {
        evaluateString("(defun a () \"doc1\")");
        LObject doc = evaluateString("(documentation 'a)");
        org.junit.Assert.assertEquals(new LispString("doc1"), doc);
        evaluateString("(put 'a 'function-documentation \"doc2\")");
        doc = evaluateString("(documentation 'a)");
        org.junit.Assert.assertEquals(new LispString("doc2"), doc);

        //fun staff:
        LObject f = evaluateString("(symbol-function 'a)");
        org.junit.Assert.assertEquals("(lambda nil \"doc1\")", f.toString());
    }

    @Test
       public void testFunctionDocumentationNil () {
           evaluateString("(defun a () \"doc\" 2)");
           LObject doc = evaluateString("(documentation-property 'a 'function-documentation)");
           org.junit.Assert.assertEquals(LispSymbol.ourNil, doc);
       }

       @Test
       public void testDocumentationNoDef () {
           try {
               evaluateString("(documentation 'a)");
           } catch (Exception e) {
               Throwable q = getCause(e);
               if (!(q instanceof VoidFunctionException))
                   org.junit.Assert.fail(q.getLocalizedMessage());
           }
       }

       @Test
       public void testDocumentationString () {
           evaluateString("(defun a () \"doc\" 2)");
           LObject doc = evaluateString("(documentation 'a)");
           org.junit.Assert.assertEquals(new LispString("doc"), doc);
       }

       @Test
       public void testDocumentationProperty () {
           evaluateString("(defun a () 2)");
           evaluateString("(put 'a 'function-documentation \"doc\")");
           LObject doc = evaluateString("(documentation 'a)");
           org.junit.Assert.assertEquals(new LispString("doc"), doc);
           doc = evaluateString("(documentation-property 'a 'function-documentation)");
           org.junit.Assert.assertEquals(new LispString("doc"), doc);
       }

       @Test
       public void testDocumentationPropertyFun () {
           evaluateString("(defvar a 2)");
           evaluateString("(put 'a 'function-documentation \"doc\")");
           LObject doc = evaluateString("(documentation 'a)");
           org.junit.Assert.assertEquals(new LispString("doc"), doc);
       }


}
