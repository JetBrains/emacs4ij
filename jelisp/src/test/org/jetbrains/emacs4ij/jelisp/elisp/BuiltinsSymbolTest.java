package org.jetbrains.emacs4ij.jelisp.elisp;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.Parser;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.junit.Before;
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

    @Before
    public void setUp() {
        GlobalEnvironment.ourEmacsSource = "/home/kate/Downloads/emacs 23.2a/emacs-23.2";
        GlobalEnvironment.ourEmacsPath = "/usr/share/emacs/23.2";
        GlobalEnvironment.initialize(null, null, null);
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

        Assert.assertTrue(doc != null && doc != LispSymbol.ourNil);
        Assert.assertEquals(trueDoc, doc);
    }


    @Test
    public void testIntegerDocumentation () {
        LObject doc = evaluateString("(get 'internal-doc-file-name 'variable-documentation)");
    }
}
