package org.jetbrains.emacs4ij;

import com.intellij.openapi.editor.Editor;
import com.intellij.testFramework.fixtures.CodeInsightFixtureTestCase;
import com.intellij.ui.EditorTextField;
import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.parser.ForwardParser;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 11/4/11
 * Time: 4:24 PM
 * To change this template use File | Settings | File Templates.
 */
public class InteractiveTest extends CodeInsightFixtureTestCase {
    private IdeaMiniBuffer myMiniBuffer;
    private Environment myEnvironment;
    String myTestsPath;
    EditorTextField myMiniBufferEditor;
    String myFileName = "1.txt";

    @Before
    public void setUp() throws Exception {
        myTestsPath = TestSetup.setGlobalEnv();
        super.setUp();
        GlobalEnvironment.initialize(new KeymapCreator(), new IdeProvider(), new TestFrameManagerImpl());
        myEnvironment = new CustomEnvironment(GlobalEnvironment.INSTANCE);
        myFixture.configureByFile(myTestsPath + myFileName);
        Editor editor = getEditor();
        myMiniBufferEditor = new EditorTextField();
        myMiniBufferEditor.addNotify();
        editor.setHeaderComponent(myMiniBufferEditor);
        new IdeaBuffer(myEnvironment, myFileName, myTestsPath, editor);
        myMiniBuffer = new IdeaMiniBuffer(0, myMiniBufferEditor.getEditor(), myEnvironment, null);
        myMiniBuffer.startRead();
    }

    @Override
    public void tearDown() throws Exception {
        myMiniBufferEditor.removeNotify();
        super.tearDown();
    }

    private LispObject evaluateString (String lispCode) throws LispException {
        ForwardParser forwardParser = new ForwardParser();
        return forwardParser.parseLine(lispCode).evaluate(myEnvironment);
    }

    @Test
    public void testArgument_a () {
        evaluateString("(defun g () (+ 5 5))");
        evaluateString("(defun f (fun) (interactive \"aFunction: \") (funcall fun))");
        myMiniBuffer.appendText("f");
        myMiniBuffer.onReadInput();
        myMiniBuffer.appendText("g");
        LispObject result = myMiniBuffer.onReadInput();
        Assert.assertEquals(new LispInteger(10), result);
    }

    @Test
    public void testArgument_a_fboundp () {
        evaluateString("(defun g () (+ 5 5))");
        evaluateString("(defun f (fun) (interactive \"aFunction: \") (fboundp fun))");
        myMiniBuffer.appendText("f");
        myMiniBuffer.onReadInput();
        myMiniBuffer.appendText("g");
        LispObject result = myMiniBuffer.onReadInput();
        Assert.assertEquals(LispSymbol.ourT, result);

    }

    @Test
    public void testArgument_a_NoMatch () {
        evaluateString("(defun f (fun) (interactive \"aFunction: \") (funcall fun))");
        myMiniBuffer.appendText("f");
        myMiniBuffer.onReadInput();
        myMiniBuffer.appendText("g");
        LispObject result = myMiniBuffer.onReadInput();
        Assert.assertNull(result);
        Assert.assertTrue(myMiniBuffer.getEditor().getDocument().getText().contains("No Match"));
    }

    @Test
    public void testArgument_b () {
        evaluateString("(defun f (buf) (interactive \"bBuffer: \") (buffer-name (get-buffer buf)))");
        myMiniBuffer.appendText("f");
        myMiniBuffer.onReadInput();
        LispObject result = myMiniBuffer.onReadInput();
        Assert.assertEquals(new LispString(myFileName), result);
    }

    @Test
    public void testArgument_b_NoMatch () {
        evaluateString("(defun f (buf) (interactive \"bBuffer: \") (buffer-name (get-buffer buf)))");
        myMiniBuffer.appendText("f");
        myMiniBuffer.onReadInput();
        myMiniBuffer.appendText("buffer.lisp");
        LispObject result = myMiniBuffer.onReadInput();
        Assert.assertNull(result);
        Assert.assertTrue(myMiniBuffer.getEditor().getDocument().getText().contains("No Match"));
    }

    @Test
    public void testArgument_B () {
        evaluateString("(defun f (buf) (interactive \"BBuffer: \") (buffer-name (get-buffer buf)))");
        myMiniBuffer.appendText("f");
        myMiniBuffer.onReadInput();
        LispObject result = myMiniBuffer.onReadInput();
        Assert.assertEquals(new LispString(myFileName), result);
    }

    @Test
    public void testArgument_B_create () {
        evaluateString("(defun f (buf) (interactive \"BBuffer: \") (switch-to-buffer buf))");
        myMiniBuffer.appendText("f");
        myMiniBuffer.onReadInput();
        myMiniBuffer.appendText("buffer.lisp");
        LispObject result = myMiniBuffer.onReadInput();
        Assert.assertEquals (new LispString("It is not allowed to create files this way."), result);
    }

   /* @Test
    public void testArgument_c () {
        evaluateString("(defun f (ch) (interactive \"cCharacter: \") ch)");
        myMiniBuffer.appendText("f");
        myMiniBuffer.onReadInput();
        try {
            Robot robot = new Robot();
            robot.keyPress(KeyEvent.VK_SHIFT);
            robot.keyPress(KeyEvent.VK_G);
        } catch (AWTException e) {
            Assert.fail(e.getMessage());
        }
    }  */


    @Test
    public void testArgument_C_NonExistent () {
        evaluateString("(defun f (cmd) (interactive \"CCommand: \") (funcall cmd))");
        myMiniBuffer.appendText("f");
        myMiniBuffer.onReadInput();
        myMiniBuffer.appendText("g");
        LispObject result = myMiniBuffer.onReadInput();
        Assert.assertNull(result);
        Assert.assertTrue(myMiniBuffer.getEditor().getDocument().getText().contains("No Match"));
    }

    @Test
    public void testArgument_C_NotCommand () {
        evaluateString("(defun f (cmd) (interactive \"CCommand: \") (funcall cmd))");
        evaluateString("(defun g () (+ 5 5))");
        myMiniBuffer.appendText("f");
        myMiniBuffer.onReadInput();
        myMiniBuffer.appendText("g");
        LispObject result = myMiniBuffer.onReadInput();
        Assert.assertNull(result);
        Assert.assertTrue(myMiniBuffer.getEditor().getDocument().getText().contains("No Match"));
    }

    @Test
    public void testArgument_C_WrongNumberOfArguments () {
        evaluateString("(defun f (cmd) (interactive \"CCommand: \") (funcall cmd))");
        evaluateString("(defun g (n) (interactive \"nNumber: \") (+ n 5))");
        myMiniBuffer.appendText("f");
        myMiniBuffer.onReadInput();
        myMiniBuffer.appendText("g");
        try {
            myMiniBuffer.onReadInput();
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-number-of-arguments (lambda (n) (interactive \"nNumber: \") (+ n 5)) 0)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testArgument_C () {
        evaluateString("(defun f (cmd) (interactive \"CCommand: \") (funcall cmd))");
        evaluateString("(defun g () (interactive) (+ 5 5))");
        myMiniBuffer.appendText("f");
        myMiniBuffer.onReadInput();
        myMiniBuffer.appendText("g");
        LispObject result = myMiniBuffer.onReadInput();
        Assert.assertEquals(new LispInteger(10), result);
    }

    @Test
    public void testArgument_n () {
        evaluateString("(defun f (n) (interactive \"nNumber: \") (+ n 5))");
        myMiniBuffer.appendText("f");
        myMiniBuffer.onReadInput();
        myMiniBuffer.appendText("5.6");
        LispObject result = myMiniBuffer.onReadInput();
        Assert.assertEquals(new LispInteger(10), result);
    }

    @Test
    public void testArgument_n_NoMatch () {
        evaluateString("(defun f (n) (interactive \"nNumber: \") (+ n 5))");
        myMiniBuffer.appendText("f");
        myMiniBuffer.onReadInput();
        myMiniBuffer.appendText("hello");
        LispObject result = myMiniBuffer.onReadInput();
        Assert.assertNull(result);
        Assert.assertTrue(myMiniBuffer.getEditor().getDocument().getText().contains("Please, enter a number."));
    }

}
