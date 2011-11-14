package org.jetbrains.emacs4ij;

import com.intellij.openapi.editor.Editor;
import com.intellij.testFramework.fixtures.CodeInsightFixtureTestCase;
import com.intellij.ui.EditorTextField;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.Parser;
import org.jetbrains.emacs4ij.jelisp.elisp.LObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
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

    String myTestsPath = "/home/kate/emacs4ij/emacs4ij/src/testSrc/";
    EditorTextField myMiniBufferEditor;

    @Before
    public void setUp() throws Exception {
        super.setUp();

        GlobalEnvironment.ourEmacsPath = "/usr/share/emacs/23.2";
        GlobalEnvironment.initialize(new BufferCreator(), null);
        myEnvironment = new Environment(GlobalEnvironment.getInstance());

        String fileName = "1.txt";
        myFixture.configureByFile(myTestsPath + fileName);

        Editor editor = getEditor();
        myMiniBufferEditor = new EditorTextField();
        myMiniBufferEditor.addNotify();
        editor.setHeaderComponent(myMiniBufferEditor);
        IdeaEditor buffer = new IdeaEditor(myEnvironment, fileName, myTestsPath, editor);
        myEnvironment.defineBuffer(buffer);

        myMiniBuffer = new IdeaMiniBuffer(0, myMiniBufferEditor.getEditor(), myEnvironment);
        myMiniBuffer.setReadCommandStatus();
        myEnvironment.defineServiceBuffer(myMiniBuffer);

        myMiniBuffer.readCommand(null, null, false);
    }

    @Override
    public void tearDown() throws Exception {
        myMiniBufferEditor.removeNotify();
        super.tearDown();
    }

    private LObject evaluateString (String lispCode) throws LispException {
        Parser parser = new Parser();
        return parser.parseLine(lispCode).evaluate(myEnvironment);
    }

    @Test
    public void testReadCommand () {
        myMiniBuffer.appendText("bury");
        LObject result = myMiniBuffer.onReadInput();
        Assert.assertEquals(LispSymbol.ourNil, result);
    }

    @Test
    public void testArgumentA () {
        evaluateString("(defun g (a) (+ a 5))");
        evaluateString("(defun f (fun) (interactive \"aFunction: \") (funcall fun 5))");
        myMiniBuffer.appendText("f");
        myMiniBuffer.onReadInput();
        myMiniBuffer.appendText("g");
        LObject result = myMiniBuffer.onReadInput();
        Assert.assertEquals(new LispInteger(10), result);
    }


}
