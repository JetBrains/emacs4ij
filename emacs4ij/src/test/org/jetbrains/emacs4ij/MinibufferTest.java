package org.jetbrains.emacs4ij;

import com.intellij.testFramework.fixtures.CodeInsightFixtureTestCase;
import com.intellij.ui.EditorTextField;
import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.parser.ForwardParser;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/23/11
 * Time: 4:50 PM
 * To change this template use File | Settings | File Templates.
 */
public class MinibufferTest extends CodeInsightFixtureTestCase {
    private IdeaMiniBuffer myMiniBuffer;
    private Environment myEnvironment;
    String myTestsPath;

    @Before
    public void setUp() throws Exception {
        myTestsPath = TestSetup.setGlobalEnv();
        super.setUp();
        GlobalEnvironment.initialize(new KeymapCreator(), new BufferCreator(), new WindowCreator(),
                new IdeProvider(), new TestFrameManagerImpl());
        myEnvironment = new CustomEnvironment(GlobalEnvironment.INSTANCE);
        EditorTextField t = new EditorTextField();
        myMiniBuffer = new IdeaMiniBuffer(0, t.getEditor(), myEnvironment, null);
    }

    private LispObject evaluateString (String lispCode) throws LispException {
        ForwardParser forwardParser = new ForwardParser();
        return forwardParser.parseLine(lispCode).evaluate(myEnvironment);
    }

    @Test
    public void testReturnDefault_Integer () {
        try {
            myMiniBuffer.returnDefault(new LispInteger(5));
        } catch (WrongTypeArgumentException e) {
            Assert.assertTrue(true);
            return;
        }
        Assert.assertTrue(false);
    }

    @Test
    public void testReturnDefault_StringList () {
        LispObject ret = myMiniBuffer.returnDefault (LispList.list(new LispString("hi"), new LispInteger(5)));
        Assert.assertEquals(new LispSymbol("hi"), ret);
    }

    @Test
    public void testReturnDefault_String () {
        LispObject ret = myMiniBuffer.returnDefault (new LispString("hi"));
        Assert.assertEquals(new LispSymbol("hi"), ret);
    }

    @Test
    public void testReturnDefault_IntList () {
        try {
            myMiniBuffer.returnDefault (LispList.list(new LispInteger(5), new LispString("hi")));
        } catch (WrongTypeArgumentException e) {
            Assert.assertTrue(true);
            return;
        }
        Assert.assertTrue(false);
    }

    @Test
    public void testReturnDefault_ListList () {
        try {
            myMiniBuffer.returnDefault (LispList.list(LispList.list(new LispString("wow"), new LispString("hi")), new LispInteger(5), new LispString("hi")));
        } catch (WrongTypeArgumentException e) {
            Assert.assertTrue(true);
            return;
        }
        Assert.assertTrue(false);
    }

    @Test
    public void testReturnDefault_ExistingSymbol () {
        evaluateString("(setq s 5)");
        LispObject ret = myMiniBuffer.returnDefault (new LispSymbol("s"));
        Assert.assertEquals(new LispSymbol("s", new LispInteger(5)), ret);
    }

    @Test
    public void testReturnDefault_NonExistingSymbol () {
        LispObject ret = myMiniBuffer.returnDefault (new LispSymbol("s"));
        Assert.assertEquals(new LispSymbol("s"), ret);
    }

    @Test
    public void testReturnDefault_NilSymbol () {
        LispObject ret = myMiniBuffer.returnDefault (LispSymbol.ourNil);
        Assert.assertEquals(new LispSymbol(""), ret);
    }

    @Test
    public void testReturnDefault_EmptyList () {
        LispObject ret = myMiniBuffer.returnDefault (LispList.list());
        Assert.assertEquals(new LispSymbol(""), ret);
    }

    @Test
    public void testMessage() {
        evaluateString("(minibuffer-message \"test\")");
    }
}
