package org.jetbrains.emacs4ij;

import com.intellij.testFramework.fixtures.CodeInsightFixtureTestCase;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.Parser;
import org.jetbrains.emacs4ij.jelisp.elisp.LObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.util.HashMap;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 11/21/11
 * Time: 8:08 PM
 * To change this template use File | Settings | File Templates.
 */
public class MarkTest extends CodeInsightFixtureTestCase {
    Environment myEnvironment;
    Parser myParser = new Parser();
    String myTestsPath = "/home/kate/emacs4ij/emacs4ij/src/testSrc/";
    HashMap<String, IdeaBuffer> myTests;
    String[]  myTestFiles;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        myTestFiles = (new File(myTestsPath)).list();
        myTests = new HashMap<String, IdeaBuffer>();

        GlobalEnvironment.ourEmacsPath = "/home/kate/Downloads/emacs 23.2a/emacs-23.2";
        GlobalEnvironment.initialize(new BufferCreator(), myFixture.getProject(), new IdeProvider());
        myEnvironment = new Environment(GlobalEnvironment.getInstance());
        for (String fileName: myTestFiles) {
            myFixture.configureByFile(myTestsPath + fileName);
            IdeaBuffer buffer = new IdeaBuffer(myEnvironment, fileName, myTestsPath, getEditor());
            myTests.put(fileName, buffer);
            myEnvironment.defineBuffer(buffer);
        }
    }

    private Throwable getCause (Throwable e) {
        if (e.getCause() == null)
            return e;
        return getCause(e.getCause());
    }

    private LObject evaluateString (String lispCode) {
        return myParser.parseLine(lispCode).evaluate(myEnvironment);
    }


    @Test
    public void testMark () throws Throwable {
        try {
            GlobalEnvironment.findAndRegisterEmacsFunction(GlobalEnvironment.ourEmacsPath + "/lisp/simple.el", "mark");
            LObject mark = evaluateString("(mark)");
            Assert.assertEquals(LispSymbol.ourNil, mark);
        } catch (Exception e) {
            System.out.println(getCause(e).getMessage());
            throw getCause(e);
        }
    }

    @Test
    public void testSetMark () throws Throwable {
        try {
            GlobalEnvironment.findAndRegisterEmacsFunction(GlobalEnvironment.ourEmacsPath + "/lisp/simple.el", "set-mark");
            LObject mark = evaluateString("(set-mark 10)");
            System.out.println("Mark set: " + mark.toString());
            Assert.assertEquals(LispSymbol.ourNil, mark);
        } catch (Exception e) {
            System.out.println(getCause(e).getMessage());
           // throw getCause(e);
        }

    }

     /*
    @Test
    public void testSetMark() {

    }

    @Test
    public void testUserOption() {
        LObject result = evaluateString("(setq mark-ring-max \"lol\")");
        Assert.assertEquals(new LispString("lol"), result);
        try {
            evaluateString("(push-mark)");
        } catch (Exception e) {
            Assert.assertTrue(getCause(e) instanceof WrongTypeArgumentException);
            return;
        }
        Assert.fail("Reset mark-ring-max to string failed.");
    }
    */

}
