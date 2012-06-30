package org.jetbrains.emacs4ij;

import com.intellij.psi.PsiFile;
import com.intellij.testFramework.fixtures.CodeInsightFixtureTestCase;
import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.parser.ForwardParser;
import org.junit.Before;

import java.io.File;
import java.util.*;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 6/4/12
 * Time: 11:29 AM
 * To change this template use File | Settings | File Templates.
 */
public class FrameTest extends CodeInsightFixtureTestCase {
    CustomEnvironment myEnvironment;
    ForwardParser myForwardParser = new ForwardParser();
    String myTestsPath;
    Map<String, IdeaBuffer> myTests = new HashMap<>();
    String[] myTestFiles;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        myTestsPath = TestSetup.setGlobalEnv();
        List<String> list = Arrays.asList((new File(myTestsPath)).list());
        Collections.sort(list);
        myTestFiles = list.toArray(new String[list.size()]);
        myEnvironment = new CustomEnvironment(GlobalEnvironment.INSTANCE);
        for (String fileName: myTestFiles) {
            PsiFile psiFile = myFixture.configureByFile(myTestsPath + fileName);
            IdeaBuffer buffer = new IdeaBuffer(myEnvironment, psiFile.getVirtualFile(), getEditor());
            myTests.put(fileName, buffer);
        }
    }

    private LispObject evaluateString(String lispCode) {
        return myForwardParser.parseLine(lispCode).evaluate(myEnvironment);
    }

    public void testFrameParameters() {
        Assert.assertEquals(LispSymbol.ourNil, evaluateString("(modify-frame-parameters nil '((test 2 3) (a 1) . 5))"));
        Assert.assertEquals(LispList.list(new LispInteger(2), new LispInteger(3)),
                evaluateString("(frame-parameter nil 'test)"));
        Assert.assertEquals(LispList.list(new LispInteger(1)),
                evaluateString("(frame-parameter nil 'a)"));
        LispObject parameters = evaluateString("(frame-parameters)");
//        System.out.println(parameters);
        Assert.assertTrue(parameters.toString().contains("(test 2 3)") && parameters.toString().contains("(a 1)"));
        Assert.assertEquals(LispSymbol.ourNil, evaluateString("(frame-parameter nil 'buffer-predicate)"));
    }

    public void testFrameParametersWrongArg() {
        try {
            evaluateString("(modify-frame-parameters nil '(test 1))");
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument listp test)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    public void testFrameParametersWrongName() {
        try {
            evaluateString("(modify-frame-parameters nil '((2 1)))");
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument symbolp 2)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

}