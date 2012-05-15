package org.jetbrains.emacs4ij;

import com.intellij.psi.PsiFile;
import com.intellij.testFramework.fixtures.CodeInsightFixtureTestCase;
import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.parser.ForwardParser;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.util.*;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/16/12
 * Time: 1:18 PM
 * To change this template use File | Settings | File Templates.
 */
public class ScanTest extends CodeInsightFixtureTestCase {
    CustomEnvironment myEnvironment;
    ForwardParser myForwardParser = new ForwardParser();
    String myTestsPath;
    Map<String, IdeaBuffer> myTests = new HashMap<>();
    String[] myTestFiles;

    @Before
    public void setUp() throws Exception {
        myTestsPath = TestSetup.setGlobalEnv();
        super.setUp();
        List<String> list = Arrays.asList((new File(myTestsPath)).list());
        Collections.sort(list);
        myTestFiles = list.toArray(new String[list.size()]);

        GlobalEnvironment.initialize(new KeymapCreator(), new BufferCreator(), new WindowCreator(),
                new IdeProvider(), new TestFrameManagerImpl());
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

    @Test
    public void testEvalLastSexp() {
        LispBuffer buffer1 = myEnvironment.findBufferSafe("3.txt");
        myEnvironment.switchToBuffer(buffer1);
        myEnvironment.setBufferCurrentForEditing(buffer1);
        LispBuffer buffer = myEnvironment.getBufferCurrentForEditing();
        buffer.gotoChar(buffer.pointMax()-1);
        LispObject result = evaluateString("(eval-last-sexp nil)");
        Assert.assertEquals(new LispInteger(8), result);
    }

    @Test
    public void testScanSexpsForward() {
        LispBuffer buffer1 = myEnvironment.findBufferSafe("4.txt");
        myEnvironment.switchToBuffer(buffer1);
        myEnvironment.setBufferCurrentForEditing(buffer1);
        LispObject sexpEnd = evaluateString("(scan-sexps 1 1)");
        Assert.assertEquals(new LispInteger(7), sexpEnd);
    }

    @Test
    public void testScanSexpsForwardString() {
        LispBuffer buffer1 = myEnvironment.findBufferSafe("4.txt");
        myEnvironment.switchToBuffer(buffer1);
        myEnvironment.setBufferCurrentForEditing(buffer1);
        LispObject sexpEnd = evaluateString("(scan-sexps 30 1)");
        Assert.assertEquals(new LispInteger(38), sexpEnd);
    }

    @Test
    public void testScanSexpsForwardStringWithEscaped() {
        LispBuffer buffer1 = myEnvironment.findBufferSafe("4.txt");
        myEnvironment.switchToBuffer(buffer1);
        myEnvironment.setBufferCurrentForEditing(buffer1);
        LispObject sexpEnd = evaluateString("(scan-sexps 47 1)");
        Assert.assertEquals(new LispInteger(70), sexpEnd);
    }

}
