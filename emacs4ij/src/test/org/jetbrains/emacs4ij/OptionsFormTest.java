package org.jetbrains.emacs4ij;

import com.intellij.testFramework.fixtures.CodeInsightFixtureTestCase;
import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.ForwardParser;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.LObject;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.util.HashMap;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/13/12
 * Time: 3:09 PM
 * To change this template use File | Settings | File Templates.
 */
public class OptionsFormTest extends CodeInsightFixtureTestCase {
    CustomEnvironment myEnvironment;
    ForwardParser myForwardParser = new ForwardParser();
    String myTestsPath;
    HashMap<String, IdeaBuffer> myTests;
    String[]  myTestFiles;

    @Before
    public void setUp() throws Exception {
        myTestsPath = TestSetup.setGlobalEnv();
        super.setUp();
        myTestFiles = (new File(myTestsPath)).list();
        myTests = new HashMap<>();
        GlobalEnvironment.initialize(new BufferCreator(), new IdeProvider());
        myEnvironment = new CustomEnvironment(GlobalEnvironment.INSTANCE);
        for (String fileName: myTestFiles) {
            myFixture.configureByFile(myTestsPath + fileName);
            IdeaBuffer buffer = new IdeaBuffer(myEnvironment, fileName, myTestsPath, getEditor());
            myTests.put(fileName, buffer);
        }
    }

    private Throwable getCause (Throwable e) {
        if (e.getCause() == null)
            return e;
        return getCause(e.getCause());
    }

    private LObject evaluateString (String lispCode) {
        return myForwardParser.parseLine(lispCode).evaluate(myEnvironment);
    }

    @Test
    public void testForm() {
        OptionsForm optionsForm = new OptionsForm(null);// (PlatformDataKeys.PROJECT.getData(e.getDataContext()).getComponent(MyProjectComponent.class));
        optionsForm.setVisible(true);
        optionsForm.pack();
    }
    
}
