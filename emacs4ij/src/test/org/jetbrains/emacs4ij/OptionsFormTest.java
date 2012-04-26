package org.jetbrains.emacs4ij;

import com.intellij.psi.PsiFile;
import com.intellij.testFramework.fixtures.CodeInsightFixtureTestCase;
import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/13/12
 * Time: 3:09 PM
 * To change this template use File | Settings | File Templates.
 */
public class OptionsFormTest extends CodeInsightFixtureTestCase {
    CustomEnvironment myEnvironment;
    String myTestsPath;
    HashMap<String, IdeaBuffer> myTests;
    String[]  myTestFiles;

    @Before
    public void setUp() throws Exception {
        myTestsPath = TestSetup.setGlobalEnv();
        super.setUp();
        List<String> list = Arrays.asList((new File(myTestsPath)).list());
        Collections.reverse(list);
        myTestFiles = list.toArray(new String[list.size()]);

        GlobalEnvironment.initialize(new KeymapCreator(), new BufferCreator(), new WindowCreator(),
                new IdeProvider(), new TestFrameManagerImpl());
        myEnvironment = new CustomEnvironment(GlobalEnvironment.INSTANCE);
        for (int i = myTestFiles.length - 1; i > -1; i--) {
            String fileName = myTestFiles[i];
            PsiFile psiFile = myFixture.configureByFile(myTestsPath + fileName);
            IdeaBuffer buffer = new IdeaBuffer(myEnvironment, psiFile.getVirtualFile(), getEditor());
            myTests.put(fileName, buffer);
        }
    }

    @Test
    public void testForm() {
        OptionsForm optionsForm = new OptionsForm(null);
        optionsForm.setVisible(true);
        optionsForm.pack();
    }
    
}
