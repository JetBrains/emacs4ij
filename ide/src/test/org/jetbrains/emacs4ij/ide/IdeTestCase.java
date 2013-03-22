package org.jetbrains.emacs4ij.ide;

import com.intellij.openapi.components.ServiceManager;
import com.intellij.psi.PsiFile;
import com.intellij.testFramework.fixtures.CodeInsightFixtureTestCase;
import com.intellij.ui.EditorTextField;
import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.DefinitionLoader;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.LogUtil;
import org.jetbrains.emacs4ij.jelisp.TestMode;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.parser.ForwardParser;
import org.junit.Before;

import java.io.File;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

class IdeTestCase extends CodeInsightFixtureTestCase {
  protected Environment myEnvironment;
  protected String myTestsPath;
  protected Map<String, IdeaBuffer> myTests = new HashMap<>();
  protected String[] myTestFiles;

  @Before
  public void setUp() throws Exception {
    super.setUp();
    myTestsPath = setGlobalEnv();
    myEnvironment = new CustomEnvironment(GlobalEnvironment.INSTANCE);
  }

  private String setGlobalEnv() {
    TestMode.TEST = true;
    TestMode.EXTRACT_DOC = false;
//    TestMode.LOAD_FILES = false;
//    TestMode.INIT_GLOBAL_ENV_FROM_EMACS_SOURCES = false;

    if (GlobalEnvironment.INSTANCE == null) {
      LogUtil.info("INIT GLOBAL ENV");
      GlobalEnvironment.setEmacsSource("/home/kate/Downloads/emacs-23.4");
      GlobalEnvironment.setEmacsHome("/usr/share/emacs/23.4");
      DefinitionLoader.initialize(ServiceManager.getService(EmacsIndexService.class).getEmacsIndex());
      GlobalEnvironment.initialize(new KeymapCreator(), new BufferCreator(), new WindowCreator(), new TestFrameManagerImpl(), new IdeProvider(),
          new Runnable() {
            @Override
            public void run() {
              TestIdeaMinibuffer.init(new EditorTextField().getEditor(), null);
            }
          }
      );
    }
    GlobalEnvironment.INSTANCE.startRecording();

    GlobalEnvironment.INSTANCE.defineBuffer(TestIdeaMinibuffer.getInstance());

    return "/home/kate/emacs4ij/ide/src/testSrc/";
  }

  protected final void setTestFiles(boolean reverseOrder) {
    List<String> list = Arrays.asList((new File(myTestsPath)).list());
    Collections.sort(list);
    for (String fileName: list) {
      PsiFile psiFile = myFixture.configureByFile(myTestsPath + fileName);
      IdeaBuffer buffer = new IdeaBuffer(myEnvironment, psiFile.getVirtualFile(), getEditor());
      myTests.put(fileName, buffer);
    }
    if (reverseOrder) {
      Collections.reverse(list);
    }
    myTestFiles = list.toArray(new String[list.size()]);
  }

  protected final String getCauseMsg(Throwable e) {
    return getCause(e).getMessage();
  }

  protected final Throwable getCause (Throwable e) {
    if (e.getCause() == null) return e;
    return getCause(e.getCause());
  }

  protected final LispObject evaluateString(String lispCode) {
    return new ForwardParser().parseLine(lispCode).evaluate(myEnvironment);
  }

  protected final void assertT(LispObject object) {
    Assert.assertEquals(LispSymbol.T, object);
  }

  protected final void assertNil(LispObject object) {
    Assert.assertEquals(LispSymbol.NIL, object);
  }
}
