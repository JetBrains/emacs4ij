package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/5/12
 * Time: 2:32 PM
 * To change this template use File | Settings | File Templates.
 */
public class DefinitionLoaderTest {
    @BeforeClass
    public static void runBeforeClass() {
        TestSetup.runBeforeClass();
    }

    @Test
    public void testInit() {
        DefinitionLoader.test();
    }

    @Test
    public void testGetFunctionFromFile() {
        String lispObjectFileNameFile = GlobalEnvironment.getEmacsSource() + "/lisp/help-fns.el";
        String lispFunctionName = "find-lisp-object-file-name";
        LispList functionFromFile = DefinitionLoader.getDefFromFile(lispObjectFileNameFile, lispFunctionName,
                DefinitionLoader.DefType.FUN);
        Assert.assertEquals(new LispSymbol(lispFunctionName), ((LispList) functionFromFile.cdr()).car());
    }

    @Test
    public void testGetDef() {
        LispList def = DefinitionLoader.getDefFromFile("/home/kate/Downloads/emacs-23.4/lisp/edmacro.el",
                "edmacro-parse-keys", DefinitionLoader.DefType.FUN);
        Assert.assertNotNull(def);
    }

    @Test
    public void testGetDef1() {
        LispList def = DefinitionLoader.getDefFromFile("/home/kate/Downloads/emacs-23.4/lisp/simple.el",
                "region-active-p", DefinitionLoader.DefType.FUN);
        Assert.assertNotNull(def);
    }

    @Test
    public void testFindMark () throws Throwable {
        try {
            DefinitionLoader.getDefFromFile(GlobalEnvironment.getEmacsSource() + "/lisp/simple.el", "mark",
                    DefinitionLoader.DefType.FUN);
        } catch (Exception e) {
            System.out.println(TestSetup.getCause(e));
        }
    }

    @Test
    public void testParseDefineMinorMode() {
        GlobalEnvironment.setEmacsSource("/home/kate/Downloads/emacs-23.4");
        LispObject dmm = DefinitionLoader.getDefFromFile(GlobalEnvironment.getEmacsSource() + "/lisp/emacs-lisp/easy-mmode.el",
                "define-minor-mode", DefinitionLoader.DefType.FUN);
        junit.framework.Assert.assertNotNull(dmm);
    }

    @Test
    public void testDefFormsAreInRightPlaces() {
        List<String> filename = DefinitionLoader.getFileName("defcustom", DefinitionLoader.DefType.FUN);
        Assert.assertNotNull(filename);
        Assert.assertTrue(containsStringWhichEndsWith(filename, "/lisp/custom.el"));
        Assert.assertTrue(containsStringWhichEndsWith(
                DefinitionLoader.getFileName("defsubst", DefinitionLoader.DefType.FUN), "/lisp/emacs-lisp/byte-run.el"));
        Assert.assertTrue(containsStringWhichEndsWith(
                DefinitionLoader.getFileName("defgroup", DefinitionLoader.DefType.FUN), "/lisp/custom.el"));
        Assert.assertTrue(containsStringWhichEndsWith(
                DefinitionLoader.getFileName("defface", DefinitionLoader.DefType.FUN), "/lisp/custom.el"));
    }

    private boolean containsStringWhichEndsWith (List<String> list, String ending) {
        for (String s: list)
            if (s.endsWith(ending))
                return true;
        return false;
    }

}
