package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/13/11
 * Time: 3:43 PM
 * To change this template use File | Settings | File Templates.
 */
public class CustomEnvironmentTest {

    private CustomEnvironment e;

    @Before
    public void setUp() throws Exception {
        GlobalEnvironment.ourEmacsSource = "/home/kate/Downloads/emacs 23.2a/emacs-23.2";
        GlobalEnvironment.ourEmacsPath = "/usr/share/emacs/23.2";
        GlobalEnvironment.initialize(null, null);
        e = new CustomEnvironment(GlobalEnvironment.INSTANCE);
    }

    @Test
    public void testGetBuiltInF () {
        LispObject lispObject = e.find("+");
        Assert.assertTrue(lispObject instanceof LispSymbol);
        //Assert.assertTrue(!((LispSymbol) lispObject).getFunction().equals(LispSymbol.ourVoid));
    }

    @Test
    public void testOverrideVar () {
        e.defineSymbol(new LispSymbol("a", new LispInteger(5)));
        e.defineSymbol(new LispSymbol("a", new LispInteger(6)));
        Assert.assertEquals(new LispInteger(6), (e.find("a")).getValue());
    }

    @Test
    public void testGetFunctionFromFile() {
        String lispObjectFileNameFile = GlobalEnvironment.ourEmacsSource + "/lisp/help-fns.el";
        String lispFunctionName = "find-lisp-object-file-name";
        LispList functionFromFile = GlobalEnvironment.INSTANCE.getDefFromFile(lispObjectFileNameFile, lispFunctionName);
        Assert.assertEquals(new LispSymbol(lispFunctionName), ((LispList)functionFromFile.cdr()).car());
    }

    @Ignore
    @Test
    public void testFindEmacsFinder() {
        LispSymbol finder = e.find(GlobalEnvironment.ourFinder.getName());
        Assert.assertEquals(GlobalEnvironment.ourFinder, finder.getName());
    }

    /*
    @Ignore
    @Test
    public void findir() {
        File[] fList = new File("c:\\Users\\ekaterina.polishchuk\\Downloads\\emacs-23.3\\lisp").listFiles();
        for (File f: fList) {

            if (f.isDirectory())
                System.out.println(f.getName() + " --DIR");
            else
                System.out.println(f.getName());
        }
    }    */
}
