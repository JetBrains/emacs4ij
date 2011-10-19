package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/13/11
 * Time: 3:43 PM
 * To change this template use File | Settings | File Templates.
 */
public class EnvironmentTest {

    private Environment e;

    @Before
    public void setUp() throws Exception {
        e = new Environment(new Environment());
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


    /*
    @Test
    public void testGetFunctionFromFile() {
        String lispObjectFileNameFile = "c:\\Users\\ekaterina.polishchuk\\Downloads\\emacs-23.3\\lisp\\help-fns.el";
        String lispFunctionName = "find-lisp-object-file-name";
        LispList functionFromFile = e.getFunctionFromFile(lispObjectFileNameFile, lispFunctionName);
        Assert.assertEquals(new LispSymbol(lispFunctionName), (functionFromFile.cdr()).car());
    }

    @Test
    public void testFindEmacsFinder() {
        Environment.ourEmacsPath = "c:\\Users\\ekaterina.polishchuk\\Downloads\\emacs-23.3\\";
        CustomFunction finder = (CustomFunction) e.find(Environment.ourFinder.getName(), Environment.SymbolType.FUNCTION, "");
        Assert.assertEquals(Environment.ourFinder, finder.getName());
    }

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
