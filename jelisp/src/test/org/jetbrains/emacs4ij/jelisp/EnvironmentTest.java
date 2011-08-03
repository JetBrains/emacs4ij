package org.jetbrains.emacs4ij.jelisp;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import java.io.File;

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
        e = new Environment(Environment.ourGlobal);
    }
    /*
    @Test
    public void testGetBuiltInF () {
        LispObject lispObject = e.find("+", Environment.SymbolType.FUNCTION);
        Assert.assertTrue(lispObject instanceof CoreBuiltin);
        Assert.assertEquals(new LispSymbol("+"), ((CoreBuiltin) lispObject).getName());
    }

    @Test
    public void testOverrideVar () {
        e.defineVariable(new LispSymbol("a"), new LispInteger(5));
        e.defineVariable(new LispSymbol("a"), new LispInteger(6));
        Assert.assertEquals(new LispInteger(6), ((LispVariable)e.getSymbol("a")).getValue());
    }

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
    */
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
    }

    @Test
    public void invokeMeth() {
        LispSymbol lispObject = new LispSymbol("test");
        String methodName = "setProperty";
        Object[] methodParameters = new Object[] {new LispSymbol("name"), new LispString("i am test")};
        LispObject lispObject1 = lispObject.invokeMethod(methodName, new Class[]{LispSymbol.class, LispObject.class}, methodParameters);
        Assert.assertEquals(lispObject, lispObject1);
    }

    @Test
    public void testSwitch() {
        int k = 5;
        switch (k) {
            case 1:
                case 2:
                    System.out.println("2");
                    break;
                case 5:
                    System.out.println("5");
                    break;
                //break;
            default:
                System.out.println("0");

        }
    }

}
