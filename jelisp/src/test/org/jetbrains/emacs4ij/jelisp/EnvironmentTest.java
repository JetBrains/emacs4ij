package org.jetbrains.emacs4ij.jelisp;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
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
        e = new Environment(Environment.ourGlobal);
    }

    @Test
    public void testGetBuiltInF () {
        LispObject lispObject = e.find("+", Environment.SymbolType.FUNCTION);
        Assert.assertTrue(lispObject instanceof LispBuiltinFunction);
        Assert.assertEquals(new LispSymbol("+"), ((LispBuiltinFunction) lispObject).getName());
    }

    @Test
    public void testOverrideVar () {
        e.setVariable(new LispSymbol("a"), new LispInteger(5));
        e.setVariable(new LispSymbol("a"), new LispInteger(6));
        Assert.assertEquals(new LispInteger(6), e.getVariable("a"));
    }

    @Test
    public void testGetFunctionFromFile() {
        String lispObjectFileNameFile = "c:\\Users\\ekaterina.polishchuk\\Downloads\\emacs-23.3\\lisp\\help-fns.el";
        String lispFunctionName = "find-lisp-object-file-name";
        LispList functionFromFile = e.getFunctionFromFile(lispObjectFileNameFile, lispFunctionName);
        Assert.assertEquals(new LispSymbol(lispFunctionName), ((LispList) functionFromFile.cdr()).car());
    }

    @Test
    public void testFindEmacsFinder() {
        Environment.ourEmacsPath = "c:\\Users\\ekaterina.polishchuk\\Downloads\\emacs-23.3\\";
        LispCustomFunction finder = (LispCustomFunction) e.find(Environment.ourFinder.getName(), Environment.SymbolType.FUNCTION);
        Assert.assertEquals(Environment.ourFinder, finder.getName());
    }

}
