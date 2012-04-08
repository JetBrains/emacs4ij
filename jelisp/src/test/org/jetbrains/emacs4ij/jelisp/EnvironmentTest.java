package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/13/11
 * Time: 3:43 PM
 * To change this template use File | Settings | File Templates.
 */
public class EnvironmentTest {

    private Environment e;

    @BeforeClass
    public static void runBeforeClass() throws Exception {
        TestSetup.runBeforeClass();
    }

    @Before
    public void setUp() throws Exception {
        GlobalEnvironment.INSTANCE.clearRecorded();
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
    public void testCommandList() {
        List<String> commandList = GlobalEnvironment.INSTANCE.getCommandList("f");
        Assert.assertFalse(commandList.isEmpty());
    }
}
