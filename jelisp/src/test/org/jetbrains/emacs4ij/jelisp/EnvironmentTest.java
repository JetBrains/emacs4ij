package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.junit.*;

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
    public void testGetFunctionFromFile() {
        String lispObjectFileNameFile = GlobalEnvironment.getEmacsSource() + "/lisp/help-fns.el";
        String lispFunctionName = "find-lisp-object-file-name";
        LispList functionFromFile = GlobalEnvironment.getDefFromFile(lispObjectFileNameFile, lispFunctionName, GlobalEnvironment.SymbolType.FUN);
        Assert.assertEquals(new LispSymbol(lispFunctionName), ((LispList)functionFromFile.cdr()).car());
    }

    @Ignore
    @Test
    public void testFindEmacsFinder() {
        LispSymbol finder = e.find(GlobalEnvironment.ourFinder.getName());
        Assert.assertEquals(GlobalEnvironment.ourFinder, finder);
    }
    
    @Test
    public void testGetDef() {
        LispList def = GlobalEnvironment.getDefFromFile("/home/kate/Downloads/emacs-23.4/lisp/edmacro.el", "edmacro-parse-keys", GlobalEnvironment.SymbolType.FUN);
        Assert.assertNotNull(def);
    }
    
    @Test
    public void testGetDef1() {
        LispList def = GlobalEnvironment.getDefFromFile("/home/kate/Downloads/emacs-23.4/lisp/simple.el", "region-active-p", GlobalEnvironment.SymbolType.FUN);
        Assert.assertNotNull(def);
    }
    
    @Test
    public void testCommandList() {
        List<String> commandList = GlobalEnvironment.INSTANCE.getCommandList("f");
        Assert.assertFalse(commandList.isEmpty());
    }
}
