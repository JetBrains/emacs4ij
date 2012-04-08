package org.jetbrains.emacs4ij.jelisp.elisp;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.TestSetup;
import org.jetbrains.emacs4ij.jelisp.parser.ForwardParser;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/2/12
 * Time: 1:42 PM
 * To change this template use File | Settings | File Templates.
 */
public class LambdaTest {
    private CustomEnvironment environment;

    @BeforeClass
    public static void runBeforeClass() {
        TestSetup.runBeforeClass();
    }

    @Before
    public void setUp() throws Exception {
        GlobalEnvironment.INSTANCE.clearRecorded();
        environment = new CustomEnvironment(GlobalEnvironment.INSTANCE);
    }

    private LispObject evaluateString (String lispCode) {
        ForwardParser forwardParser = new ForwardParser();
        return forwardParser.parseLine(lispCode).evaluate(environment);
    }

    @Test
    public void testParseArgumentsList() throws Exception {
        LispList list = (LispList) evaluateString("'(lambda (a &optional b &key c &rest d))");
        List<LambdaArgument> args = new Lambda(list).getArguments();
        Assert.assertEquals(4, args.size());
        Assert.assertEquals(LambdaArgument.Type.REQUIRED, args.get(0).getType());
        Assert.assertEquals(LambdaArgument.Type.OPTIONAL, args.get(1).getType());
        Assert.assertEquals(LambdaArgument.Type.KEYWORD, args.get(2).getType());
        Assert.assertEquals(LambdaArgument.Type.REST, args.get(3).getType());
    }

    @Test
    public void testParseArgumentsList_Keyword_Simple() throws Exception {
        LispList list = (LispList) evaluateString("'(lambda (&key (c 10)))");
        List<LambdaArgument> args = new Lambda(list).getArguments();
        Assert.assertEquals(1, args.size());
        Assert.assertEquals(new LispInteger(10), args.get(0).getInitForm());
        Assert.assertEquals(new LispSymbol(":c"), args.get(0).getKeyword());
        Assert.assertEquals(new LispSymbol("c"), args.get(0).getVar());
    }

    @Test
    public void testParseArguments_Keyword_Complex() throws Exception {
        LispList list = (LispList) evaluateString("'(lambda (&key ((word var) 10 a)))");
        List<LambdaArgument> args = new Lambda(list).getArguments();
        Assert.assertEquals(1, args.size());
        Assert.assertEquals(new LispInteger(10), args.get(0).getInitForm());
        Assert.assertEquals(new LispSymbol("word"), args.get(0).getKeyword());
        Assert.assertEquals(new LispSymbol("var"), args.get(0).getVar());
        Assert.assertEquals(new LispSymbol("a"), args.get(0).getSetVar());
    }

    @Test
    public void testSubstituteArguments() throws Exception {
        LispList list = (LispList) evaluateString("'(lambda (&key ((word var) 10 a)))");
        List<LispObject> args = ((LispList) evaluateString("'('word 5)")).toLispObjectList();
        CustomEnvironment inner = new Lambda(list).substituteArguments(environment, args);
        LispSymbol var = inner.find("var");
        Assert.assertNotNull(var);
        Assert.assertEquals(new LispInteger(5), var.getValue());
    }

    @Test
    public void testSetInteractive() {
        LispSymbol f = (LispSymbol) evaluateString("(defun f () (+ 3 6) (interactive \"B1\") (interactive \"B2\"))");
        if (f.isInteractive()) {
            LispCommand cmd = (LispCommand) f.getFunction();
            Assert.assertEquals(cmd.getInteractiveString(), "B1");
            return;
        }
        Assert.fail();
    }
}
