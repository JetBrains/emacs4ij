package org.jetbrains.emacs4ij.jelisp.elisp;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.Parser;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
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
        GlobalEnvironment.ourEmacsSource = "/home/kate/Downloads/emacs 23.2a/emacs-23.2";
        GlobalEnvironment.ourEmacsPath = "/usr/share/emacs/23.2";
        GlobalEnvironment.initialize(null, null);
        GlobalEnvironment.INSTANCE.startRecording();
    }

    @Before
    public void setUp() throws Exception {
        GlobalEnvironment.INSTANCE.clearRecorded();
        environment = new CustomEnvironment(GlobalEnvironment.INSTANCE);
    }

    private LObject evaluateString (String lispCode) throws LispException {
        Parser parser = new Parser();
        return parser.parseLine(lispCode).evaluate(environment);
    }

    private Throwable getCause (Throwable e) {
        if (e.getCause() == null)
            return e;
        return getCause(e.getCause());
    }

    @Test
    public void testParseArgumentsList() throws Exception {
        LispList list = (LispList) evaluateString("'(lambda (a &optional b &key c &rest d))");
        List<LambdaArgument> args = new Lambda(list, environment).getArguments();
        Assert.assertEquals(4, args.size());
        Assert.assertEquals(LambdaArgument.Type.REQUIRED, args.get(0).getType());
        Assert.assertEquals(LambdaArgument.Type.OPTIONAL, args.get(1).getType());
        Assert.assertEquals(LambdaArgument.Type.KEYWORD, args.get(2).getType());
        Assert.assertEquals(LambdaArgument.Type.REST, args.get(3).getType());
    }

    @Test
    public void testParseArgumentsList_Keyword_Simple() throws Exception {
        LispList list = (LispList) evaluateString("'(lambda (&key (c 10)))");
        List<LambdaArgument> args = new Lambda(list, environment).getArguments();
        Assert.assertEquals(1, args.size());
        Assert.assertEquals(new LispInteger(10), args.get(0).getInitForm());
        Assert.assertEquals(new LispSymbol(":c"), args.get(0).getKeyword());
        Assert.assertEquals(new LispSymbol("c"), args.get(0).getVar());
    }

    @Test
    public void testParseArguments_Keyword_Complex() throws Exception {
        LispList list = (LispList) evaluateString("'(lambda (&key ((word var) 10 a)))");
        List<LambdaArgument> args = new Lambda(list, environment).getArguments();
        Assert.assertEquals(1, args.size());
        Assert.assertEquals(new LispInteger(10), args.get(0).getInitForm());
        Assert.assertEquals(new LispSymbol("word"), args.get(0).getKeyword());
        Assert.assertEquals(new LispSymbol("var"), args.get(0).getVar());
        Assert.assertEquals(new LispSymbol("a"), args.get(0).getSetVar());
    }

    @Test
    public void testSubstituteArguments() throws Exception {
        LispList list = (LispList) evaluateString("'(lambda (&key ((word var) 10 a)))");
        List<LObject> args = ((LispList) evaluateString("'('word 5)")).toLObjectList();
        CustomEnvironment inner = new Lambda(list, environment).substituteArguments(environment, args);
        LispSymbol var = inner.find("var");
        Assert.assertNotNull(var);
        Assert.assertEquals(new LispInteger(5), var.getValue());
    }
}
