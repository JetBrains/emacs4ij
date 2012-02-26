package org.jetbrains.emacs4ij.jelisp.subroutine;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.ForwardParser;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.TestSetup;
import org.jetbrains.emacs4ij.jelisp.elisp.LObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 26/02/12
 * Time: 18:19
 * To change this template use File | Settings | File Templates.
 */
public class BuiltinsStringTest {
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

    private LObject evaluateString (String lispCode) throws LispException {
        ForwardParser forwardParser = new ForwardParser();
        LObject object = forwardParser.parseLine(lispCode);
        return object.evaluate(environment);
    }

    @Test
    public void testFormatEmpty() {
        LObject s = evaluateString("(format \"\")");
        Assert.assertEquals(new LispString(""), s);
    }

    @Test
    public void testFormatNoNeedInArgs() {
        LObject s = evaluateString("(format \"hello\" 1 2 3)");
        Assert.assertEquals(new LispString("hello"), s);
    }

    @Test
    public void testFormatNotEnoughArgs() {
        try {
            evaluateString("(format \"%d\")");
        } catch (Exception e) {
            Assert.assertEquals("Not enough arguments for format string", TestSetup.getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }

    @Test
    public void testFormatUnexpectedEndOfFormatString() {
        try {
            evaluateString("(format \"%123456\" 1)");
        } catch (Exception e) {
            Assert.assertEquals("Format string ends in middle of format specifier", TestSetup.getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }

    @Test
    public void testFormatInvalidFormatCharacter() {
        try {
            evaluateString("(format \"%123456q\" 1)");
        } catch (Exception e) {
            Assert.assertEquals("Invalid format operation %q", TestSetup.getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }

    @Test
    public void testFormatSimple() {
        LObject s = evaluateString("(format \"%1234d\" 1 2 3)");
        Assert.assertEquals(new LispString("1"), s);
    }

    @Test
    public void testStringMatchSimple() {
        LObject r = evaluateString("(string-match \"a\" \"africa\")");
        Assert.assertEquals(new LispInteger(0), r);
        r = evaluateString("(string-match \"A\" \"africa\" 1)");
        Assert.assertEquals(new LispInteger(5), r);
        evaluateString("(setq case-fold-search nil)");
        r = evaluateString("(string-match \"A\" \"africa\")");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }

    @Test
    public void testStringMatchOutOfRange() {
        try {
            evaluateString("(string-match \"a\" \"africa\" 10)");
        } catch (Exception e) {
            Assert.assertEquals("'(args-out-of-range \"africa\" 10)", TestSetup.getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }

    @Test
    public void testStringMatchTick() {
        LObject r = evaluateString("(string-match \"^[ACHMsS]-.\" \"M-x\")");
        Assert.assertEquals(new LispInteger(0), r);
        r = evaluateString("(string-match \"^[ACHMsS]-.\" \"kvaM-x\")");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }

    @Test
    public void testStringMatchTickAfterNewLine() {
        LObject r = evaluateString("(string-match \"^[ACHMsS]-.\" \"kva\\\\nM-x\")");
        Assert.assertEquals(new LispInteger(4), r);
    }

    @Test
    public void testStringMatchCharAlternative() {
        LObject r = evaluateString("(string-match \"[abc]\" \"d\\\\^alla\")");
        Assert.assertEquals(new LispInteger(4), r);
    }

    @Test
    public void testStringMatchStar() {
        LObject r = evaluateString("(string-match \"hi*\" \"hihihi5\")");
        Assert.assertEquals(new LispInteger(0), r);
        r = evaluateString("(string-match \"a*\" \"sdf\")");
        Assert.assertEquals(new LispInteger(0), r);
    }

    @Test
    public void testStringMatchBackSlash() {
        LObject r = evaluateString("(string-match \"a\\\\\\\\.b\" \"a\\\\.b\")");
        Assert.assertEquals(new LispInteger(0), r);
    }

    @Test
    public void testStringMatchQuestion() {
        LObject r = evaluateString("(string-match \"caca?r\" \"cacr\")");
        Assert.assertEquals(new LispInteger(0), r);
        r = evaluateString("(string-match \"a?\" \"crtcr\")");
        Assert.assertEquals(new LispInteger(0), r);
    }

    @Test
    public void testStringMatchDollar() {
        LObject r = evaluateString("(string-match \"u?hhhh$\" \"ah\")");
        Assert.assertEquals(LispSymbol.ourNil, r);
        r = evaluateString("(string-match \"u?h$\" \"ah\")");
        Assert.assertEquals(new LispInteger(1), r);
    }

    @Test
    public void testStringMatchDot() {
        LObject r = evaluateString("(string-match \"[.]\" \"a.\")");
        Assert.assertEquals(new LispInteger(1), r);
        r = evaluateString("(string-match \".\" \"a.\")");
        Assert.assertEquals(new LispInteger(0), r);
    }

    @Test
    public void testStringMatch() {
        LObject r = evaluateString("(string-match \"^[ACHMsS]-.\" \"M-x\")");
        Assert.assertEquals(new LispInteger(0), r);
    }

    @Test
    public void testCapitalize() {
        LObject r = evaluateString("?a");
        Assert.assertEquals(new LispInteger(97), r);
        r = evaluateString("(capitalize ?a)");
        Assert.assertEquals(new LispInteger(65), r);
        r = evaluateString("(capitalize 97)");
        Assert.assertEquals(new LispInteger(65), r);
        r = evaluateString("(capitalize 5)");
        Assert.assertEquals(new LispInteger(5), r);
        r = evaluateString("(capitalize \"EVERY day\")");
        Assert.assertEquals(new LispString("Every Day"), r);
    }
}
