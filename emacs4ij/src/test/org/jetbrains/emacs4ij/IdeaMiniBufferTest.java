package org.jetbrains.emacs4ij;

import com.intellij.ui.EditorTextField;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.Parser;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgument;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/23/11
 * Time: 4:50 PM
 * To change this template use File | Settings | File Templates.
 */
public class IdeaMiniBufferTest {
    private IdeaMiniBuffer myMiniBuffer;
    private Environment myEnvironment;
    @Before
    public void setUp() throws Exception {
        myEnvironment = new Environment(new Environment());
        EditorTextField t = new EditorTextField();
        myMiniBuffer = new IdeaMiniBuffer(0, t.getEditor(), myEnvironment);
    }

    private LObject evaluateString (String lispCode) throws LispException {
        Parser parser = new Parser();
        return parser.parseLine(lispCode).evaluate(myEnvironment);
    }

    @Test (expected = WrongTypeArgument.class)
    public void testReturnDefault_Integer () {
        myMiniBuffer.returnDefault(new LispInteger(5));
    }

    @Test
    public void testReturnDefault_StringList () {
        LObject ret = myMiniBuffer.returnDefault (new LispList(new LispString("hi"), new LispInteger(5)));
        Assert.assertEquals(new LispSymbol("hi"), ret);
    }

    @Test
    public void testReturnDefault_String () {
        LObject ret = myMiniBuffer.returnDefault (new LispString("hi"));
        Assert.assertEquals(new LispSymbol("hi"), ret);
    }

    @Test (expected = WrongTypeArgument.class)
    public void testReturnDefault_IntList () {
        myMiniBuffer.returnDefault (new LispList(new LispInteger(5), new LispString("hi")));
    }

    @Test (expected = WrongTypeArgument.class)
    public void testReturnDefault_ListList () {
        myMiniBuffer.returnDefault (new LispList(new LispList(new LispString("wow"), new LispString("hi")), new LispInteger(5), new LispString("hi")));
    }

    @Test
    public void testReturnDefault_ExistingSymbol () {
        evaluateString("(setq s 5)");
        LObject ret = myMiniBuffer.returnDefault (new LispSymbol("s"));
        Assert.assertEquals(new LispSymbol("s", new LispInteger(5)), ret);
    }

    @Test
    public void testReturnDefault_NonExistingSymbol () {
        LObject ret = myMiniBuffer.returnDefault (new LispSymbol("s"));
        Assert.assertEquals(new LispSymbol("s"), ret);
    }

    @Test
    public void testReturnDefault_NilSymbol () {
        LObject ret = myMiniBuffer.returnDefault (LispSymbol.ourNil);
        Assert.assertEquals(new LispSymbol(""), ret);
    }

    @Test
    public void testReturnDefault_EmptyList () {
        LObject ret = myMiniBuffer.returnDefault (new LispList());
        Assert.assertEquals(new LispSymbol(""), ret);
    }


}
