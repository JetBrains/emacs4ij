package org.jetbrains.emacs4ij.jelisp;

import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: ekaterina.polishchuk
 * Date: 7/8/11
 * Time: 5:21 PM
 * To change this template use File | Settings | File Templates.
 */
public class InterpreterTest {
    @Test
    public void testNoArg() throws Exception {
        Interpreter.main(new String[] {});
    }
    @Test
    public void testOneCodeArg() throws Exception {
        Interpreter.main(new String[] {"'(+ 2 3)"});
    }
    @Test
    public void testOneFileArg() throws Exception {
        Interpreter.main(new String[] {"test.lisp"});
    }
    @Test
    public void testMultipleCodeArgs() throws Exception {
        Interpreter.main(new String[] {"'(+", "2", "3)"});
    }
}
