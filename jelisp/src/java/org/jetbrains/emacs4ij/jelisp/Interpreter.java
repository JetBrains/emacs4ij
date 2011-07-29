package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;

/**
 * Created by IntelliJ IDEA.
 * User: ekaterina.polishchuk
 * Date: 7/7/11
 * Time: 5:24 PM
 * To change this template use File | Settings | File Templates.
 */
public class Interpreter {

    public static LispObject interpret (String line) {
        Parser parser = new Parser();
        Environment environment = new Environment(Environment.ourGlobal);
        return parser.parseLine(line).evaluate(environment);
    }
}
