package org.jetbrains.emacs4ij.jelisp.exception;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/15/11
 * Time: 7:21 PM
 * To change this template use File | Settings | File Templates.
 */

@Error("args-out-of-range")
public class ArgumentOutOfRange extends LispException {
    private static String fromArray (Object... arguments) {
        if (arguments.length < 1)
            throw new RuntimeException("You must specify arguments for args-out-of-range error!");
        StringBuilder builder = new StringBuilder(arguments[0].toString());
        for (int i = 1, argumentsLength = arguments.length; i < argumentsLength; i++) {
            builder.append(' ').append(arguments[i].toString());
        }
        return builder.toString();
    }

    public ArgumentOutOfRange (Object... arguments) {
        super("'(args-out-of-range " + fromArray(arguments) + ')');
    }
}
