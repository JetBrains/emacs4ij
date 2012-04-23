package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/13/12
 * Time: 4:48 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class Scan {
    private Scan() {}

    @Subroutine("scan-sexps")
    public static LispObject scanSexps (Environment environment, LispInteger from, LispInteger count) {
        return scanLists(environment, from.getData(), count.getData(), 0, true);
    }

    @Subroutine("scan-lists")
    public static LispObject scanLists (Environment environment, LispInteger from, LispInteger count, LispInteger depth) {
        return scanLists(environment, from.getData(), count.getData(), depth.getData(), false);
    }

    private static LispObject scanLists (Environment environment, int from, int count, int depth, boolean sexpFlag) {
        int minDepth = depth > 0 ? depth : 0;
        if (from < environment.getBufferCurrentForEditing().pointMin())
            from = environment.getBufferCurrentForEditing().pointMin();
        if (from > environment.getBufferCurrentForEditing().pointMax())
            from = environment.getBufferCurrentForEditing().pointMax();

        boolean ignoreComments = environment.find("parse-sexp-ignore-comments").getValue().equals(LispSymbol.ourT);

        if (count > 0)
            return scanForward(environment, from, depth, minDepth, count, sexpFlag, ignoreComments);
        if (count < 0)
            return scanBackward(environment, from);
        return LispSymbol.ourNil;
    }

    private static int getCharFullSyntaxCode (Environment environment, int c) {
        LispObject car = SyntaxTable.getSyntaxTable(environment).getCharSyntax(c).car();
        return  car instanceof LispInteger
                ? ((LispInteger) car).getData()
                : SyntaxDescriptor.getSyntaxClass(SyntaxDescriptor.ClassType.WHITESPACE);
    }

    private static LispObject scanForward (Environment environment, int from, int depth, int minDepth,
                                           int count, boolean sexpFlag, boolean ignoreComments) {
        int finish = environment.getBufferCurrentForEditing().pointMax();
        while (from < finish) {
            //todo
        }

        return new LispInteger(from); // or nil
    }

    private static LispObject scanBackward (Environment environment, int from) {
        int finish = environment.getBufferCurrentForEditing().pointMin();
        while (from > finish) {
            //todo
        }
        return new LispInteger(from); // or nil
    }

    //todo: compiled lisp f
    @Subroutine(value = "eval-last-sexp", isCmd = true, interactive = "P", key = "\\C-x\\C-e")
    public static void evalLastSexp (Environment environment, LispObject evalLastSexpArgInternal) {
        try {
            LispBuffer buffer = environment.getBufferCurrentForEditing();
            LispObject result = buffer.evaluateLastForm();
            if (result != null)
                GlobalEnvironment.echoMessage(result.toString() + "\n");
        } catch (LispException exc) {
            GlobalEnvironment.echoError(exc.getMessage() + "\n");
        }
    }

}
