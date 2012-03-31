package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/31/12
 * Time: 2:52 PM
 * To change this template use File | Settings | File Templates.
 */
public class DirectEvaluationException extends LispException {
    public DirectEvaluationException (String type) {
        super(JelispBundle.message("direct.evaluation.error", type));
    }
}
