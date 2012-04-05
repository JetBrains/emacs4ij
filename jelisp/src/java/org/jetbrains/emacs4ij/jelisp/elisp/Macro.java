package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.exception.DirectEvaluationException;
import org.jetbrains.emacs4ij.jelisp.exception.InternalException;

import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 11/24/11
 * Time: 7:46 PM
 * To change this template use File | Settings | File Templates.
 */
public class Macro implements FunctionCell {
    private Lambda myLambda;

    public Macro (LispList def) {
        List<LispObject> data = def.toLispObjectList();
        if (!data.get(0).equals(new LispSymbol("macro")))
            throw new InternalException(JelispBundle.message("wrong.def.form", "macro", def.toString()));
        myLambda = new Lambda(LispList.list(data.subList(1, data.size())));
    }

    public LispObject expand (Environment environment, List<LispObject> args) {
        return myLambda.evaluate(environment, args);
    }

    @Override
    public LispObject getDocumentation() {
        return myLambda.getDocumentation();
    }

    @Override
    public void setDocumentation(LispObject doc) {
        myLambda.setDocumentation(doc);
    }

    @Override
    public int getNRequiredArguments() {
        return myLambda.getNRequiredArguments();
    }

    @Override
    public LispObject evaluate(Environment environment) {
        throw new DirectEvaluationException("macro");
    }
}
