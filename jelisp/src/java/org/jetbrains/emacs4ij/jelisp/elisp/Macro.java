package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;

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

    public Macro (LispList def, Environment environment) {
        List<LispObject> data = def.toLispObjectList();
        if (!data.get(0).equals(new LispSymbol("macro")))
            throw new InternalError("Wrong macro definition");
        myLambda = new Lambda(LispList.list(data.subList(1, data.size())), environment);
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
    public boolean isInteractive() {
        return false;
    }

    @Override
    public String getInteractiveString() {
        return null;
    }

    @Override
    public LispObject evaluate(Environment environment) {
        throw new InternalError("We can't come to macro evaluation: it is used as function or throws void-variable exc");
    }
}
