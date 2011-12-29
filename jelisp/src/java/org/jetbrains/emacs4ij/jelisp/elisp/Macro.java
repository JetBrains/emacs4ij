package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;

import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 11/24/11
 * Time: 7:46 PM
 * To change this template use File | Settings | File Templates.
 */
public class Macro extends LispObject implements FunctionCell {
   // private String myName;
    private Lambda myLambda;
    private LispList myDeclaration;

    public Macro (LispList def, CustomEnvironment environment) {
        List<LObject> data = def.toLObjectList();
        if (!data.get(0).equals(new LispSymbol("macro")))
            throw new RuntimeException("wrong macro definition");
        myLambda = new Lambda(LispList.list(data.subList(1, data.size())), environment);
    }

    public void setDeclaration(LispList declaration) {
        myDeclaration = declaration;
    }

    public LObject expand (CustomEnvironment environment, List<LObject> args) {
        return myLambda.evaluate(environment, args);
    }

    @Override
    public LispObject getDocString() {
        return myLambda.getDocString();
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
    public LObject evaluate(CustomEnvironment environment) {
        throw new RuntimeException("not impl");
    }
}
