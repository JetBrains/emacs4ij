package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.exception.WrongNumberOfArgumentsException;import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: Kate
 * Date: 7/16/11
 * Time: 2:47 PM
 * To change this template use File | Settings | File Templates.
 */
public class LispSpecialForm extends LispObject {

    private String myName;

    public LispSpecialForm(String myName) {
        this.myName = myName;
    }

    public String getName() {
        return myName;
    }

    public LispObject execute (List<LispObject> args)throws WrongNumberOfArgumentsException {
        if (myName.equals("quote")) {
            if (args.size() != 1)
                throw new WrongNumberOfArgumentsException();
            return args.get(0);
        }

        throw new RuntimeException("no execute param");
    }

    @Override
    public LispString toLispString() {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }
}
