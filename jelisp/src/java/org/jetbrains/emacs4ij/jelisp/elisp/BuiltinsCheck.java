package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/2/11
 * Time: 5:02 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class BuiltinsCheck {
    private BuiltinsCheck() {}

    @Subroutine("stringp")
    public static LispObject stringp (LObject arg) {
        return (arg instanceof LispString) ? LispSymbol.ourT : LispSymbol.ourNil;
    }

    @Subroutine("symbolp")
    public static LispObject symbolp (LObject arg) {
        return (arg instanceof LispSymbol) ? LispSymbol.ourT : LispSymbol.ourNil;
    }

    @Subroutine("integerp")
    public static LispObject integerp (LObject arg) {
        return (arg instanceof LispInteger) ? LispSymbol.ourT : LispSymbol.ourNil;
    }

    @Subroutine("subrp")
    public static LispObject subrp (LispObject functionCell) {
        if (functionCell == null || !(functionCell instanceof FunctionCell))
            return LispSymbol.ourNil;
        FunctionCell.Type type = ((FunctionCell) functionCell).getType();
        if (type == FunctionCell.Type.BuiltIn || type == FunctionCell.Type.SpecialForm)
            return LispSymbol.ourT;
        return LispSymbol.ourNil;
    }

    //todo: it's a compliled lisp function
    @Subroutine("functionp")
    public static LispObject functionp (Environment environment, LObject function) {
        if (function == null)
            return LispSymbol.ourNil;
        if (function instanceof LispList) {
            try {
                new Lambda((LispList) function, environment);
                return LispSymbol.ourT;
            } catch (Exception e) {
                return LispSymbol.ourNil;
            }
        }
        FunctionCell functionCell = null;
        if (function instanceof LispSymbol) {
            LispSymbol f = environment.find(((LispSymbol) function).getName());
            if (f == null || f.getFunctionCell() == null)
                return LispSymbol.ourNil;
            if (f.isSubroutine())
                return LispSymbol.ourNil;
            functionCell = f.getFunctionCell();
        }
        if (functionCell == null) {
            try {
                functionCell = (FunctionCell) function;
            } catch (ClassCastException e) {
                return LispSymbol.ourNil;
            }
        }
        FunctionCell.Type type = functionCell.getType();
        if (type == FunctionCell.Type.BuiltIn || type == FunctionCell.Type.CustomFunction)
            return LispSymbol.ourT;
        return LispSymbol.ourNil;
    }

    @Subroutine("bufferp")
    public static LispObject bufferp (LObject arg) {
        return (arg instanceof LispBuffer) ? LispSymbol.ourT : LispSymbol.ourNil;
    }

    @Subroutine("commandp")
    public static LispSymbol commandp (Environment environment, LObject function, @Optional LObject forCallInteractively) {
        if (function instanceof LispSymbol) {
            if (!(((LispSymbol) function).getProperty("interactive-form").equals(LispSymbol.ourNil)))
                return LispSymbol.ourT;
            if (((LispSymbol) function).isCustom()) {
                ((LispSymbol) function).castToLambda(environment);
                return commandp(environment, ((LispSymbol) function).getFunction(), forCallInteractively);
            }
            if (((LispSymbol) function).isSubroutine()) {
                if (((LispSymbol) function).isInteractive())
                    return LispSymbol.ourT;
            }

            //todo: autoload objects
            // http://www.gnu.org/s/emacs/manual/html_node/elisp/Interactive-Call.html
        }
        if (function instanceof Lambda) {
            return ((Lambda) function).isInteractive() ? LispSymbol.ourT : LispSymbol.ourNil;
        }

        if (forCallInteractively == null || forCallInteractively.equals(LispSymbol.ourNil)) {
            // do not accept keyboard macros: string and vector
            return LispSymbol.ourNil;
        }
        if (function instanceof LispString) {
            //todo: check
            return LispSymbol.ourNil;
        }
        if (function instanceof LispVector) {
            //todo: check
            return LispSymbol.ourNil;
        }
        return LispSymbol.ourNil;
    }

    @Subroutine("buffer-live-p")
    public static LispSymbol bufferLivePredicate (LObject object) {
        if (bufferp(object).equals(LispSymbol.ourT))
            return (LispSymbol) ((LispBuffer)object).getLocalVariableValue("is-alive");
        return LispSymbol.ourNil;
    }

    @Subroutine("fboundp")
    public static LispSymbol fboundp (Environment environment, LispSymbol symbol) {
        LispSymbol f = environment.find(symbol.getName());
        if (f == null || f.getFunctionCell() == null)
            return LispSymbol.ourNil;
        return LispSymbol.ourT;
    }


}
