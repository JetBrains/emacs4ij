package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;

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
    public static LispObject subrp (LObject functionCell) {
        if (functionCell == null || !(functionCell instanceof FunctionCell))
            return LispSymbol.ourNil;
        if (functionCell instanceof Primitive)
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
            if (!((LispSymbol) function).isFunction())
                return LispSymbol.ourNil;

            if (((LispSymbol) function).isInteractive(environment))
                return LispSymbol.ourT;
            else
                return LispSymbol.ourNil;

            //todo: autoload objects
            // http://www.gnu.org/s/emacs/manual/html_node/elisp/Interactive-Call.html
        }

        if (function instanceof Lambda || function instanceof Primitive) {
            return ((FunctionCell) function).isInteractive() ? LispSymbol.ourT : LispSymbol.ourNil;
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
        if (f == null || !f.isFunction())
            return LispSymbol.ourNil;
        return LispSymbol.ourT;
    }
    
    @Subroutine("byte-code-function-p")
    public static LispSymbol byteCodeFunctionP (LObject object) {
        return LispSymbol.ourNil;
    }

    @Subroutine("listp")
    public static LispSymbol listp (LObject object) {
        if (object instanceof LispList || object instanceof ConsCell || object.equals(LispSymbol.ourNil))
            return LispSymbol.ourT;
        return LispSymbol.ourNil;
    }

    @Subroutine("consp")
    public static LispSymbol consp (LObject object) {
        if (object instanceof ConsCell)
            return LispSymbol.ourT;
        return LispSymbol.ourNil;
    }

}
