package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/2/11
 * Time: 5:02 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class BuiltinPredicates {
    private BuiltinPredicates() {}

    public static boolean isCharacter(LispObject object) {
        if (!isWholeNumber(object))
            return false;
        Integer data = ((LispInteger) object).getData();
        return data <= CharUtil.MAX_CHAR;
    }

    public static boolean isString (LispObject object) {
        return object instanceof LispString;
    }

    public static boolean isCharOrString (LispObject object) {
        return isCharacter(object) || isString(object);
    }

    @Subroutine("stringp")
    public static LispSymbol stringp (LispObject arg) {
        return LispSymbol.bool(isString(arg));
    }

    @Subroutine("symbolp")
    public static LispSymbol symbolp (LispObject arg) {
        return LispSymbol.bool(arg instanceof LispSymbol);
    }

    @Subroutine("integerp")
    public static LispSymbol integerp (LispObject arg) {
        return LispSymbol.bool(arg instanceof LispInteger);
    }

    @Subroutine("subrp")
    public static LispObject subrp (LispObject functionCell) {
        if (functionCell == null || !(functionCell instanceof FunctionCell))
            return LispSymbol.ourNil;
        if (functionCell instanceof Primitive)
            return LispSymbol.ourT;
        return LispSymbol.ourNil;
    }

    @Subroutine("bufferp")
    public static LispSymbol bufferp (LispObject arg) {
        return LispSymbol.bool(arg instanceof LispBuffer);
    }

    @Subroutine("commandp")
    public static LispSymbol commandp (Environment environment, LispObject function, @Nullable @Optional LispObject forCallInteractively) {
        if (function instanceof LispSymbol) {
            if (((LispSymbol) function).getName().equals("forward-char"))
                System.out.print(1);


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
    public static LispSymbol bufferLivePredicate (Environment environment, LispObject object) {
        if (object instanceof LispBuffer) {
            return LispSymbol.bool(!environment.isBufferDead(((LispBuffer) object).getName()));
        }
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
    public static LispSymbol byteCodeFunctionP (LispObject object) {
        return LispSymbol.ourNil;
    }

    @Subroutine("framep")
    public static LispSymbol framep (LispObject object) {
        if (object instanceof LispFrame) {
            String os = System.getProperty("os.name").toLowerCase();
            if (os.contains("win"))
                return new LispSymbol("w32");
            if (os.contains("mac"))
                return new LispSymbol("mac");
            if (os.contains("nix") || os.contains("nux"))
                return new LispSymbol("x");
            //todo: ?? `ns' for an Emacs frame on a GNUstep or Macintosh Cocoa display,
            return LispSymbol.ourT;
        }
        return LispSymbol.ourNil;
    }

    @Subroutine("frame-live-p")
    public static LispSymbol frameLiveP (LispObject object) {
        LispSymbol frameP = framep(object);
        if (frameP.equals(LispSymbol.ourNil))
            return LispSymbol.ourNil;
        if (GlobalEnvironment.isFrameAlive((LispFrame) object))
            return frameP;
        return LispSymbol.ourNil;
    }

    @Subroutine("frame-visible-p")
    public static LispSymbol frameVisibleP (LispObject object) {
        LispSymbol frameLiveP = frameLiveP(object);
        if (frameLiveP.equals(LispSymbol.ourNil))
            throw new WrongTypeArgumentException("frame-live-p", object);
        if (((LispFrame) object).isIconified())
            return new LispSymbol("icon");
        if (((LispFrame) object).isVisible())
            return LispSymbol.ourT;
        return LispSymbol.ourNil;
    }

    @Subroutine("windowp")
    public static LispSymbol windowP (LispObject object) {
        return LispSymbol.bool (object instanceof LispWindow);
    }
    
    @Subroutine("number-or-marker-p") 
    public static LispSymbol numberOrMarkerP (LispObject object) {
        return LispSymbol.bool (object instanceof LispNumber || object instanceof LispMarker);
    }

    @Subroutine("integer-or-marker-p")
    public static LispSymbol integerOrMarkerP (LispObject object) {
        return LispSymbol.bool (object instanceof LispInteger || object instanceof LispMarker);
    }

    @Subroutine("markerp")
    public static LispSymbol markerP (LispObject object) {
        return LispSymbol.bool(object instanceof LispMarker);
    }

    @Subroutine("keywordp")
    public static LispSymbol keywordP (LispObject object) {
        return LispSymbol.bool (object instanceof LispSymbol && ((LispSymbol) object).getName().startsWith(":"));
    }

    @Subroutine("default-boundp")
    public static LispSymbol defaultBoundP (Environment environment, LispSymbol symbol) {
        try {
            BuiltinsSymbol.defaultValue(environment, symbol);
        } catch (VoidVariableException e) {
            return LispSymbol.ourNil;
        }
        return LispSymbol.ourT;
    }
    
    @Subroutine("vectorp")
    public static LispSymbol vectorP (LispObject object) {
        return LispSymbol.bool(object instanceof LispVector);
    }
    
    @Subroutine("characterp")
    public static LispSymbol characterP (LispObject object, @Optional LispObject ignore) {
        return LispSymbol.bool(isCharacter(object));
    }

    @Subroutine("char-or-string-p")
    public static LispSymbol charOrStringP (LispObject object) {
        return LispSymbol.bool(isCharOrString(object));
    }
    
    @Subroutine("numberp")
    public static LispSymbol numberP (LispObject object) {
        return LispSymbol.bool(object instanceof LispNumber);
    }
    
    public static boolean isWholeNumber(LispObject object) {
        return (object instanceof LispInteger) && (((LispInteger) object).getData() > -1);
    }
    
    @Subroutine("wholenump")
    public static LispSymbol wholeNumP (LispObject object) {
        return LispSymbol.bool(isWholeNumber(object));
    }
    
    public static LispObject eventHead (LispObject event) {
        return event instanceof LispList ? ((LispList) event).car() : event;
    }

}
