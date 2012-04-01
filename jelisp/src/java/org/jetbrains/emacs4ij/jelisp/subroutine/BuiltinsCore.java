package org.jetbrains.emacs4ij.jelisp.subroutine;

import com.intellij.openapi.util.text.StringUtil;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinPredicates.isNil;
import static org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinPredicates.subrp;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/13/11
 * Time: 3:49 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class BuiltinsCore {
    private BuiltinsCore() {}

    public static LispObject thisOrNil (@Nullable LispObject object) {
        return object == null ? LispSymbol.ourNil : object;
    }

    public static void error (String message) {
        error(message, new LispObject[0]);
    }

    public static void error (String message, LispObject... args) {
        ArrayList<LispObject> data = new ArrayList<>();
        data.add(new LispSymbol("error"));
        data.add(new LispString(message));
        if (args.length > 0) {
            data.addAll(Arrays.asList(args));
        }
        LispList.list(data).evaluate(GlobalEnvironment.INSTANCE);
    }

    @Subroutine("set")
    public static LispObject set (Environment environment, LispSymbol variable, LispObject initValue) {
        LispObject value = (initValue == null) ? LispSymbol.ourVoid : initValue;
        LispSymbol symbol = new LispSymbol(variable.getName(), value);
        environment.setVariable(symbol);
        return value;
    }

    public static boolean equals (LispObject one, LispObject two) {
        return one.equals(two);
    }

    @Subroutine("equal")
    public static LispObject equal (LispObject one, LispObject two) {
        return LispSymbol.bool(equals(one, two));
    }

    public static boolean eqs (LispObject one, LispObject two) {
        if (one == two) return true;
        if (one.getClass() != two.getClass()) return false;
        if (one instanceof LispNumber) {
            return (((LispNumber) one).getData()  == ((LispNumber) two).getData());
        }
        if (one instanceof LispSymbol) {
            return ((LispSymbol) one).getName().equals(((LispSymbol) two).getName());
        }
        return one instanceof LispString
                && ((LispString) one).getData().equals("")
                && ((LispString) two).getData().equals("");
        //all other types eq by reference
    }

    @Subroutine("eq")
    public static LispObject eq (LispObject one, LispObject two) {
        return LispSymbol.bool(eqs(one, two));
    }

    @Subroutine("null")
    public static LispObject lispNull (LispObject lObject) {
        return LispSymbol.bool(lObject.equals(LispSymbol.ourNil));
    }

    @Subroutine("not")
    public static LispObject lispNot (LispObject lObject) {
        return lispNull(lObject);
    }

    @Subroutine("call-interactively")
    public static void callInteractively (Environment environment, LispSymbol symbol, @Nullable @Optional LispObject recordFlag, @Nullable LispObject keys) {
        LispSymbol function = environment.find(symbol.getName());
        if (function == null)
            function = symbol;
        if (!BuiltinPredicates.commandp(function, null).equals(LispSymbol.ourT))
            throw new WrongTypeArgumentException("commandp", function.getName());
        if (StringUtil.isEmptyOrSpaces(function.getInteractiveString())) {
            LispList.list(function).evaluate(environment);
            return;
        }
        LispMiniBuffer miniBuffer = environment.getMiniBuffer();
        miniBuffer.onInteractiveCall(environment, function);
    }

    @Subroutine("funcall")
    public static LispObject functionCall (Environment environment, LispObject function, @Optional LispObject... args) {
        environment.setArgumentsEvaluated(true);
        List<LispObject> data = new ArrayList<>();
        data.add(function);
        Collections.addAll(data, args);
        LispList funcall = LispList.list(data);
        return funcall.evaluate(environment);
    }

    @Subroutine("signal")
    public static LispObject signal (LispSymbol errorSymbol, LispList data) {
//        LispObject errorMessage = errorSymbol.getProperty("error-message");
        String msg = "";// '[' + ((errorMessage instanceof LispString) ? ((LispString) errorMessage).getData() : "peculiar error") + "] ";
        msg += '(' + errorSymbol.getName() + ' ';
        msg += (data.length() == 1 ? data.car().toString() : data.toString()) + ')';
//        System.out.println(msg);
        throw new LispException(msg);
    }

    private static void runFunction (Environment environment, LispSymbol function) {
        if (function.equals(LispSymbol.ourNil))
            return;
        if (!function.isFunction()) {
            throw new InvalidFunctionException(function.getName());
        }
        function.evaluateFunction(environment, new ArrayList<LispObject>());
    }

    @Subroutine("run-hooks")
    public static LispObject runHooks (Environment environment, @Optional LispSymbol... hooks) {
        if (hooks == null)
            return LispSymbol.ourNil;
        for (LispSymbol hook: hooks) {
            LispSymbol tHook = environment.find(hook.getName());
            if (tHook == null || tHook.equals(LispSymbol.ourNil))
                continue;
            if (hook.getValue() instanceof LispSymbol) {
                runFunction(environment, (LispSymbol) hook.getValue());
                continue;
            }
            if (hook.getValue() instanceof LispList) {
                for (LispObject function: ((LispList) hook.getValue()).toLispObjectList()) {
                    if (!(function instanceof LispSymbol))
                        throw new WrongTypeArgumentException("symbolp", function);

                    LispSymbol tFunction = environment.find(((LispSymbol)function).getName());
                    runFunction(environment, tFunction);
                }
                continue;
            }
            throw new InvalidFunctionException(hook.getValue().toString());
        }
        return LispSymbol.ourNil;
    }

    @Subroutine("macroexpand")
    public static LispObject macroExpand (Environment environment, LispObject macroCall) {
        if (!(macroCall instanceof LispList))
            return macroCall;
        LispSymbol macro;
        try {
            macro = (LispSymbol) ((LispList) macroCall).car();
        } catch (ClassCastException e) {
            return macroCall;
        }
        LispSymbol trueMacro = environment.find(macro.getName());
        if (!trueMacro.isMacro())
            return macroCall;

        return trueMacro.macroExpand(environment, ((LispList) ((LispList) macroCall).cdr()).toLispObjectList());
    }

    @Subroutine("fset")
    public static LispObject functionSet (Environment environment, LispSymbol symbol, LispObject function) {
        symbol.setFunction(function);
        environment.setVariable(symbol);
        return function;
    }

    @Subroutine("indirect-function")
    public static LispObject indirectFunction (LispObject object, @Optional LispObject noError) {
        if (!(object instanceof LispSymbol)) {
            return object;
        }
        LispSymbol symbol = (LispSymbol) object;
        ArrayList<String> examined = new ArrayList<String>();
        examined.add(symbol.getName());

        while (true) {
            if (!symbol.isFunction()) {
                if (noError != null && !noError.equals(LispSymbol.ourNil))
                    return LispSymbol.ourNil;
                throw new VoidFunctionException(((LispSymbol) object).getName());
                //return signalOrNot(noError, "void-function", symbol.getName());
            }
            LispObject f = symbol.getFunction();
            if (f instanceof LispSymbol) {
                if (examined.contains(((LispSymbol) f).getName())) {
                    if (noError != null && !noError.equals(LispSymbol.ourNil))
                        return LispSymbol.ourNil;
                    throw new CyclicFunctionIndirectionException(symbol.getName());

                    //return signalOrNot(noError, "cyclic-function-indirection", symbol.getName());
                }
                symbol = (LispSymbol) f;
                examined.add(symbol.getName());
                continue;
            }
            return f;
        }
    }

    @Subroutine("subr-arity")
    public static LispObject subrArity (LispObject object) {
        if (subrp(object).equals(LispSymbol.ourNil))
            throw new WrongTypeArgumentException("subrp",
                    object instanceof LispSymbol ? ((LispSymbol) object).getName() : object.toString());
        Primitive subr = (Primitive)object;
        return LispList.cons(new LispInteger(subr.getNRequiredArguments()), subr.getMaxNumArgs());
    }

    @Subroutine("aref")
    public static LispObject aRef (LispArray array, LispInteger index) {
        try {
            return array.getItem(index.getData());
            //todo: bool-vector
//            throw new WrongTypeArgumentException("arrayp", array.toString());
        } catch (IndexOutOfBoundsException e) {
            throw new ArgumentOutOfRange(array.toString(), index.toString());
        }
    }

    @Subroutine("aset")
    public static LispObject aSet (LispArray array, LispInteger index, LispObject value) {
        try {
            array.setItem(index.getData(), value);
            return value;
            //todo: bool-vector
//            throw new WrongTypeArgumentException("arrayp", array.toString());
        } catch (IndexOutOfBoundsException e) {
            throw new ArgumentOutOfRange(array.toString(), index.toString());
        }
    }

    @Subroutine(value = "apply")
    public static LispObject apply (Environment environment, LispObject function, LispObject... args) {
        if (!(function instanceof LispSymbol) || !((LispSymbol) function).isFunction()
                || (!((LispSymbol) function).isCustom() && !((LispSymbol) function).isBuiltIn()))
            throw new InvalidFunctionException(function.toString());

        if (!(args[args.length-1] instanceof LispList) && !args[args.length-1].equals(LispSymbol.ourNil))
            throw new WrongTypeArgumentException("listp", args[args.length-1]);
        ArrayList<LispObject> list = new ArrayList<>();
        list.addAll(Arrays.asList(args).subList(0, args.length - 1));

        if (!args[args.length-1].equals(LispSymbol.ourNil)) {
            List<LispObject> last = ((LispList)args[args.length-1]).toLispObjectList();
            list.addAll(last);
        }
        environment.setArgumentsEvaluated(true);
        return ((LispSymbol) function).evaluateFunction(environment, list);
    }

    @Subroutine(value = "purecopy")
    public static LispObject pureCopy (LispObject object) {
        return object;
    }

    @Subroutine(value = "eval")
    public static LispObject evaluate (Environment environment, LispObject object) {
        return object.evaluate(environment);
    }

    @Subroutine("defalias")
    public static LispObject defineAlias (LispSymbol symbol, LispObject functionDefinition, @Optional LispObject docString) {
        LispSymbol real = GlobalEnvironment.INSTANCE.find(symbol.getName());
        if (real == null)
            real = new LispSymbol(symbol.getName());
        real.setFunction(functionDefinition);
        if (docString != null && !(docString instanceof LispNumber)) {
            real.setFunctionDocumentation(docString);
        }
        GlobalEnvironment.INSTANCE.defineSymbol(real);
        return functionDefinition;
    }

    @Subroutine("provide")
    public static LispSymbol provide (LispSymbol feature, @Optional LispObject subFeatures) {
        //todo: implement
        return feature;
    }

    @Subroutine("atom")
    public static LispSymbol atom (LispObject object) {
        return LispSymbol.bool(!(object instanceof LispList));
    }
    
    @Subroutine("throw")
    public static void lispThrow (LispObject tag, LispObject value) {
        throw new LispThrow(tag, value);
    }
    
    @Subroutine("identity")
    public static LispObject identity (LispObject arg) {
        return arg;
    }
    
    @Subroutine("match-data")
    public static LispObject matchData(@Optional LispObject integers, LispObject reuse, LispObject reseat) {
        //todo :)
        return LispSymbol.ourNil;
    }

    private static LispInteger getInt (LispObject object) {
        if (!(object instanceof LispInteger))
            throw new WrongTypeArgumentException("integerp", object);
        return (LispInteger) object;
    }

    private static int processBound (LispInteger bound, int length) {
        return bound.getData() < 0 ? length + bound.getData() : bound.getData();
    }

    @Subroutine("substring")
    public static LispObject substring (StringOrVector stringOrVector, LispInteger from, @Optional LispObject to) {
        int length = stringOrVector.length();
        int start = processBound(from, length);        
        int end = BuiltinPredicates.isNil(to)
                ? length
                : processBound(getInt(to), length);
        try {
            return stringOrVector.substring(start, end);
        } catch (IndexOutOfBoundsException e) {
            throw new ArgumentOutOfRange(stringOrVector, start, end);
        }
    }

    @Subroutine(value = "execute-extended-command", isCmd = true, interactive = "CM-x ", key = "\\M-x")
    public static void executeExtendedCommand (Environment environment, LispObject prefixArg) {
        LispBuffer buffer = environment.getBufferCurrentForEditing();
        buffer.setActive();
        LispMiniBuffer miniBuffer = environment.getMiniBuffer();
        miniBuffer.open(buffer);
    }

    public static boolean isSyntaxTable (LispObject object) {
        //todo: true if char-table
        return false;
    }

    @Subroutine("syntax-table-p")
    public static LispSymbol syntaxTableP (LispObject object) {
        return LispSymbol.bool(isSyntaxTable(object));
    }

    @Subroutine("set-syntax-table")
    public static void setSyntaxTable (LispObject syntaxTable) {
        if (!isSyntaxTable(syntaxTable))
            throw new WrongTypeArgumentException("syntax-table-p", syntaxTable.toString());
        //todo ?
    }

    private static LispString print (Environment environment, LispObject object,
                                     @Optional LispObject printCharFun, boolean quoteStrings) {
        LispString result = quoteStrings 
                ? new LispString(object.toString()) 
                : object instanceof LispString ? (LispString) object : new LispString(object.toString());
        LispObject toInsert = object instanceof LispString ? result : object;
        if (isNil(printCharFun))
            printCharFun = environment.find("standard-output").getValue();
        if (printCharFun instanceof LispBuffer) {
            ((LispBuffer) printCharFun).insert(toInsert);
        } else if (printCharFun instanceof LispMarker) {
            ((LispMarker) printCharFun).insert(toInsert);
        } else if (printCharFun.equals(LispSymbol.ourT)) {
            GlobalEnvironment.showInfoMessage(object.toString());
        } else {
            for (LispObject character: result.toLispObjectList()) {
                functionCall(environment, printCharFun, character);
            }
        }
        return result;
    }
    
    @Subroutine("prin1")
    public static LispString prin1 (Environment environment, LispObject object, @Optional LispObject printCharFun) {
        return print(environment, object, printCharFun, true);
    }

    @Subroutine("princ")
    public static LispString princ (Environment environment, LispObject object, @Optional LispObject printCharFun) {
        return print(environment, object, printCharFun, false);
    }
}