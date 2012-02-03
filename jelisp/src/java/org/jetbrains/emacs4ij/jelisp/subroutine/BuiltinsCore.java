package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

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

    private static LispNumber numberOrMarkerToNumber (LObject lispObject) {
        if (BuiltinPredicates.numberOrMarkerP(lispObject).equals(LispSymbol.ourNil))
            throw new WrongTypeArgumentException("number-or-marker-p", lispObject.toString());
        LispNumber n;
        if (BuiltinPredicates.markerP(lispObject).equals(LispSymbol.ourT)) {
            n = new LispInteger(((LispMarker)lispObject).getPosition());
        } else {
            n = (LispNumber) lispObject;
        }
        return n;
    }
    
    @Subroutine("+")
    public static LispNumber plus (@Optional LObject... args) {
        double ans = 0.0;
        boolean isDouble = false;
        if (args != null) {
            for (LObject lispObject: args) {
                LispNumber n = numberOrMarkerToNumber(lispObject);               
                if (!isDouble && (n.getData() instanceof Double))
                    isDouble = true;
                ans += n.getDoubleData();
            }
        }
        return isDouble ? LispNumber.newInstance(ans) : LispNumber.newInstance((int)ans);
    }
    
    @Subroutine("*")
    public static LispNumber multiply (@Optional LObject... args) {
        double ans = 1;
        boolean isDouble = false;
        for (LObject lispObject: args) {
            LispNumber n = numberOrMarkerToNumber(lispObject);            
            if (!isDouble && (n.getData() instanceof Double))
                isDouble = true;
            ans *= n.getDoubleData();
        }
        return isDouble ? LispNumber.newInstance(ans) : LispNumber.newInstance((int)ans);
    }
    
    @Subroutine(">")
    public static LispSymbol more (LObject num1, LObject num2) {
        LispNumber n1 = numberOrMarkerToNumber(num1);
        LispNumber n2 = numberOrMarkerToNumber(num2);
        if (n1.getDoubleData() > n2.getDoubleData())
            return LispSymbol.ourT;
        return LispSymbol.ourNil;
    }

    @Subroutine("set")
    public static LObject set (Environment environment, LispSymbol variable, LObject initValue) {
        LObject value = (initValue == null) ? LispSymbol.ourVoid : initValue;
        variable.setValue(value);
        environment.setVariable(variable);
        return value;
    }

    @Subroutine("equal")
    public static LispObject equal (LObject one, LObject two) {
        if (one.equals(two))
            return LispSymbol.ourT;
        return LispSymbol.ourNil;
    }

    /* eq returns t if object1 and object2 are integers with the same value.
    Also, since symbol names are normally unique, if the arguments are symbols with the same name, they are eq.
    For other types (e.g., lists, vectors, strings), two arguments with the same contents or elements are not necessarily eq to each
    other: they are eq only if they are the same object, meaning that a change in the contents of one will be reflected by the
    same change in the contents of the other.
    * */

    @Subroutine("eq")
    public static LispObject eq (LObject one, LObject two) {
        if (one == two) return LispSymbol.ourT;
        if (one.getClass() != two.getClass()) return LispSymbol.ourNil;
        if (one instanceof LispNumber) {
            return (((LispNumber) one).getData()  == ((LispNumber) two).getData()) ? LispSymbol.ourT : LispSymbol.ourNil;
        }
        if (one instanceof LispSymbol) {
            return ((LispSymbol) one).getName().equals(((LispSymbol) two).getName()) ? LispSymbol.ourT : LispSymbol.ourNil;
        }
        if ((one instanceof LispString) && (((LispString) one).getData().equals(""))) {
            return ((LispString) two).getData().equals("") ? LispSymbol.ourT : LispSymbol.ourNil;
        }
        return LispSymbol.ourNil;
    }

    @Subroutine("null")
    public static LispObject lispNull (LObject lObject) {
        return lObject.equals(LispSymbol.ourNil) ? LispSymbol.ourT : LispSymbol.ourNil;
    }

    @Subroutine("not")
    public static LispObject lispNot (LObject lObject) {
        return lispNull(lObject);
    }

    @Subroutine("call-interactively")
    public static LObject callInteractively (Environment environment, LispSymbol function, @Optional LObject recordFlag, LObject keys) {
        if (!BuiltinPredicates.commandp(environment, function, null).equals(LispSymbol.ourT))
            throw new WrongTypeArgumentException("commandp", function.getName());
        //read args
        //assign args
        //invoke function
        return LispSymbol.ourNil;

    }

    @Subroutine("funcall")
    public static LObject functionCall (Environment environment, LObject function, @Optional LObject... args) {
        environment.setArgumentsEvaluated(true);
        ArrayList<LObject> data = new ArrayList<LObject>();
        data.add(function);
        Collections.addAll(data, args);
        LispList funcall = LispList.list(data);
        return funcall.evaluate(environment);
    }

    @Subroutine("signal")
    public static LObject signal (LispSymbol errorSymbol, LispList data) {
        LObject errorMessage = errorSymbol.getProperty("error-message");
        String msg = '[' + errorSymbol.getName() + "] ";
        msg += (errorMessage instanceof LispString) ? ((LispString) errorMessage).getData() : "peculiar error";
        msg += ": " + data.toString();
//        GlobalEnvironment.showErrorMessage(msg);
        //todo: this method returns for test only
      //  return new LispString(msg);
        System.out.println(msg);
        throw new LispException(msg);
    }

    private static void runFunction (Environment environment, LispSymbol function) {
        if (function.equals(LispSymbol.ourNil))
            return;
        if (!function.isFunction()) {
            throw new InvalidFunctionException(function.toString());
        }
        function.evaluateFunction(environment, new ArrayList<LObject>());
    }

    @Subroutine("run-hooks")
    public static LObject runHooks (Environment environment, @Optional LispSymbol... hooks) {
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
                for (LObject function: ((LispList) hook.getValue()).toLObjectList()) {
                    if (!(function instanceof LispSymbol))
                        throw new WrongTypeArgumentException("symbolp", function.toString());

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
    public static LObject macroExpand (Environment environment, LObject macroCall) {
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

        return trueMacro.macroExpand(environment, ((LispList) ((LispList) macroCall).cdr()).toLObjectList());
    }

    @Subroutine("fset")
    public static LObject functionSet (Environment environment, LispSymbol symbol, LObject function) {
        symbol.setFunction(function);
        environment.setVariable(symbol);
        return function;
    }
    
   /* private static LObject signalOrNot (LObject noError, String name, String data) {
        if (noError != null && !noError.equals(LispSymbol.ourNil))
            return LispSymbol.ourNil;


        LispSymbol errorSymbol = new LispSymbol(name);
        errorSymbol.setProperty("error-message", new LispString(name));
        return signal(errorSymbol, new LispList(new LispSymbol(data)));    
    }*/
    
    @Subroutine("indirect-function")
    public static LObject indirectFunction (LObject object, @Optional LObject noError) {
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
            LObject f = symbol.getFunction();
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
    public static LObject subrArity (LObject object) {
        if (subrp(object).equals(LispSymbol.ourNil))
            throw new WrongTypeArgumentException("subrp",
                    object instanceof LispSymbol ? ((LispSymbol) object).getName() : object.toString());
        Primitive subr = (Primitive)object;
        return LispList.cons(subr.getMinNumArgs(), subr.getMaxNumArgs());
    }

    @Subroutine("aref")
    public static LObject aRef (LObject array, LispInteger index) {
        try {
            if (array instanceof LispVector) {
                return ((LispVector) array).get(index.getData());
            }
            if (array instanceof LispString) {
                return new LispInteger(((LispString) array).getData().charAt(index.getData()));
            }
            //todo: char-table, bool-vector
            throw new WrongTypeArgumentException("arrayp", array.toString());            
        } catch (IndexOutOfBoundsException e) {
            throw new ArgumentOutOfRange(array.toString(), index.toString());
        }
    }

    @Subroutine(value = "apply")
    public static LObject apply (Environment environment, LObject function, LObject... args) {
        if (!(function instanceof LispSymbol) || !((LispSymbol) function).isFunction())
            throw new InvalidFunctionException(function.toString());
        if (!(args[args.length-1] instanceof LispList))
            throw new WrongTypeArgumentException("listp", args[args.length-1].toString());
        ArrayList<LObject> list = new ArrayList<>();
        list.addAll(Arrays.asList(args).subList(0, args.length - 1));
        List<LObject> last =((LispList)args[args.length-1]).toLObjectList();
        list.addAll(last);
        return ((LispSymbol) function).evaluateFunction(environment, list);
    }

    @Subroutine(value = "format")
    public static LispString format (LispString formatString, @Optional LObject... objects) {
        String s = formatString.getData();
        int k = s.indexOf('%');
        String result = s.substring(0, (k == -1 ? s.length() : k));
        char p = '%';
        int index = 0;
        Character[] formatCharsArray = new Character[] {'s', 'S', 'd', 'o', 'x', 'X', 'e', 'f', 'g', 'c'};
        List<Character> formatChars = Arrays.asList(formatCharsArray);
        
        while (k != -1) {
            k++;
            while (k < s.length() && s.charAt(k) < 'A' && s.charAt(k) != '%') { //skip flags,width,precision: %<flags><width><precision>character
                k++;                    
            }
            if (k == s.length())
                throw new LispException("Format string ends in middle of format specifier");
            if (s.charAt(k) == '%') {
                result += '%';
            } else {
                if (index >= objects.length)
                    throw new LispException("Not enough arguments for format string");
                if (!(formatChars.contains(s.charAt(k))))
                    throw new LispException("Invalid format operation %" + s.charAt(k));
                result += objects[index].toString();
                index++;
            }
           /* switch (s.charAt(k)) {
                case 's': //print a string argument.  Actually, prints any object, with `princ'.
                    break;
                case 'S': //print any object as an s-expression (using `prin1').
                    break;
                case '%':
                    break;

                //todo: The argument used for %d, %o, %x, %e, %f, %g or %c must be a number.
                
                case 'd': //print as number in decimal (%o octal, %x hex)
                    break;
                case 'o': //print as number in octal
                    break;
                case 'x': //print as number in hex
                    break;
                case 'X': //is like %x, but uses upper case.
                    break;
                case 'e': //print a number in exponential notation.
                    break;
                case 'f': //print a number in decimal-point notation.
                    break;
                case 'g': //print a number in exponential notation or decimal-point notation, whichever uses fewer characters.
                    break;
                case 'c': //print a number as a single character.
                    break;


                default:
                    throw new LispException("Invalid format operation %" + s.charAt(k));
            } */
            k++;
            int nextPercent = s.indexOf('%', k); 
            result += s.substring(k, (nextPercent == -1 ? s.length() : nextPercent));
            k = nextPercent;
        }
        return new LispString(result);
    }

    @Subroutine(value = "purecopy")
    public static LObject pureCopy (LObject object) {
        /*
        TODO: ?
         */
        return object;
    }

    @Subroutine(value = "string-match")
    public static LObject stringMatch (LObject regexp, LispString string, @Optional LObject start) {
        return null;
    }
}