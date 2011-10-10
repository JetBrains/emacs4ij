package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.InvalidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidFunctionException;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/2/11
 * Time: 5:05 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class BuiltinsSymbol {
    private BuiltinsSymbol() {}

    @Subroutine("symbol-function")
    public static LObject symbolFunction(Environment environment, LispSymbol arg) {
        LispSymbol f = environment.find(arg.getName());
        if (f == null || f.getFunction().equals(LispSymbol.ourVoid))
            throw new VoidFunctionException(arg.getName());
        return f;
    }

    @Subroutine("get")
    public static LObject get(Environment environment, LispSymbol symbol, LispSymbol propertyName) {
        return environment.find(symbol.getName(), "getProperty", new Class[]{LispSymbol.class}, propertyName);
    }

    @Subroutine("put")
    public static LObject put(Environment environment, LispSymbol symbol, LispSymbol propertyName, LObject value) {
        environment.find(symbol.getName(), "setProperty", new Class[] {LispSymbol.class, LispObject.class}, propertyName, value);
        return value;
    }

    @Subroutine("documentation-property")
    public static LObject documentationProperty (Environment environment, LispSymbol symbol, LispSymbol propertyName, @Optional LObject verbatim)  {
        //todo: if (verbatim != null) && !(verbatim.equals(LispSymbol.ourNil) ---
        // Third argument RAW omitted or nil means pass the result through `substitute-command-keys' if it is a string.
        LObject value = environment.find(symbol.getName(), "getProperty", new Class[]{LispSymbol.class}, propertyName);
        if (!(value instanceof LispString)) {
            //TODO:
            /* by Dmitry Neverov:: Похоже на то, что если в property записан integer, то он
интерпретируется как смещение в файле документации, где записана дока
по всем build-in функциям. Emacs лезет в этот файл, читает по данному
смещению и ничего не находит (или находит не то, что не ожидал) и
поэтому возвращает nil. Имя этого файла задаётся переменной
internal-doc-file-name. У меня он находится в
/usr/local/share/emacs/23.2/etc/DOC-23.2.1.

Если выполнить вот такое:

(defun fun ())
(setq x 0)
(while (< x 1000)
 (put 'fun 'function-documentation x)
 (let ((doc (documentation-property 'fun 'function-documentation)))
   (if doc (message doc)))
 (incf x))

То по нескольким смещениям документация все-таки находится.
             */
            return value.evaluate(environment);
        }
        return value;
    }

    @Subroutine("documentation")
    public static LObject documentation (Environment environment, LispObject function) {
        if (function instanceof LispSymbol) {
            String name = ((LispSymbol) function).getName();
            LispSymbol f = environment.find(name);
            if (f == null)
                throw new VoidFunctionException(name);
            if (f.getFunction() == null)
                throw new VoidFunctionException(name);
            LObject funPropValue = documentationProperty(environment, f, new LispSymbol("function-documentation"), null);
            if (!funPropValue.equals(LispSymbol.ourNil))
                return funPropValue;

            if (f.is(LispSymbol.FunctionType.Custom)) {
                f.castToLambda(environment);
                return ((Lambda)f.getFunction()).getDocString();
            }

            return null;

            //todo: provide doc for special forms and biiltins

        } else if (function instanceof Lambda) {
            return ((Lambda) function).getDocString();
        }
        throw new InvalidFunctionException(function.toString());
    }
}
