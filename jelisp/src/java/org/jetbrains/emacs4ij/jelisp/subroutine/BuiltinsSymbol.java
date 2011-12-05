package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.InvalidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;

import java.io.IOException;
import java.io.RandomAccessFile;

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
        if (f == null || !f.isFunction())
            throw new VoidFunctionException(arg.getName());
        return f.getFunction();
    }

    @Subroutine("symbol-value")
    public static LObject symbolValue (Environment environment, LispSymbol arg) {
        LispSymbol symbol = environment.find(arg.getName());
        if (symbol == null || symbol.getValue() == null || symbol.getValue().equals(LispSymbol.ourVoid))
            throw new VoidVariableException(arg.getName());
        return symbol.getValue();
    }

    @Subroutine("get")
    public static LObject get(Environment environment, LispSymbol symbol, LispSymbol propertyName) {
        LObject result = environment.find(symbol.getName(), "getProperty", new Class[]{LispSymbol.class}, propertyName);
        return result == null ? LispSymbol.ourNil : result;
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
            if (!(value instanceof LispInteger))
                return value.evaluate(environment);

            int offset = ((LispInteger) value).getData();
            if (offset < 0)
                offset = -offset;
            try {
                RandomAccessFile docFile = new RandomAccessFile(((LispString)GlobalEnvironment.getInstance().find("doc-directory").getValue()).getData() +
                        ((LispString)GlobalEnvironment.getInstance().find("internal-doc-file-name").getValue()).getData(), "r");
                docFile.seek(offset);
                String line = docFile.readLine();
                String doc = line;
                while ((line = docFile.readLine()) != null) {
                    if (line.contains(""))  // \u001F
                        break;
                    doc += '\n' + line;
                }
                docFile.close();
                doc += '\n' + line.substring(0, line.indexOf(''));
                return new LispString(doc);
            } catch (IOException e) {
                //throw new RuntimeException(e.getMessage());
                return LispSymbol.ourNil;
            }
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

            LObject funPropValue = documentationProperty(environment, f, new LispSymbol("function-documentation"), null);
            if (!funPropValue.equals(LispSymbol.ourNil))
                return funPropValue;

            if (!f.isFunction())
                throw new VoidFunctionException(name);
            return f.getDocumentation(environment);

        } else if (function instanceof Lambda) {
            return ((Lambda) function).getDocString();
        } else if (function instanceof Macro) {
            return ((Macro)function).getDocString();
        } else if (function instanceof Primitive) {
            return ((Primitive) function).getDocString();
        }
        throw new InvalidFunctionException(function.toString());
    }
}
