package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.Error;
import org.jetbrains.emacs4ij.jelisp.exception.InternalException;
import org.jetbrains.emacs4ij.jelisp.exception.LispThrow;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.interactive.SpecialFormInteractive;
import org.jetbrains.emacs4ij.jelisp.parser.ForwardParser;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: Kate
 * Date: 7/16/11
 * Time: 2:47 PM
 * To change this template use File | Settings | File Templates.
 *
 * in fact it is a kind of builtin function
 */
public abstract class SpecialForms {
    private SpecialForms() {}

    private static void bindLetVariables (boolean isStar, Environment inner, LispList varList) {
        ArrayList<LispSymbol> vars = new ArrayList<>();
        for (LispObject var: varList.toLispObjectList()) {
            if (var instanceof LispList) {
                LispSymbol symbol = new LispSymbol(((LispSymbol) ((LispList) var).car()).getName());
                LispObject valueForm = ((LispList) var).cdr();
                if (valueForm instanceof LispList) {
                    LispObject value = ((LispList)valueForm).car().evaluate(inner);
                    symbol.setValue(value);
                } else {
                    symbol.setValue(valueForm.evaluate(inner));
                }
                if (isStar)
                    inner.defineSymbol(symbol);
                else vars.add(symbol);
                continue;
            }
            if (var instanceof LispSymbol) {
                LispSymbol symbol = new LispSymbol (((LispSymbol) var).getName(), LispSymbol.ourNil);
                if (isStar)
                    inner.defineSymbol(symbol);
                else vars.add(symbol);
                continue;
            }
            throw new InternalException(JelispBundle.message("wrong.variable", var.toString()));
        }
        if (!isStar)
            for (LispSymbol symbol : vars) {
                inner.defineSymbol(symbol);
            }
    }

    private static LispObject executeLet (boolean isStar, Environment environment, LispList varList, LispObject... body) {
        Environment inner = new CustomEnvironment(environment);
        bindLetVariables(isStar, inner, varList);
        LispObject result = LispSymbol.ourNil;
        for (LispObject bodyForm: body) {
            result = bodyForm.evaluate(inner);
        }
        return result;
    }

    @Subroutine("quote")
    public static LispObject quote(Environment environment, LispObject arg) {
        if (arg instanceof LispSymbol) {
            LispSymbol symbol = environment.find(((LispSymbol) arg).getName());
            if (symbol != null)
                return symbol;
        }
        return arg;
    }

    @Subroutine("let")
    public static LispObject let (Environment environment, LispList varList, @Optional LispObject... body) {
        return executeLet(false, environment, varList, body);
    }

    @Subroutine("let*")
    public static LispObject letStar (Environment environment, LispList varList, @Optional LispObject... body) {
        return executeLet(true, environment, varList, body);
    }

    @Subroutine("cond")
    public static LispObject cond (Environment environment, @Optional LispObject... args) {
        if (args == null)
            return LispSymbol.ourNil;

        LispObject result = LispSymbol.ourNil;
        for (int i=0; i!=args.length; ++i) {
            LispObject clause = args[i];
            if (!(clause instanceof LispList)) {
                if (clause.equals(LispSymbol.ourNil))
                    continue;
                throw new WrongTypeArgumentException("listp", clause);
            }
            if (((LispList) clause).isEmpty())
                continue;
            LispObject condition = ((LispList) clause).car().evaluate(environment);
            if (!condition.equals(LispSymbol.ourNil)) {
                List<LispObject> data = ((LispList) clause).cdr() instanceof LispList ?
                        ((LispList)((LispList) clause).cdr()).toLispObjectList() : new ArrayList<LispObject>();
                result = condition;
                for (int k = 0; k != data.size(); ++k)
                    result = data.get(k).evaluate(environment);
                return result;
            }
        }
        return result;
    }
    @Subroutine("while")
    public static LispObject lispWhile(Environment environment, LispObject cond, @Optional LispObject... body) {
        Environment inner = new CustomEnvironment(environment);
        LispObject condition = cond.evaluate(inner);
        while (!condition.equals(LispSymbol.ourNil)) {
            if (body != null)
                for (LispObject bodyForm: body)
                    bodyForm.evaluate(inner);
            condition = cond.evaluate(inner);
        }
        return condition;
    }
    @Subroutine(value = "if")
    public static LispObject lispIf (Environment environment, LispObject cond, LispObject then, @Optional LispObject... elseBody) {
        LispObject condition = cond.evaluate(environment);
        if (!condition.equals(LispSymbol.ourNil)) {
            return then.evaluate(environment);
        }
        if (elseBody == null)
            return LispSymbol.ourNil;

        LispObject result = LispSymbol.ourNil;
        for (LispObject bodyForm: elseBody) {
            result = bodyForm.evaluate(environment);
        }
        return result;
    }

    @Subroutine("and")
    public static LispObject lispAnd(Environment environment, @Optional LispObject... conditions) {
        if (conditions == null)
            return LispSymbol.ourT;
        LispObject result = LispSymbol.ourT;
        for (LispObject condition: conditions) {
            result = condition.evaluate(environment);
            if (result.equals(LispSymbol.ourNil))
                return result;
        }
        return result;
    }

    @Subroutine("or")
    public static LispObject lispOr(Environment environment, @Optional LispObject... conditions) {
        if (conditions == null)
            return LispSymbol.ourNil;
        for (LispObject condition: conditions) {
            LispObject result = condition.evaluate(environment);
            if (!result.equals(LispSymbol.ourNil))
                return result;
        }
        return LispSymbol.ourNil;
    }

    private static LispSymbol defSymbol (Environment environment, LispSymbol name, LispObject initValue, boolean overwrite, LispObject docString) {
        LispSymbol variable = GlobalEnvironment.INSTANCE.find(name.getName());
        if (variable == null) {
            LispObject value = initValue == null ? null : initValue.evaluate(environment);
            LispSymbol symbol = new LispSymbol(name.getName());
            symbol.setValue(value);
            if (docString != null) {
                symbol.setVariableDocumentation(docString.evaluate(environment));
            }
            GlobalEnvironment.INSTANCE.defineSymbol(symbol);
            return symbol;
        }
        if (overwrite || (!variable.hasValue() && initValue != null)) {
            if (initValue == null)
                throw new InternalException(JelispBundle.message("null.init.value"));
            LispObject value = initValue.evaluate(environment);
            variable.setValue(value);
        }
        if (docString != null) {
            variable.setVariableDocumentation(docString.evaluate(environment));
        }
        GlobalEnvironment.INSTANCE.defineSymbol(variable);
        return variable;
    }

    @Subroutine("defvar")
    public static LispObject defineVariable(Environment environment, LispSymbol name, @Optional LispObject initValue, @Optional LispObject docString) {
        return defSymbol(environment, name, initValue, false, docString);
    }

    @Subroutine("defconst")
    public static LispObject defineConstant (Environment environment, LispSymbol name, LispObject value, @Optional LispObject docString) {
        return defSymbol(environment, name, value, true, docString);
    }

    @Subroutine(value = "defun")
    public static LispSymbol defineFunction(Environment environment, LispSymbol name, LispObject... body) {
        LispSymbol symbol = GlobalEnvironment.INSTANCE.find(name.getName());
        LispSymbol f = symbol != null ? symbol : new LispSymbol(name.getName());
        ArrayList<LispObject> data = new ArrayList<>();
        data.add(new LispSymbol("lambda"));
        Collections.addAll(data, body);
        f.setFunction(LispList.list(data));
        GlobalEnvironment.INSTANCE.defineSymbol(f);
        return f;
    }

    private static LispList interactivePrepare (Environment environment, @Nullable LispObject... args) {
        if (args == null || args.length == 0)
            return LispList.list();

        LispObject arg = args[0];
        if (Predicate.isNil(arg))
            return LispList.list();
//            return arg instanceof LispList && ((LispList) arg).isEmpty()
//                    ? (LispList) arg
//                    : LispList.list(LispSymbol.ourNil);

        if (arg instanceof LispList) {
            arg = arg.evaluate(environment);
            if (arg instanceof LispList)
                return (LispList) arg;
            throw new WrongTypeArgumentException("listp", arg.toString());
        }

        if (arg instanceof LispString) {
            SpecialFormInteractive interactive = new SpecialFormInteractive(environment, Core.getInvoker(),
                    ((LispString) arg).getData());
            LispMiniBuffer miniBuffer = environment.getMiniBuffer();
            miniBuffer.onInteractiveNoIoInput(interactive);
            return null;
        }
        throw new WrongTypeArgumentException("listp", arg.toString());
    }

    @Subroutine("interactive")
    public static LispList interactive(Environment environment, @Optional LispObject... args) {
        return environment.isMainOrGlobal() ? interactivePrepare(environment, args) : LispList.list();
    }

    @Subroutine("progn")
    public static LispObject progn (Environment environment, @Optional LispObject... args) {
        if (args == null)
            return LispSymbol.ourNil;
        Environment inner = new CustomEnvironment(environment);
        LispObject result = LispSymbol.ourNil;
        for (LispObject arg: args) {
            result = arg.evaluate(inner);
        }
        return result;
    }

    @Subroutine("prog1")
    public static LispObject prog1 (Environment environment, LispObject p, @Optional LispObject... args) {
        Environment inner = new CustomEnvironment(environment);
        LispObject result = p.evaluate(inner);
        for (LispObject arg: args) {
            arg.evaluate(inner);
        }
        return result;
    }

    @Subroutine("prog2")
    public static LispObject prog1 (Environment environment, LispObject p1, LispObject p2, @Optional LispObject... args) {
        Environment inner = new CustomEnvironment(environment);
        p1.evaluate(inner);
        LispObject result = p2.evaluate(inner);
        for (LispObject arg: args) {
            arg.evaluate(inner);
        }
        return result;
    }

    @Subroutine("setq")
    public static LispObject setq (Environment environment, @Optional LispObject... args) {
        if (args == null)
            return LispSymbol.ourNil;
        Environment inner = new CustomEnvironment(environment);
        int index = 0;
        LispObject value = LispSymbol.ourNil;
        while (index < args.length) {
            if (!(args[index] instanceof LispSymbol))
                throw new WrongTypeArgumentException("symbolp", args[index]);
            value = (index+1 == args.length) ? LispSymbol.ourNil : args[index+1].evaluate(inner);
            LispSymbol symbol = new LispSymbol(((LispSymbol) args[index]).getName(), value);
            environment.setVariable(symbol);
            index += 2;
        }
        return value;
    }

    private static boolean isDeclareForm (LispObject form) {
        if (form instanceof LispList)
            if (((LispList)form).car().equals(new LispSymbol("declare")))
                return true;
        return false;
    }

    private static int getAllDeclareForms (int startIndex, LispObject[] body) {
        int k = startIndex;
        while (k < body.length && isDeclareForm(body[k])) {
            //todo: store this declare form somewhere
            ++k;
        }
        return k;
    }

    @Subroutine("defmacro")
    public static LispSymbol defmacro (LispSymbol name, LispObject argList, @Optional LispObject ... body) {
        LispSymbol symbol = GlobalEnvironment.INSTANCE.find(name.getName());
        LispSymbol f = symbol != null ? symbol : new LispSymbol(name.getName());
        ArrayList<LispObject> data = new ArrayList<>();
        data.add(new LispSymbol("macro"));
        data.add(new LispSymbol("lambda"));
        data.add(argList);
        if (body != null) {
            int k = 0;
            if (body.length != 0) {
                k = getAllDeclareForms(k, body);
                if (k == 0) {
                    if (body[0] instanceof LispString) {
                        data.add(body[0]);
                        k = getAllDeclareForms(1, body);
                    }
                }
            }

            for (; k < body.length; ++k) {
                data.add(body[k]);
            }
        }
        f.setFunction(LispList.list(data));
        GlobalEnvironment.INSTANCE.defineSymbol(f);
        return f;
    }

    @Subroutine("condition-case")
    public static LispObject conditionCase (Environment environment, LispSymbol var, LispObject bodyForm, @Optional LispObject... handlers) {
        ArrayList<LispList> h = new ArrayList<>();
        if (handlers != null) {
            for (LispObject element: handlers) {
                try {
                    LispList handler = (LispList)element;
                    if (!(handler.car() instanceof LispSymbol))
                        throw new ClassCastException();
                    h.add(handler);
                } catch (ClassCastException e) {
                    Core.error(JelispBundle.message("invalid.condition.handler"));
                }
            }
        }
        try {
            return bodyForm.evaluate(environment);
        } catch (RuntimeException e) {
            Throwable exc = e;
            while (exc.getCause() != null) {
                exc = exc.getCause();
            }
            Error annotation = exc.getClass().getAnnotation(Error.class);
            if (annotation == null)
                throw e;
            for (LispList handler: h) {
                LispSymbol errorSymbol = (LispSymbol) handler.car();
                if (errorSymbol.getName().equals(annotation.value())) {
                    // todo: unbind all bindings; clean-ups for all unwind-protect forms
                    ForwardParser forwardParser = new ForwardParser();
                    LispList errorInfo = (LispList) forwardParser.parseLine(exc.getMessage());
                    while (!GlobalEnvironment.ourCallStack.getFirst().equals("condition-case")) {
                        //todo: make full error list and store it in errorInfo
                        // make somehow new error message
                        // errorInfo = new LispList( <new error> errorInfo);
                        GlobalEnvironment.ourCallStack.removeFirst();
                    }
                    Environment inner = new CustomEnvironment(environment);
                    if (!var.equals(LispSymbol.ourNil)) {
                        LispSymbol test = environment.find(var.getName());
                        if (test == null) {
                            //init in given environment
                            setq(environment, var, errorInfo);
                        } else {
                            //create local binding
                            var.setValue(errorInfo.evaluate(inner));
                            inner.defineSymbol(var);
                        }
                    }
                    LispObject result = LispSymbol.ourNil;
                    for (LispObject form: ((LispList)handler.cdr()).toLispObjectList()) {
                        result = form.evaluate(inner);
                    }
                    return result;
                }
            }
            throw e;
        }
    }

    @Subroutine("catch")
    public static LispObject lispCatch (Environment environment, LispObject tagObject, @Optional LispObject... body) {
        //note: emacs man says nil cannot be tag, but signals no error though
        LispObject tag = tagObject.evaluate(environment);
        try {
            LispObject result = LispSymbol.ourNil;
            for (LispObject bodyForm: body) {
                result = bodyForm.evaluate(environment);
            }
            return result;
        } catch (LispThrow e) {
            if (Core.eqs(tag, e.getTag())) {
                return e.getValue();
            }
            throw e;
        }
    }

    @Subroutine("function")
    public static LispObject function (Environment environment, LispObject arg) {
        return quote(environment, arg);
    }

    @Subroutine("save-excursion")
    public static LispObject saveExcursion (Environment environment, @Optional LispObject... body) {
        LispBuffer current = environment.getBufferCurrentForEditing();
        int point = current.point();
        LispMarker mark = current.getMark();
        try {
            return progn(environment, body);
        } finally {
            current.setPoint(point);
            current.setMark(mark);
            environment.setBufferCurrentForEditing(current);
        }
    }

    @Subroutine("save-current-buffer")
    public static LispObject saveCurrentBuffer (Environment environment, @Optional LispObject... body) {
        String oldCurrentBufferName = environment.getBufferCurrentForEditing().getName();
        try {
            return progn(environment, body);
        } finally {
            LispBuffer old = environment.findBuffer(oldCurrentBufferName);
            if (old != null) {
                environment.setBufferCurrentForEditing(old);
            }
        }
    }
    
    @Subroutine("unwind-protect")
    public static LispObject unwindProtect (Environment environment, LispObject body, @Optional LispObject... protectForms) {
        try {
            return body.evaluate(environment);
        } finally {
            for (LispObject protectForm: protectForms) {
                protectForm.evaluate(environment);
            }
        }
    }
}