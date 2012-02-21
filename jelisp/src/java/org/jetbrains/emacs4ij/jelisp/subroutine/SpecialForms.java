package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.ForwardParser;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.Error;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

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

    private static void bindLetVariables (boolean isStar, CustomEnvironment inner, LispList varList) {
        ArrayList<LispSymbol> vars = new ArrayList<LispSymbol>();
        for (LObject var: varList.toLObjectList()) {
            if (var instanceof LispList) {
                LispSymbol symbol = new LispSymbol(((LispSymbol) ((LispList) var).car()).getName());
                LObject valueForm = ((LispList) var).cdr();
                if (valueForm instanceof LispList) {
                    LObject value = ((LispList)valueForm).car().evaluate(inner);
                    symbol.setValue(value);
                } else {
                    symbol.setValue(valueForm.evaluate(inner));
                }

                if (isStar)
                    inner.defineSymbol(symbol);
                else
                    vars.add(symbol);

                continue;
            }
            if (var instanceof LispSymbol) {
                LispSymbol symbol = new LispSymbol (((LispSymbol) var).getName(), LispSymbol.ourNil);
               // ((LispSymbol) var).setValue(LispSymbol.ourNil);
                if (isStar)
                    inner.defineSymbol(symbol);
                else
                    vars.add(symbol);

                continue;
            }
            throw new RuntimeException("wrong variable " + var.toString());
        }

        if (!isStar)
            for (LispSymbol symbol : vars) {
                inner.defineSymbol(symbol);
            }
    }

    private static LObject executeLet (boolean isStar, Environment environment, LispList varList, LObject... body) {
        CustomEnvironment inner = new CustomEnvironment(environment);
        bindLetVariables(isStar, inner, varList);
        LObject result = LispSymbol.ourNil;
        for (LObject bodyForm: body) {
            result = bodyForm.evaluate(inner);
        }
        return result;
    }

    @Subroutine("quote")
    public static LObject quote(Environment environment, LObject arg) {
        if (arg instanceof LispSymbol) {
            LispSymbol symbol = environment.find(((LispSymbol) arg).getName());
            if (symbol != null)
                return symbol;
        }
        return arg;
    }

    @Subroutine("let")
    public static LObject let (Environment environment, LispList varList, @Optional LObject... body) {
        return executeLet(false, environment, varList, body);
    }

    @Subroutine("let*")
    public static LObject letStar (Environment environment, LispList varList, @Optional LObject... body) {
        return executeLet(true, environment, varList, body);
    }

    @Subroutine("cond")
    public static LObject cond (Environment environment, @Optional LObject... args) {
        if (args == null)
            return LispSymbol.ourNil;

        LObject result = LispSymbol.ourNil;
        for (int i=0; i!=args.length; ++i) {
            LObject clause = args[i];
            if (!(clause instanceof LispList)) {
                if (clause.equals(LispSymbol.ourNil))
                    continue;
                throw new WrongTypeArgumentException("listp", clause.getClass().toString());
            }
            if (((LispList) clause).isEmpty())
                continue;
            LObject condition = ((LispList) clause).car().evaluate(environment);
            if (!condition.equals(LispSymbol.ourNil)) {
                List<LObject> data = ((LispList) clause).cdr() instanceof LispList ?
                        ((LispList)((LispList) clause).cdr()).toLObjectList() : new ArrayList<LObject>();
                result = condition;
                for (int k = 0; k != data.size(); ++k)
                    result = data.get(k).evaluate(environment);
                //if (!result.equals(LispSymbol.ourNil))
                return result;
            }
        }
        return result;
    }
    @Subroutine("while")
    public static LObject lispWhile(Environment environment, LObject cond, @Optional LObject... body) {
        CustomEnvironment inner = new CustomEnvironment(environment);
        LObject condition = cond.evaluate(inner);
        while (!condition.equals(LispSymbol.ourNil)) {
            if (body != null)
                for (LObject bodyForm: body)
                    bodyForm.evaluate(inner);
            condition = cond.evaluate(inner);
        }
        return condition;
    }
    @Subroutine(value = "if")
    public static LObject lispIf (Environment environment, LObject cond, LObject then, @Optional LObject... elseBody) {
        LObject condition = cond.evaluate(environment);
        if (!condition.equals(LispSymbol.ourNil)) {
            return then.evaluate(environment);
        }
        if (elseBody == null)
            return LispSymbol.ourNil;

        LObject result = LispSymbol.ourNil;
        for (LObject bodyForm: elseBody) {
            result = bodyForm.evaluate(environment);
        }
        return result;
    }

    @Subroutine("and")
    public static LObject lispAnd(Environment environment, @Optional LObject... conditions) {
        if (conditions == null)
            return LispSymbol.ourT;
        LObject result = LispSymbol.ourT;
        for (LObject condition: conditions) {
            result = condition.evaluate(environment);
            if (result.equals(LispSymbol.ourNil))
                return result;
        }
        return result;
    }

    @Subroutine("or")
    public static LObject lispOr(Environment environment, @Optional LObject... conditions) {
        if (conditions == null)
            return LispSymbol.ourNil;
        for (LObject condition: conditions) {
            LObject result = condition.evaluate(environment);
            if (!result.equals(LispSymbol.ourNil))
                return result;
        }
        return LispSymbol.ourNil;
    }

    private static LispSymbol defSymbol (Environment environment, LispSymbol name, LObject initValue, boolean overwrite, LObject docString) {
        LispSymbol variable = GlobalEnvironment.INSTANCE.find(name.getName());
        if (variable == null) {
            LObject value = (initValue == null) ? null : initValue.evaluate(environment);

            LispSymbol symbol = new LispSymbol(name.getName());
            symbol.setValue(value);

            if (docString != null) {
                symbol.setVariableDocumentation(docString.evaluate(environment));
            }
            GlobalEnvironment.INSTANCE.defineSymbol(symbol);
            return name;
        }
        if (overwrite || (!variable.hasValue() && initValue != null)) {
            if (initValue == null)
                throw new RuntimeException("Init value is null!"); 
            LObject value = initValue.evaluate(environment);
            //LispSymbol symbol = new LispSymbol(name.getName(), value);
            variable.setValue(value);
        }
        if (docString != null) {
            variable.setVariableDocumentation(docString.evaluate(environment));
        }
        GlobalEnvironment.INSTANCE.defineSymbol(variable);
        return variable;
    }
    
    @Subroutine("defvar")
    public static LObject defineVariable(Environment environment, LispSymbol name, @Optional LObject initValue, @Optional LObject docString) {
        return defSymbol(environment, name, initValue, false, docString);
    }

    @Subroutine("defconst")
    public static LObject defineConstant (Environment environment, LispSymbol name, LObject value, @Optional LObject docString) {
        return defSymbol(environment, name, value, true, docString);
    }

    @Subroutine(value = "defun")
    public static LispSymbol defineFunction(Environment environment, LispSymbol name, LObject... body) {
        LispSymbol symbol = GlobalEnvironment.INSTANCE.find(name.getName());
        LispSymbol f = symbol != null ? symbol : new LispSymbol(name.getName());
        ArrayList<LObject> data = new ArrayList<>();
        data.add(new LispSymbol("lambda"));
        Collections.addAll(data, body);
        f.setFunction(LispList.list(data));
        GlobalEnvironment.INSTANCE.defineSymbol(f);
        return f;
    }

    @Subroutine("interactive")
    public static LObject interactive(Environment environment, @Optional LObject args) {
        return null;


        /*if (args == null)
            return LispSymbol.ourNil;
        if (args instanceof LispList) {
            args = args.evaluate(environment);
            if (args instanceof LispList)
                return args;
        }
        if (args instanceof LispString) {
            //TODO: interactively read the arguments
            SpecialFormInteractive interactive = new SpecialFormInteractive(environment, ((LispString) args).getData());
            synchronized (interactive) {
                while (!interactive.isFinished()) {
                    LObject a = interactive.getArgument();
                    System.out.println(a);
                }
            }

            return null;
        }
        throw new WrongTypeArgumentException("listp", args.toString()); */
    }

    @Subroutine("progn")
    public static LObject progn (Environment environment, @Optional LObject... args) {
        if (args == null)
            return LispSymbol.ourNil;
        CustomEnvironment inner = new CustomEnvironment(environment);
        LObject result = LispSymbol.ourNil;
        for (LObject arg: args) {
            result = arg.evaluate(inner);
        }
        return result;
    }

    @Subroutine("prog1")
    public static LObject prog1 (Environment environment, LObject p, @Optional LObject... args) {
        CustomEnvironment inner = new CustomEnvironment(environment);
        LObject result = p.evaluate(inner);
        for (LObject arg: args) {
            arg.evaluate(inner);
        }
        return result;
    }

    @Subroutine("prog2")
    public static LObject prog1 (Environment environment, LObject p1, LObject p2, @Optional LObject... args) {
        CustomEnvironment inner = new CustomEnvironment(environment);
        p1.evaluate(inner);
        LObject result = p2.evaluate(inner);
        for (LObject arg: args) {
            arg.evaluate(inner);
        }
        return result;
    }

    @Subroutine("setq")
    public static LObject setq (Environment environment, @Optional LObject... args) {
        if (args == null)
            return LispSymbol.ourNil;
        CustomEnvironment inner = new CustomEnvironment(environment);
        int index = 0;
        LObject value = LispSymbol.ourNil;
        while (index < args.length) {
            if (!(args[index] instanceof LispSymbol))
                throw new WrongTypeArgumentException("symbolp", args[index].getClass().getSimpleName());
            value = (index+1 == args.length) ? LispSymbol.ourNil : args[index+1].evaluate(inner);
            LispSymbol symbol = new LispSymbol(((LispSymbol) args[index]).getName(), value);
            environment.setVariable(symbol);
            index += 2;
        }
        return value;
    }

    private static boolean isDeclareForm (LObject form) {
        if (form instanceof LispList)
            if (((LispList)form).car().equals(new LispSymbol("declare")))
                return true;
        return false;
    }

    private static int getAllDeclareForms (int startIndex, LObject[] body) {
        int k = startIndex;
        while (k < body.length && isDeclareForm(body[k])) {
            //todo: store this declare form somewhere
            ++k;
        }
        return k;
    }

    @Subroutine("defmacro")
    public static LispSymbol defmacro (LispSymbol name, LObject argList, @Optional LObject ... body) {
        LispSymbol symbol = GlobalEnvironment.INSTANCE.find(name.getName());
        LispSymbol f = symbol != null ? symbol : new LispSymbol(name.getName());
        ArrayList<LObject> data = new ArrayList<>();
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
    public static LObject conditionCase (Environment environment, LispSymbol var, LObject bodyForm, @Optional LObject... handlers) {
        ArrayList<LispList> h = new ArrayList<LispList>();
        if (handlers != null) {
            for (LObject element: handlers) {
                try {
                    LispList handler = (LispList)element;
                    if (!(handler.car() instanceof LispSymbol))
                        throw new ClassCastException();
                    h.add(handler);
                } catch (ClassCastException e) {
                    throw new RuntimeException("Invalid condition handler");
                    //todo: return parseString("(error \"\"Invalid condition handler\")").evaluate(environment);
                }
            }
        }
        
        try {
            return bodyForm.evaluate(environment);
        } catch (RuntimeException e) {

            Throwable exc = e;
            while (!(exc instanceof LispException)) {
                exc = exc.getCause();
            }

            Error annotation = exc.getClass().getAnnotation(Error.class);
            if (annotation != null) {
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

                        CustomEnvironment inner = new CustomEnvironment(environment);
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

                        LObject result = LispSymbol.ourNil;
                        for (LObject form: ((LispList)handler.cdr()).toLObjectList()) {
                            result = form.evaluate(inner);
                        }

                        return result;
                    }
                }
            } 
            throw e;
        }
    }
    
    @Subroutine("catch")
    public static void lispCatch (LObject tag, LObject... body) {
        //todo: emacs man says nil cannot be tag, but signals no error though
        //if (tag.equals(LispSymbol.ourNil))

    }
    
}