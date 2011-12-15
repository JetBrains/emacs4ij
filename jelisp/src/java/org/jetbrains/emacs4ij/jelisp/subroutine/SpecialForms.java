package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.Parser;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.Error;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

import java.util.ArrayList;
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
        ArrayList<LispSymbol> vars = new ArrayList<LispSymbol>();
        for (LObject var: varList.toLObjectList()) {
            if (var instanceof LispList) {
                LispSymbol symbol = (LispSymbol) ((LispList) var).car();
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
                ((LispSymbol) var).setValue(LispSymbol.ourNil);
                if (isStar)
                    inner.defineSymbol((LispSymbol) var);
                else
                    vars.add((LispSymbol) var);

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
        Environment inner = new Environment(environment);
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
                if (!result.equals(LispSymbol.ourNil))
                    return result;
            }
        }
        return result;
    }
    @Subroutine("while")
    public static LObject lispWhile(Environment environment, LObject cond, @Optional LObject... body) {
        Environment inner = new Environment(environment);
        LObject condition = cond.evaluate(inner);
        while (condition != LispSymbol.ourNil) {
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
        if (condition != LispSymbol.ourNil) {
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
            if (result == LispSymbol.ourNil)
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

    @Subroutine("defvar")
    public static LObject defineVariable(Environment environment, LispSymbol name, @Optional LObject initValue, LispString docString) {
        LispSymbol variable = GlobalEnvironment.getInstance().find(name.getName());
        if (variable == null) {
            LObject value = (initValue == null) ? null : initValue.evaluate(environment);
            name.setValue(value);
            if (docString != null)
                name.setVariableDocumentation(docString);
            GlobalEnvironment.getInstance().defineSymbol(name);
            return name;
        }
        if (!variable.hasValue() && initValue != null) {
            LObject value = initValue.evaluate(environment);
            variable.setValue(value);
        }
        if (docString != null)
            variable.setVariableDocumentation(docString);
        GlobalEnvironment.getInstance().defineSymbol(variable);
        return variable;
    }

    @Subroutine(value = "defun")
    public static LispSymbol defineFunction(Environment environment, LispSymbol name, LObject... body) {
        LispSymbol symbol = GlobalEnvironment.getInstance().find(name.getName());
        LispSymbol f = symbol != null ? symbol : name;
        LispList functionCell = LispList.list(new LispSymbol("lambda"));
        for (LObject bodyForm: body)
            functionCell.add(bodyForm);
        f.setFunction(functionCell);
        GlobalEnvironment.getInstance().defineSymbol(f);
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
        Environment inner = new Environment(environment);
        LObject result = LispSymbol.ourNil;
        for (LObject arg: args) {
            result = arg.evaluate(inner);
        }
        return result;
    }

    @Subroutine("setq")
    public static LObject setq (Environment environment, @Optional LObject... args) {
        if (args == null)
            return LispSymbol.ourNil;
        Environment inner = new Environment(environment);
        int index = 0;
        LObject value = LispSymbol.ourNil;
        while (index < args.length) {
            if (!(args[index] instanceof LispSymbol))
                throw new WrongTypeArgumentException("symbolp", args[index].getClass().getSimpleName());
            value = (index+1 == args.length) ? LispSymbol.ourNil : args[index+1].evaluate(inner);
            ((LispSymbol) args[index]).setValue(value);
            environment.setVariable((LispSymbol) args[index]);
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
        LispSymbol symbol = GlobalEnvironment.getInstance().find(name.getName());
        LispSymbol f = symbol != null ? symbol : name;
        LispList functionCell = LispList.list(new LispSymbol("macro"));
        functionCell.add(new LispSymbol("lambda"));
        functionCell.add(argList);
        if (body != null) {
            int k = 0;
            if (body.length != 0) {
                k = getAllDeclareForms(k, body);
                if (k == 0) {
                    if (body[0] instanceof LispString) {
                        functionCell.add(body[0]);
                        k = getAllDeclareForms(1, body);
                    }
                }
            }

            for (; k < body.length; ++k) {
                functionCell.add(body[k]);
            }
        }
        f.setFunction(functionCell);
        GlobalEnvironment.getInstance().defineSymbol(f);
        return f;
    }

    private static LObject parseString(String lispCode, Environment environment) throws LispException {
        Parser parser = new Parser();
        return parser.parseLine(lispCode);
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


                        Parser parser = new Parser();
                        LispList errorInfo = (LispList) parser.parseLine(exc.getMessage());
                        while (!GlobalEnvironment.ourCallStack.getFirst().equals("condition-case")) {
                            //todo: make full error list and store it in errorInfo
                            // make somehow new error message
                            // errorInfo = new LispList( <new error> errorInfo);

                            GlobalEnvironment.ourCallStack.removeFirst();
                        }

                        Environment inner = new Environment(environment);
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
}