package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgument;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

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
        for (LObject var: varList.getData()) {
            if (var instanceof LispList) {
                LispSymbol symbol = (LispSymbol) ((LispList) var).car();
                LispList valueForm = ((LispList) var).cdr();
                LObject value = valueForm.car().evaluate(inner);
                symbol.setValue(value);

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

    @Subroutine("defmacro")
    public static LObject defineMacro (Environment environment, List<LispObject> args) {
        throw new NotImplementedException();
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
                throw new WrongTypeArgument("LispList", clause.getClass().toString());
            }
            if (((LispList) clause).isEmpty())
                continue;
            LObject condition = ((LispList) clause).car().evaluate(environment);
            if (!condition.equals(LispSymbol.ourNil)) {
                List<LObject> data = ((LispList) clause).cdr().getData();
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
        LispSymbol variable = environment.getMainEnvironment().find(name.getName());
        if (variable == null) {
            LObject value = (initValue == null) ? LispSymbol.ourVoid : initValue.evaluate(environment);
            name.setValue(value);
            if (docString != null)
                name.setVariableDocumentation(docString);
            environment.getMainEnvironment().defineSymbol(name);
            return name;
        }
        if (variable.getValue().equals(LispSymbol.ourVoid)) {
            LObject value = (initValue == null) ? LispSymbol.ourVoid : initValue.evaluate(environment);
            variable.setValue(value);
        }
        if (docString != null)
            variable.setVariableDocumentation(docString);
        environment.getMainEnvironment().defineSymbol(variable);
        return variable;
    }

    @Subroutine(value = "defun")
    public static LObject defineFunction(Environment environment, LispSymbol name, LObject... body) {
        LispSymbol symbol = environment.getMainEnvironment().find(name.getName());
        LispSymbol f = symbol != null ? symbol : name;
        LispList functionCell = new LispList(new LispSymbol("lambda"));
        for (LObject bodyForm: body)
            functionCell.add(bodyForm);
        f.setFunction(functionCell);
        environment.getMainEnvironment().defineSymbol(f);
        return name;
    }

    @Subroutine("interactive")
    public static LObject interactive(Environment environment, @Optional LObject args) {
        if (args == null)
            return LispSymbol.ourNil;
        if (args instanceof LispList) {
            args = args.evaluate(environment);
            if (args instanceof LispList)
                return args;
        }
        if (args instanceof LispString) {
            //TODO: interactively read the arguments
            /*SpecialFormInteractive interactive = new SpecialFormInteractive(environment, ((LispString) args).getData());
            synchronized (interactive) {
                while (!interactive.isFinished()) {
                    LObject a = interactive.getArgument();
                    System.out.println(a);
                }
            }*/

            return null;
        }
        throw new WrongTypeArgument("listp", args.toString());
    }

    /*

    private static LispList processInteractiveString (LispString interactiveString, Environment environment) {
        String[] commands = interactiveString.getData().split("\n");
        LispList args = new LispList();
        for (int i = 0, commandsLength = commands.length; i < commandsLength; i++) {
            String command = commands[i];
            String parameter = "";
            String parameterStartValue = null;
            String message = command.substring(1);
            boolean noMatch = false;
            char codeLetter = command.charAt(0);



            switch (codeLetter) {
                case 'a': // -- Function name: symbol with a function definition. todo: Completion
                    while (true) {
                        parameter = getParameter(environment, message, parameterStartValue, noMatch);
                        LispSymbol f = environment.find(parameter);
                        if (f != null) {
                            if (f.isFunction()) {
                                LispSymbol fun = new LispSymbol(parameter, f.getFunction());
                                args.add(fun);
                                break;
                            }
                        }
                        parameterStartValue = parameter;
                        noMatch = true;
                    }
                    break;
                case 'b': // -- Name of existing buffer.  todo: Completion
                    String currentBufferName = environment.getBufferCurrentForEditing().getName();
                    message = command.substring(1)+ " (default " + currentBufferName + ") : ";
                    while (true) {
                        parameter = getParameter(environment, message, parameterStartValue, noMatch);
                        if (parameter.equals(""))
                            parameter = currentBufferName;
                        else {
                            LispBuffer b = environment.findBuffer(parameter);
                            if (b == null) {
                                parameterStartValue = parameter;
                                noMatch = true;
                                continue;
                            }
                        }
                        args.add(new LispString(parameter));
                        break;
                    }
                    break;
                case 'B': // -- Name of buffer, possibly nonexistent. todo: Completion
                    parameter = getParameter(environment, command.substring(1)+ " (default " + environment.getBufferCurrentForEditing().getName() + ") :", parameterStartValue, noMatch);
                    if (parameter.equals(""))
                        parameter = environment.getBufferCurrentForEditing().getName();
                    args.add(new LispString(parameter));
                    break;
                case 'c': // -- Character (no input method is used).
                    //ascii code of first key pressed
                    //TODO: keyEvent
                    break;
                case 'C': // -- Command name: symbol with interactive function definition. todo: Completion
                    while (true) {
                        parameter = getParameter(environment, message, parameterStartValue, noMatch);
                        LispSymbol cmd = environment.find(parameter);
                        if (cmd != null)
                            if (BuiltinsCheck.commandp(environment, cmd, null).equals(LispSymbol.ourT)) {
                                LispSymbol c = new LispSymbol(parameter, cmd.getFunction());
                                args.add(c);
                                break;
                            }
                        parameterStartValue = parameter;
                        noMatch = true;
                    }
                    break;
                case 'd': // -- Value of point as number. Does not do I/O.
                    args.add(new LispInteger(environment.getBufferCurrentForEditing().point()));
                    break;
                case 'D': // -- Directory name. todo: Completion
                    parameterStartValue = environment.getDefaultDirectory().getData();
                    while (true) {
                        parameter = getParameter(environment, message, parameterStartValue, noMatch);
                        File dir = new File(parameter);
                        if (dir.exists() && dir.isDirectory()) {
                            args.add(new LispString(parameter));
                            break;
                        }
                        parameterStartValue = parameter;
                        noMatch = true;
                    }
                    break;
                case 'e': // -- Parametrized event (i.e., one that's a list) that invoked this command.
                          // If used more than once, the Nth `e' returns the Nth parametrized event.
                          // This skips events that are integers or symbols.
                    //if no event: (error "command must be bound to an event with parameters")
                    break;
                case 'f': // -- Existing file name.
                    parameter = getParameter(environment, command.substring(1) + System.getProperty("user.home"), parameterStartValue, noMatch);
                    //list of existing files beginning from [what was printed] and ability to retype

                    break;
                case 'F': // -- Possibly nonexistent file name. -- no check
                    parameter = getParameter(environment, command.substring(1) + System.getProperty("user.home"), parameterStartValue, noMatch);
                    if (parameter.equals(System.getProperty("user.home")))
                        parameter += "#scratch.lisp#";
                    args.add(new LispString(parameter));
                    break;
                case 'G': // -- Possibly nonexistent file name, defaulting to just directory name.
                    parameter = getParameter(environment, command.substring(1) + System.getProperty("user.home"), parameterStartValue, noMatch);
                    args.add(new LispString(parameter));
                    break;
                case 'i': // -- Ignored, i.e. always nil. Does not do I/O.
                    args.add(LispSymbol.ourNil);
                    break;
                case 'k': // -- Key sequence (downcase the last event if needed to get a definition).
                    // 1 first printed char or ??
                    break;
                case 'K': // -- Key sequence to be redefined (do not downcase the last event).
                    break;
                case 'm': // -- Value of mark as number. Does not do I/O.

                    break;
                case 'M': // -- Any string. Inherits the current input method.
                    break;
                case 'n': // -- Number read using minibuffer.
                    break;
                case 'N': // -- Numeric prefix arg, or if none, do like code `n'.
                    break;
                case 'p': // -- Prefix arg converted to number. Does not do I/O.
                    break;
                case 'P': // -- Prefix arg in raw form. Does not do I/O.
                    break;
                case 'r': // -- Region: point and mark as 2 numeric args, smallest first. Does no I/O.
                    break;
                case 's': // -- Any string. Does not inherit the current input method.
                    break;
                case 'S': // -- Any symbol.
                    break;
                case 'U': // -- Mouse up event discarded by a previous k or K argument.
                    break;
                case 'v': // -- Variable name: symbol that is user-variable-p.
                    break;
                case 'x': // -- Lisp expression read but not evaluated.
                    break;
                case 'X': // -- Lisp expression read and evaluated.
                    break;
                case 'z': // -- Coding system.
                    break;
                case 'Z': // -- Coding system, nil if no prefix arg.
                    break;
                default:
                    throw new InvalidControlLetterException(codeLetter);
            }
        }
        return args;
    }

    */

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
                throw new WrongTypeArgument("symbolp", args[index].getClass().getSimpleName());
            value = (index+1 == args.length) ? LispSymbol.ourNil : args[index+1].evaluate(inner);
            ((LispSymbol) args[index]).setValue(value);
            environment.setVariable((LispSymbol) args[index]);
            index += 2;
        }
        return value;
    }

}