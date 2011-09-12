package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.InvalidControlLetterException;
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
        for (LispObject var: varList.getData()) {
            if (var instanceof LispList) {
                LispSymbol symbol = (LispSymbol) ((LispList) var).car();
                LispList valueForm = ((LispList) var).cdr();
                LispObject value = valueForm.car().evaluate(inner);
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

    private static LispObject executeLet (boolean isStar, Environment environment, List<LispObject> args) {
        /* (let(*) VARLIST BODY...)
        Bind variables according to VARLIST then eval BODY.
        The value of the last form in BODY is returned.
        Each element of VARLIST is a symbol (which is bound to nil)
        or a list (SYMBOL VALUEFORM) (which binds SYMBOL to the value of VALUEFORM).

        let:  All the VALUEFORMs are evalled before any symbols are bound.
        let*: Each VALUEFORM can refer to the symbols already bound by this VARLIST.
        */
        Environment inner = new Environment(environment);
        LispList varList = (LispList) args.get(0);
        bindLetVariables(isStar, inner, varList);

        // eval body
        LispObject result = LispSymbol.ourNil;
        for (int i=1; i!=args.size(); ++i) {
            result = args.get(i).evaluate(inner);
        }
        return result;
    }

    @Subroutine(value = "quote", exact = 1)
    public static LispObject quote(Environment environment, List<LispObject> args) {
        return args.get(0);
    }
    
    @Subroutine("defmacro")
    public static LispObject defineMacro (Environment environment, List<LispObject> args) {
        throw new NotImplementedException();
    }

    @Subroutine("let")
    public static LispObject let (Environment environment, List<LispObject> args) {
        return executeLet(false, environment, args);
    }

    @Subroutine("let*")
    public static LispObject letStar (Environment environment, List<LispObject> args) {
        return executeLet(true, environment, args);
    }

    @Subroutine("interactive")
    public static LispObject interactive(Environment environment, List<LispObject> args) {
        throw new NotImplementedException();

        /*if (args.size() > 1) {
            throw new WrongNumberOfArgumentsException("interactive");
        }
        if (args.size() == 1) {
            LispObject a = args.get(0);
            if (!(a instanceof LispString)) {
                LispObject result = a.evaluate(environment);
                if (result instanceof LispList)
                    return result;
                throw new WrongTypeArgument("LispList", args.get(0).getClass().toString());
            }
            return null;//processInteractiveString((LispString) a, environment);
        }
        return LispSymbol.ourNil; */
    }

    @Subroutine("cond")
    public static LispObject cond(Environment environment, List<LispObject> args) {
        LispObject result = LispSymbol.ourNil;
        for (int i=0; i!=args.size(); ++i) {
            LispObject clause = args.get(i);
            if (!(clause instanceof LispList)) {
                if (clause.equals(LispSymbol.ourNil))
                    continue;
                throw new WrongTypeArgument("LispList", clause.getClass().toString());
            }
            if (((LispList) clause).isEmpty())
                continue;
            LispObject condition = ((LispList) clause).car().evaluate(environment);
            if (condition != LispSymbol.ourNil) {
                List<LispObject> data = ((LispList) clause).cdr().getData();
                result = condition;
                for (int k = 0; k != data.size(); ++k)
                    result = data.get(k).evaluate(environment);
                return result;
            }
        }
        return result;
    }
    @Subroutine(value = "while", min = 1)
    public static LispObject lispWhile(Environment environment, List<LispObject> args) {
        Environment inner = new Environment(environment);
        LispObject condition = args.get(0).evaluate(inner);
        while (condition != LispSymbol.ourNil) {
            for (int i = 1; i != args.size(); ++i)
                args.get(i).evaluate(inner);
            condition = args.get(0).evaluate(inner);
        }
        return condition;
    }
    @Subroutine(value = "if", min = 1)
    public static LispObject lispIf(Environment environment, List<LispObject> args) {
        LispObject condition = args.get(0).evaluate(environment);
        if (condition != LispSymbol.ourNil) {
            if (args.size() > 1)
                return args.get(1).evaluate(environment);
            return LispSymbol.ourT;
        }
        LispObject result = LispSymbol.ourNil;
        for (int i=2; i<args.size(); ++i) {
            result = args.get(i).evaluate(environment);
        }
        return result;
    }
    @Subroutine("and")
    public static LispObject lispAnd(Environment environment, List<LispObject> args) {
        LispObject result = LispSymbol.ourT;
        for (int i=0; i!=args.size(); ++i) {
            result = args.get(i).evaluate(environment);
            if (result == LispSymbol.ourNil)
                return result;
        }
        return result;
    }
    @Subroutine("or")
    public static LispObject lispOr(Environment environment, List<LispObject> args) {
        for (int i=0; i!=args.size(); ++i) {
            LispObject result = args.get(i).evaluate(environment);
            if (result != LispSymbol.ourNil)
                return result;
        }
        return LispSymbol.ourNil;
    }
    @Subroutine(value = "defvar", min = 1, max = 3)
    public static LispObject defineVariable(Environment environment, List<LispObject> args) {
        String name = ((LispSymbol) args.get(0)).getName();
        LispSymbol  variable = environment.find(name);
        if (variable == null) {
            variable = (LispSymbol) args.get(0);
            if (args.size() > 1)
                variable.setValue(args.get(1).evaluate(environment));
            if (args.size() == 3) {
                LispString docString = (LispString) args.get(2);
                variable.setVariableDocumentation(docString);
            }
        } else {
            if (variable.getValue().equals(LispSymbol.ourVoid) && (args.size() > 1))
                variable.setValue(args.get(1).evaluate(environment));
            if (args.size() == 3) {
                LispString docString = (LispString) args.get(2);
                LispObject docObject = variable.getVariableDocumentation();
                if (docObject != LispSymbol.ourNil && !(docObject.equals(docString)))
                   variable.setVariableDocumentation(docString);
            }
        }
        environment.getGlobalEnvironment().defineSymbol(variable);
        return args.get(0);
    }
    @Subroutine(value = "defun", min = 2)
    public static LispObject defineFunction(Environment environment, List<LispObject> args) {
        String name = ((LispSymbol) args.get(0)).getName();
        LispSymbol symbol = environment.find(name);
        LispSymbol f = symbol != null ? symbol : (LispSymbol) args.get(0);
        LispList functionCell = new LispList(new LispSymbol("lambda"));
        for (int i=1; i!=args.size(); ++i)
            functionCell.add(args.get(i));
        f.setFunction(functionCell);
        environment.getGlobalEnvironment().defineSymbol(f);
        return args.get(0);
    }

    private String getParameter (String message) {
        //TODO get Editor; save old header; read parameter from text field; set old header back
        throw new NotImplementedException();
    }

    private LispList processInteractiveString (LispString interactiveString, Environment environment) {
        String[] commands = interactiveString.toString().split("\n");
        LispList args = new LispList();
        for (int i = 0, commandsLength = commands.length; i < commandsLength; i++) {
            String command = commands[i];
            String parameter = "";
            char codeLetter = command.charAt(0);
            switch (codeLetter) {
                case 'a': // -- Function name: symbol with a function definition.
                    String message = command.substring(1);
                    while (true) {
                        parameter = getParameter(message);
                        try {
                            environment.find(parameter);
                            args.add(new LispSymbol(parameter));
                            break;
                        } catch (RuntimeException e) {
                            message = command.substring(1) + parameter + " [No Match]";
                        }
                    }
                    break;
                case 'b': // -- Name of existing buffer. No check
                    parameter = getParameter(command.substring(1)+ " (default *scratch*) :");
                    if (parameter.equals(""))
                        parameter = "*scratch*";
                    args.add(new LispString(parameter));
                    break;
                case 'B': // -- Name of buffer, possibly nonexistent.
                    //behaves the same way as b
                    break;
                case 'c': // -- Character (no input method is used).
                    //ascii code of first key pressed
                    //TODO: keyEvent
                    break;
                case 'C': // -- Command name: symbol with interactive function definition.
                    //list of possible commands beginning from [what was printed] and ability to retype
                    break;
                case 'd': // -- Value of point as number.  Does not do I/O.
                    break;
                case 'D': // -- Directory name.
                    parameter = getParameter(command.substring(1) + System.getProperty("user.home"));
                    args.add(new LispString(parameter));
                    break;
                case 'e': // -- Parametrized event (i.e., one that's a list) that invoked this command.
                          // If used more than once, the Nth `e' returns the Nth parametrized event.
                          // This skips events that are integers or symbols.
                    //if no event: (error "command must be bound to an event with parameters")
                    break;
                case 'f': // -- Existing file name.
                    parameter = getParameter(command.substring(1) + System.getProperty("user.home"));
                    //list of existing files beginning from [what was printed] and ability to retype

                    break;
                case 'F': // -- Possibly nonexistent file name. -- no check
                    parameter = getParameter(command.substring(1) + System.getProperty("user.home"));
                    if (parameter.equals(System.getProperty("user.home")))
                        parameter += "#scratch.lisp#";
                    args.add(new LispString(parameter));
                    break;
                case 'G': // -- Possibly nonexistent file name, defaulting to just directory name.
                    parameter = getParameter(command.substring(1) + System.getProperty("user.home"));
                    args.add(new LispString(parameter));
                    break;
                case 'i': // -- Ignored, i.e. always nil.  Does not do I/O.
                    args.add(LispSymbol.ourNil);
                    break;
                case 'k': // -- Key sequence (downcase the last event if needed to get a definition).
                    // 1 first printed char or ??
                    break;
                case 'K': // -- Key sequence to be redefined (do not downcase the last event).
                    break;
                case 'm': // -- Value of mark as number.  Does not do I/O.

                    break;
                case 'M': // -- Any string.  Inherits the current input method.
                    break;
                case 'n': // -- Number read using minibuffer.
                    break;
                case 'N': // -- Numeric prefix arg, or if none, do like code `n'.
                    break;
                case 'p': // -- Prefix arg converted to number.  Does not do I/O.
                    break;
                case 'P': // -- Prefix arg in raw form.  Does not do I/O.
                    break;
                case 'r': // -- Region: point and mark as 2 numeric args, smallest first.  Does no I/O.
                    break;
                case 's': // -- Any string.  Does not inherit the current input method.
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

}
