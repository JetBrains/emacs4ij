package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.InvalidControlLetterException;

import java.io.File;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/24/11
 * Time: 6:46 PM
 * To change this template use File | Settings | File Templates.
 */
public class SpecialFormInteractive {
    private char myParameterCode;
    private Environment myEnvironment;
    private String myPrompt;
    private String myPromptDefaultValue;
    private String myParameterStartValue;
    private String myParameterDefaultValue;
    private boolean myNoMatch;
    private LispList myArguments;
    private String[] myParameters;
    private int myIndex;

    public SpecialFormInteractive (Environment environment, String interactive) {
        myEnvironment = environment;
        myParameters = interactive.split("\\\\n");
        myIndex = 0;
        myArguments = new LispList();
    }

    public boolean isFinished () {
        return myIndex == myParameters.length;
    }

    public String getPrompt() {
        return myPrompt;
    }

    public String getParameterStartValue() {
        return myParameterStartValue;
    }

    public boolean isNoMatch() {
        return myNoMatch;
    }

    public LispList getArguments() {
        return myArguments;
    }

    public String getPromptDefaultValue () {
        return myPromptDefaultValue;
    }

    public void readNextArgument() {
        if (isFinished())
            return;
        String command = myParameters[myIndex];
        myParameterStartValue = null;
        myPrompt = command.substring(1);
        myPromptDefaultValue = "";
        myNoMatch = false;
        myParameterCode = command.charAt(0);
        myParameterDefaultValue = null;
        prepare();
    }

    public void putArgument() {
        LispMiniBuffer miniBuffer = myEnvironment.getMiniBuffer();
        miniBuffer.readArgument(this);
    }

    private void addArg (LObject arg) {
        System.out.println("Got: " + arg) ;
        myArguments.add(arg);
        ++myIndex;
    }

    public void onReadParameter (String parameter) {
        switch (myParameterCode) {
            case 'a':
                LispSymbol f = myEnvironment.find(parameter);
                if (f != null) {
                    if (f.isFunction()) {
                        addArg(new LispSymbol(parameter, f.getFunction()));
                        return;
                    }
                }
                break;
            case 'b': // -- Name of existing buffer.  todo: Completion
                if (parameter.equals("")) {
                    parameter = myParameterDefaultValue;
                    addArg(new LispString(parameter));
                    return;
                } else {
                    LispBuffer b = myEnvironment.findBuffer(parameter);
                    if (b != null) {
                        addArg(new LispString(parameter));
                        return;
                    }
                }
                break;
            case 'B': // -- Name of buffer, possibly nonexistent. todo: Completion
                if (parameter.equals(""))
                    parameter = myParameterDefaultValue;
                addArg(new LispString(parameter));
                return;
            case 'c': // -- Character (no input method is used).
                //ascii code of first key pressed
                //TODO: keyEvent
                return;
            case 'C': // -- Command name: symbol with interactive function definition. todo: Completion
                LispSymbol cmd = myEnvironment.find(parameter);
                if (cmd != null)
                    if (BuiltinsCheck.commandp(myEnvironment, cmd, null).equals(LispSymbol.ourT)) {
                        LispSymbol c = new LispSymbol(parameter, cmd.getFunction());
                        addArg(c);
                        return;
                    }
                break;
            /*case 'd': // -- Value of point as number. Does not do I/O.
                args.add(new LispInteger(environment.getBufferCurrentForEditing().point()));
                break;  */
            case 'D': // -- Directory name. todo: Completion
                File dir = new File(parameter);
                if (dir.exists() && dir.isDirectory()) {
                    addArg(new LispString(parameter));
                    return;
                }
                break;
            /*case 'e': // -- Parametrized event (i.e., one that's a list) that invoked this command.
                // If used more than once, the Nth `e' returns the Nth parametrized event.
                // This skips events that are integers or symbols.
                //if no event: (error "command must be bound to an event with parameters")
                break;*/
            case 'f': // -- Existing file name. todo: Completion
                //list of existing files beginning from [what was printed] and ability to retype
                File file = new File(parameter);
                if (file.exists() && file.isFile()) {
                    addArg(new LispString(parameter));
                    return;
                }
                break;
            case 'F': // -- Possibly nonexistent file name. -- no check
                if (parameter.equals(myParameterStartValue))
                    parameter += myParameterDefaultValue;
                addArg(new LispString(parameter));
                return;
            case 'G': // -- Possibly nonexistent file name, defaulting to just directory name.
                addArg(new LispString(parameter));
                return;
            /*case 'i': // -- Ignored, i.e. always nil. Does not do I/O.
                args.add(LispSymbol.ourNil);
                break;*/

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
                throw new InvalidControlLetterException(myParameterCode);
        }
        myParameterStartValue = parameter;
        myNoMatch = true;
        putArgument();
    }


    private void prepare () {
        switch (myParameterCode) {
            case 'b': // -- Name of existing buffer.
                myParameterDefaultValue = myEnvironment.getBufferCurrentForEditing().getName();
                myPromptDefaultValue = " (default " + myParameterDefaultValue + ") :";
                break;
            case 'B': // -- Name of buffer, possibly nonexistent.
                myParameterDefaultValue = myEnvironment.getBufferCurrentForEditing().getName();
                myPromptDefaultValue = " (default " + myEnvironment.getBufferCurrentForEditing().getName() + ") :";
                break;
            case 'd': // -- Value of point as number. Does not do I/O.
                addArg(new LispInteger(myEnvironment.getBufferCurrentForEditing().point()));
                return;
            case 'D': // -- Directory name.
                myParameterStartValue = myEnvironment.getDefaultDirectory().getData();
                break;
            case 'e': // -- Parametrized event (i.e., one that's a list) that invoked this command.
                // If used more than once, the Nth `e' returns the Nth parametrized event.
                // This skips events that are integers or symbols.
                //if no event: (error "command must be bound to an event with parameters")
                //todo, no IO
                return;
            case 'f': // -- Existing file name.
                myParameterStartValue = myEnvironment.getDefaultDirectory().getData();
                break;
            case 'F': // -- Possibly nonexistent file name. -- no check
                myParameterStartValue = myEnvironment.getDefaultDirectory().getData();
                myParameterDefaultValue = myEnvironment.getBufferCurrentForEditing().getName();
                break;
            case 'G': // -- Possibly nonexistent file name, defaulting to just directory name.
                myParameterStartValue = myEnvironment.getDefaultDirectory().getData();
                break;
            case 'i': // -- Ignored, i.e. always nil. Does not do I/O.
                addArg(LispSymbol.ourNil);
                return;

            // terra incognita =)

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
        }
        putArgument();
    }

}
