package org.jetbrains.emacs4ij.jelisp.interactive;

import com.intellij.openapi.util.text.StringUtil;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.InternalException;
import org.jetbrains.emacs4ij.jelisp.exception.MarkerPointsNowhereException;
import org.jetbrains.emacs4ij.jelisp.exception.NotImplementedException;

import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/24/11
 * Time: 6:46 PM
 * To change this template use File | Settings | File Templates.
 */
public class SpecialFormInteractive extends InteractiveReader {
    private String[] myParameters;
    private int myIndex;
    private final StandardCompletionContext myCompletionContext = new StandardCompletionContext();

    //for tests
    public SpecialFormInteractive (Environment environment, String interactive) {
        super(environment, null);
        init(interactive);
    }

    public SpecialFormInteractive (Environment environment, LambdaOrSymbolWithFunction function, String interactive) {
        super(environment, function);
        init(interactive);
    }

    private void init (String interactive) {
        myParameters = StringUtil.isEmpty(interactive) ? new String[0] : interactive.split("\\\\n");
        myIndex = 0;
        try {
            myCompletionContext.InteractiveChar = myParameters[0].charAt(0);
            myPrompt = myParameters[0].substring(1);
        } catch (IndexOutOfBoundsException e) {
            myCompletionContext.InteractiveChar = 0;
            myPrompt = ourEmptyMessage;
        }
    }

    @Override
    public boolean isFinished () {
        return myIndex == myParameters.length;
    }

    @Override
    public boolean isNoMatch() {
        return !myCompletionContext.isMatch();
    }

    @Override
    public String getNoMatchMessage() {
        return myCompletionContext.getNoMatchMessage();
    }

    @Override
    public boolean toShowSpecialNoMatchMessage() {
        return myCompletionContext.toShowSpecialNoMatchMessage();
    }

    @Override
    public void readNextArgument() {
        if (isFinished())
            return;
        if (myParameters == null)
            throw new InternalException();
        String command = myParameters[myIndex];
        myInitialInput = null;
        myPrompt = command.substring(1);
        myPromptDefaultValue = ourEmptyMessage;

        myCompletionContext.InteractiveChar = command.charAt(0);

//        myInteractiveChar = command.charAt(0);
        myParameterDefaultValue = null;

        myCompletionContext.setMatch(true);
//        myCompletionContext.setInteractiveChar(myInteractiveChar);
        prepare();
    }

    private void addArg (LispObject arg) {
        if (isFinished()) {
            throw new InternalException("Entered more arguments than specified in interactive string!");
//            myArguments.set(myArguments.size() - 1, arg);
//            return;
        }
        myArguments.add(arg);
        ++myIndex;
    }

    @Override
    public List<String> getCompletions(String parameter) {
        return myCompletionContext.getCompletions(parameter);
    }

    @Override
    public void setNoMatch (String parameter) {
        myInitialInput = parameter;
        myCompletionContext.setMatch(false);
        putArgument();
    }

    @Override
    public void onReadParameter (String parameter) {
        LispObject arg = myCompletionContext.verify(myEnvironment, parameter, myParameterDefaultValue);
        if (myCompletionContext.isMatch()) {
            addArg(arg == null
                    ? parameter.equals("") ? new LispString(myParameterDefaultValue)
                    : new LispString(parameter)
                    : arg);
            return;
        }
        myInitialInput = parameter;
        myCompletionContext.setMatch(true);
        putArgument();
    }

    private String defaultDirectory()  {
        return ((LispString) myEnvironment.getBufferCurrentForEditing().getLocalVariableValue("default-directory")).getData();
    }

    private void prepare () {
        switch (myCompletionContext.InteractiveChar) {
            case 'b': // -- Name of existing buffer.
                myParameterDefaultValue = myEnvironment.getBufferCurrentForEditing().getName();
                myPromptDefaultValue = " (default " + myParameterDefaultValue + "): ";
                break;
            case 'B': // -- Name of buffer, possibly nonexistent.
                myParameterDefaultValue = myEnvironment.getBufferCurrentForEditing().getName();
                myPromptDefaultValue = " (default " + myParameterDefaultValue + "): ";
                break;
            case 'c':
                myEnvironment.getMiniBuffer().addCharListener();
                break;
            case 'd': // -- Value of point as number. Does not do I/O.
                addArg(new LispInteger(myEnvironment.getBufferCurrentForEditing().point()));
                notifyMiniBuffer();
                return;
            case 'D': // -- Directory name.
                myInitialInput = defaultDirectory();
                break;
            case 'e': // -- Parametrized event (i.e., one that's a list) that invoked this command.
                // If used more than once, the Nth `e' returns the Nth parametrized event.
                // This skips events that are integers or symbols.
                //if no event: (error "command must be bound to an event with parameters")
                //todo: notifyMiniBuffer(); return;
                throw new NotImplementedException("e character not implemented");
            case 'f': // -- Existing file name.
                myInitialInput = defaultDirectory();
                break;
            case 'F': // -- Possibly nonexistent file name. -- no check
                myInitialInput = defaultDirectory();
                break;
            case 'G': // -- Possibly nonexistent file name, defaulting to just directory name.
                myInitialInput = defaultDirectory();
                break;
            case 'i': // -- Ignored, i.e. always nil. Does not do I/O.
                addArg(LispSymbol.ourNil);
                notifyMiniBuffer();
                return;
            case 'k': // -- Key sequence (downcase the last event if needed to get a definition).
                throw new NotImplementedException("k character not implemented");
            case 'K': // -- Key sequence to be redefined (do not downcase the last event).
                throw new NotImplementedException("K character not implemented");
            case 'm': // -- Value of mark as number. Does not do I/O.
                addArg(new LispInteger(getMarkPosition()));
                notifyMiniBuffer();
                return;
            case 'N': // -- Numeric prefix arg, or if none, do like code `n'.
                LispInteger value = prefixArgToNumber();
                if (value == null) {
                    myCompletionContext.InteractiveChar = 'n';
                    break;
                }
                addArg(value);
                notifyMiniBuffer();
                return;
            case 'p': // -- Prefix arg converted to number. Does not do I/O.
                LispInteger v = prefixArgToNumber();
                if (v == null)
                    v = new LispInteger(1);
                addArg(v);
                notifyMiniBuffer();
                return;
            case 'P': // -- Prefix arg in raw form. Does not do I/O.
                addArg(myEnvironment.find("current-prefix-arg").getValue());
                notifyMiniBuffer();
                return;
            case 'r': // -- Region: point and mark as 2 numeric args, smallest first.
                int point = myEnvironment.getBufferCurrentForEditing().point();
                int mark = getMarkPosition();
                int min = point < mark ? point : mark;
                int max = point < mark ? mark : point;
                addArg(new LispInteger(min));
                addArg(new LispInteger(max));
                notifyMiniBuffer();
                return;
            case 'S': // -- Any symbol.
                //todo: when read a symbol, set <SPACE> to work as <RET>
                break;
            case 'U':
                //todo: get the up-event that was discarded (if any) after ‘k’ or ‘K’ read a down-event. Otherwise nil
                addArg(LispSymbol.ourNil);
                notifyMiniBuffer();
                return;
            case 'x': // -- Lisp expression read but not evaluated.
                //todo: try to parse and show [parse error message] while type
                break;
            case 'X': // -- Lisp expression read and evaluated.
                //todo: try to parse and show [parse error message] while type
                break;
        }
        normalizePromptAndDefault();
        putArgument();
    }

    private LispInteger prefixArgToNumber () {
        LispObject value = myEnvironment.find("current-prefix-arg").getValue();
        if (value.equals(LispSymbol.ourNil))
            return null;
        return value instanceof LispInteger ? (LispInteger) value : new LispInteger(1);
    }

    private int getMarkPosition () {
        Integer markPosition = myEnvironment.getBufferCurrentForEditing().getMark().getPosition();
        if (markPosition == null)
            throw new MarkerPointsNowhereException();
        return markPosition;
    }
}
