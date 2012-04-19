package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.exception.InternalException;
import org.jetbrains.emacs4ij.jelisp.exception.InvalidControlLetterException;
import org.jetbrains.emacs4ij.jelisp.exception.MarkerPointsNowhereException;
import org.jetbrains.emacs4ij.jelisp.exception.NotImplementedException;
import org.jetbrains.emacs4ij.jelisp.subroutine.Predicate;

import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/24/11
 * Time: 6:46 PM
 * To change this template use File | Settings | File Templates.
 */
public class SpecialFormInteractive implements Completer {
    private char myInteractiveChar;
    private String myNoMatchMessage;
    private List<LispObject> myArguments;
    private String[] myParameters;
    private int myIndex;

    private final static String ourStandardNoMatchMessage = JelispBundle.message("standard.no.match.msg");
    private final static String ourEmptyMessage = "";
    private Environment myEnvironment;
    private String myPrompt;
    private String myInitialInput;
    private String myParameterDefaultValue;
    private String myPromptDefaultValue = ourEmptyMessage;

    public SpecialFormInteractive (Environment environment, @Nullable String interactive) {
        myEnvironment = environment;
        myParameters = interactive == null ? null : interactive.split("\\\\n");
        myIndex = 0;
        myArguments = new ArrayList<>();
        myNoMatchMessage = ourEmptyMessage;
        try {
            myInteractiveChar = myParameters[0].charAt(0);
            myPrompt = myParameters[0].substring(1);
        } catch (IndexOutOfBoundsException e) {
            myInteractiveChar = 0;
            myPrompt = ourEmptyMessage;
        }
    }

    public String getPrompt() {
        return myPrompt;
    }

    public void setPrompt (String prompt) {
        myPrompt = prompt;
    }

    private void putArgument() {
        myEnvironment.getMiniBuffer().readParameter(this);
    }

    private void notifyMiniBuffer() {
        myEnvironment.getMiniBuffer().onInteractiveNoIoInput(this);
    }

    public String getInitialInput() {
        return myInitialInput;
    }

    public void setInitialInput(String initialInput) {
        myInitialInput = initialInput;
    }

    private void normalizePromptAndDefault() {
        if (myPromptDefaultValue.equals(ourEmptyMessage))
            return;
        Pattern pattern = Pattern.compile("\\s*:\\s*");
        Matcher matcher = pattern.matcher(myPrompt);
        int count = 0;
        while (matcher.find()) {
            count++;
            System.out.println("Match number " + count);
            System.out.println("start(): " + matcher.start());
            System.out.println("end(): " + matcher.end());
        }
        int start;
        try {
            start = matcher.start();
        } catch (IllegalStateException e) {
            start = myPrompt.length();
        }
        myPrompt = myPrompt.substring(0, start) + myPromptDefaultValue;
    }

    public boolean isFinished () {
        return myIndex == myParameters.length;
    }

    public boolean isNoMatch() {
        return !myNoMatchMessage.equals(ourEmptyMessage);
    }

    public List<LispObject> getArguments() {
        return myArguments;
    }

    public String getNoMatchMessage() {
        return myNoMatchMessage;
    }

    public boolean toShowSpecialNoMatchMessage() {
        return !myNoMatchMessage.equals(ourEmptyMessage) && !myNoMatchMessage.equals(ourStandardNoMatchMessage);
    }

    public void readNextArgument() {
        if (isFinished())
            return;
        if (myParameters == null)
            throw new InternalException();
        String command = myParameters[myIndex];
        myInitialInput = null;
        myPrompt = command.substring(1);
        myPromptDefaultValue = ourEmptyMessage;
        myInteractiveChar = command.charAt(0);
        myParameterDefaultValue = null;
        myNoMatchMessage = ourEmptyMessage;
        prepare();
    }

    private void addArg (LispObject arg) {
        if (isFinished()) {
            myArguments.set(myArguments.size() - 1, arg);
            return;
        }
        myArguments.add(arg);
        ++myIndex;
    }

    public void setNoMatch (String parameter) {
        myInitialInput = parameter;
        myNoMatchMessage = ourStandardNoMatchMessage;
        putArgument();
    }

    public void onReadParameter (String parameter) {
        switch (myInteractiveChar) {
            case 'a':
                LispSymbol f = myEnvironment.find(parameter);
                if (f != null) {
                    if (f.isFunction()) {
                        addArg(new LispSymbol(parameter, f.getFunction()));
                        return;
                    }
                }
                break;
            case 'b':
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
            case 'B':
                if (parameter.equals(""))
                    parameter = myParameterDefaultValue;
                addArg(new LispString(parameter));
                return;
            case 'c': // -- Character (todo no input method is used).
                addArg(new LispInteger(Integer.parseInt(parameter)));
                return;
            case 'C': //command
                LispSymbol cmd = myEnvironment.find(parameter);
                if (cmd != null)
                    if (Predicate.commandp(cmd, null).equals(LispSymbol.ourT)) {
                        addArg(cmd);
                        return;
                    }
                break;
            case 'd': // -- Value of point as number. todo Does not do I/O.
                addArg(new LispInteger(myEnvironment.getBufferCurrentForEditing().point()));
                break;
            case 'D': // -- Directory name.
                if (parameter.length() > 1) {
                    if (parameter.charAt(0) == '~') {
                        parameter = System.getProperty("user.home") + parameter.substring(1);
                    }
                }
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
            case 'f': // -- Existing file name. Directory also fits.
                if (parameter.length() > 1) {
                    if (parameter.charAt(0) == '~') {
                        parameter = System.getProperty("user.home") + parameter.substring(1);
                    }
                }
                File file = new File(parameter);
                if (file.exists()) {
                    addArg(new LispString(parameter));
                    return;
                }
                break;
            case 'F': // -- Possibly nonexistent file name. -- no check
                if (parameter.length() > 1) {
                    if (parameter.charAt(0) == '~') {
                        parameter = System.getProperty("user.home") + parameter.substring(1);
                    }
                }
                File ffile = new File(parameter);
                if (ffile.exists() && ffile.isDirectory()) {
                    String[] filling = ffile.list();
                    String firstFileName = null;
                    if (!parameter.endsWith("/"))
                        parameter += '/';
                    for (String fileName: filling) {
                        if (new File(parameter + fileName).isFile()) {
                            firstFileName = fileName;
                            break;
                        }
                    }
                    if (firstFileName != null)
                        parameter += firstFileName;
                }
                addArg(new LispString(parameter));
                return;
            case 'G': // -- Possibly nonexistent file name, defaulting to just directory name.
                if (parameter.length() > 1) {
                    if (parameter.charAt(0) == '~') {
                        parameter = System.getProperty("user.home") + parameter.substring(1);
                    }
                }
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
                try {
                    int n = Integer.parseInt(parameter);
                    addArg(new LispInteger(n));
                    return;
                } catch (NumberFormatException e1) {
                    try {
                        int n = (int) Double.parseDouble(parameter);
                        addArg(new LispInteger(n));
                        return;
                    } catch (NumberFormatException e2) {
                        //todo: don't show prompt
                        myNoMatchMessage = JelispBundle.message("number.no.match.msg");
                        myInitialInput = null;
                        putArgument();
                        return;
                    }
                }
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
                throw new InvalidControlLetterException(myInteractiveChar);
        }
        myInitialInput = parameter;
        myNoMatchMessage = ourStandardNoMatchMessage;
        putArgument();
    }

    private String defaultDirectory()  {
        return ((LispString) myEnvironment.getBufferCurrentForEditing().getLocalVariableValue("default-directory")).getData();
    }

    private void prepare () {
        switch (myInteractiveChar) {
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
                Integer markPosition = myEnvironment.getBufferCurrentForEditing().getMark().getPosition();
                if (markPosition == null)
                    throw new MarkerPointsNowhereException();
                addArg(new LispInteger(markPosition));
                notifyMiniBuffer();
                return;
            case 'M': // -- Any string. Inherits the current input method.
                break;
            case 'n': // -- Number read using minibuffer.
                break;
            case 'N': // -- Numeric prefix arg, or if none, do like code `n'.
                break;
            case 'p': // -- Prefix arg converted to number. Does not do I/O.
                notifyMiniBuffer();
                return;
            case 'P': // -- Prefix arg in raw form. Does not do I/O.
                addArg(myEnvironment.find("current-prefix-arg").getValue());
                notifyMiniBuffer();
                return;
            case 'r': // -- Region: point and mark as 2 numeric args, smallest first. Does no I/O.
                notifyMiniBuffer();
                return;
            case 's': // -- Any string. Does not inherit the current input method.
                break;
            case 'S': // -- Any symbol.
                break;
            case 'U': // -- Mouse up event discarded by a previous k or K argument.
                notifyMiniBuffer();
                return;
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
        normalizePromptAndDefault();
        putArgument();
    }

    private ArrayList<String> fileCompletions (String parameter, final boolean isDirectory) {
        //todo: if result is not unique, show "[Complete, but not unique]"
        if (parameter.length() > 1) {
            if (parameter.charAt(0) == '~') {
                parameter = System.getProperty("user.home") + parameter.substring(1);
            }
        }
        File d = new File(parameter);
        File parent;
        final String begin;
        if (!d.exists()) {
            int lastDelimiter = parameter.lastIndexOf('/');
            if (lastDelimiter == -1) {
                return new ArrayList<>();
            } else {
                parent = new File(parameter.substring(0, lastDelimiter+1));
                begin = parameter.substring(lastDelimiter+1);
            }
        } else {
            parent = d;
            begin = "";
        }
        File[] files =  parent.listFiles(new FilenameFilter() {
            @Override
            public boolean accept(File dir, String s) {
                if (s.length() < begin.length())
                    return false;
                if (!s.substring(0, begin.length()).equals(begin))
                    return false;
                if (!isDirectory)
                    return true;
                File f = new File(dir.getAbsolutePath() + '/' + s);
                return f.isDirectory();
            }
        });
        ArrayList<String> completions = new ArrayList<>();
        for (File file: files) {
            completions.add(file.getAbsolutePath());
        }
        return completions;
    }

    @Override
    public List<String> getCompletions (String parameter) {
        List<String> completions = new ArrayList<>();
        switch (myInteractiveChar) {
            case 'a':
                completions = GlobalEnvironment.INSTANCE.getFunctionList(parameter);
                break;
            case 'b': // -- Name of existing buffer.
                completions = GlobalEnvironment.INSTANCE.getBufferNamesList(parameter);
                break;
            case 'B': // -- Name of buffer, possibly nonexistent.
                completions = GlobalEnvironment.INSTANCE.getBufferNamesList(parameter);
                break;
            case 'C': // -- Command name: symbol with interactive function definition.
                completions = GlobalEnvironment.INSTANCE.getCommandList(parameter);
                break;
            case 'D': // -- Directory name.
                completions = fileCompletions(parameter, true);
                break;
            case 'f': // -- Existing file name.
                completions = fileCompletions(parameter, false);
                break;
            case 'F': // -- Possibly nonexistent file name. -- no check
                completions = fileCompletions(parameter, false);
                break;
            case 'G': // -- Possibly nonexistent file name, defaulting to just directory name.
                break;
            case 'v': // -- Variable name: symbol that is user-variable-p.
                break;
            case 'z': // -- Coding system.
                break;
            case 'Z': // -- Coding system, nil if no prefix arg.
                break;
        }
        if (!completions.isEmpty())
            Collections.sort(completions);
        return completions;
    }
}
