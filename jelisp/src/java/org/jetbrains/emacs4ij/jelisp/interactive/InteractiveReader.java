package org.jetbrains.emacs4ij.jelisp.interactive;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.LambdaOrSymbolWithFunction;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/20/12
 * Time: 10:50 AM
 * To change this template use File | Settings | File Templates.
 */
public abstract class InteractiveReader implements Completer {
    protected final static String ourEmptyMessage = "";

    protected Environment myEnvironment;
    protected LambdaOrSymbolWithFunction myCommand; //for whom these args are
    protected List<LispObject> myArguments = new ArrayList<>();

    protected String myPrompt;
    protected String myInitialInput;
    protected String myParameterDefaultValue = ourEmptyMessage;
    protected String myPromptDefaultValue = ourEmptyMessage;

    InteractiveReader (Environment environment, @Nullable LambdaOrSymbolWithFunction command) {
        myEnvironment = environment;
        myCommand = command;
    }

//    public static InteractiveReader create (Environment environment, LambdaOrSymbolWithFunction command, LispObject args) {
//        if (args instanceof LispString)
//            return new SpecialFormInteractive(environment, command, ((LispString) args).getData());
//        if (args instanceof LispList)
//            return new EmptyReader(environment, command, (LispList) args);
//        return null;
//    }

    public abstract boolean isFinished ();
    public abstract boolean isNoMatch();
    public abstract String getNoMatchMessage();
    public abstract boolean toShowSpecialNoMatchMessage();
    public abstract void readNextArgument();
    public abstract void setNoMatch(String parameter);
    public abstract void onReadParameter(String parameter);

    public List<LispObject> getArguments() {
        return myArguments;
    }

    public LambdaOrSymbolWithFunction getCommand() {
        return myCommand;
    }

    public Environment getEnvironment() {
        return myEnvironment;
    }

    public String getPrompt() {
        return myPrompt;
    }

    protected void putArgument() {
        myEnvironment.getMinibuffer().readParameter(this);
    }

    protected void notifyMiniBuffer() {
        myEnvironment.getMinibuffer().onInteractiveNoIoInput(this);
    }

    public String getInitialInput() {
        return myInitialInput;
    }

    public void setInitialInput(String initialInput) {
        myInitialInput = initialInput;
    }

    protected void normalizePromptAndDefault() {
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
}
