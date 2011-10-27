package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;

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
    private String myParameterStartValue;
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

    public void readNextArgument() {
        if (isFinished())
            return;
        String command = myParameters[myIndex];
        myParameterStartValue = null;
        myPrompt = command.substring(1);
        myNoMatch = false;
        myParameterCode = command.charAt(0);

        putArgument();
    }

    public synchronized void putArgument() {
        LispMiniBuffer miniBuffer = myEnvironment.getMiniBuffer();
        miniBuffer.readArgument(this);
    }

    private void addArg (LObject arg) {
        System.out.println("Got: " + arg) ;
        myArguments.add(arg);
        ++myIndex;
    }

    public synchronized void onReadParameter (String parameter) {
        switch (myParameterCode) {
            case 'a':
                LispSymbol f = myEnvironment.find(parameter);
                if (f != null) {
                    if (f.isFunction()) {
                        addArg(new LispSymbol(parameter, f.getFunction()));
                        return;
                    }
                    myParameterStartValue = parameter;
                    myNoMatch = true;
                    putArgument();
                }
                break;
            case 'b':
                break;
            case 'B':
                break;
            case 'c':
                break;
            case 'C':
                break;
            case 'd':
                break;
            case 'D':
                break;
            case 'e':
                break;
            case 'f':
                break;
            case 'F':
                break;
            case 'G':
                break;
            case 'i':
                break;
            case 'k':
                break;
            case 'K':
                break;
            case 'm':
                break;
            case 'M':
                break;
            case 'n':
                break;
            case 'N':
                break;
            case 'p':
                break;
            case 'P':
                break;
            case 'r':
                break;
            case 's':
                break;
            case 'S':
                break;
            case 'U':
                break;
            case 'v':
                break;
            case 'x':
                break;
            case 'X':
                break;
            case 'z':
                break;
            case 'Z':
                break;
        }
        throw new RuntimeException("unknown parameter code!");
    }



}
