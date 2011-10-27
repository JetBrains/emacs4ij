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
   // private enum ParameterCode {a, b, B, c, C, d, D, e, f, F, G, i, k, K, m, M, n, N, p, P, r, s, S, U, v, x, X, z, Z}
   // private ParameterCode myParameterCode;
    private char myParameterCode;
    private Environment myEnvironment;
    private String myPrompt;
    private String myParameterStartValue;
    private boolean myNoMatch;
    //private LispList myArguments;
    private String[] myParameters;
    private int myIndex;

    private LObject myArgument;
    private boolean myParameterRead;


    public SpecialFormInteractive (Environment environment, String interactive) {
        myEnvironment = environment;
        myParameters = interactive.split("\\\\n");
        //myArguments = new LispList();
        myIndex = 0;
        myParameterRead = false;

        if (isFinished())
            return;
        String command = myParameters[myIndex];
        myParameterStartValue = null;
        myPrompt = command.substring(1);
        myNoMatch = false;
        myParameterCode = command.charAt(0);
    }

    public synchronized LObject getArgument() {
        while (!myParameterRead) {
            try {
                wait();
            } catch (InterruptedException e) {
                System.out.println("Interactive::read -- InterruptedException caught");
            }
        }
        System.out.println("read " + myArgument);
        myParameterRead = false;
        notify();
        return myArgument;
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

    public synchronized void putArgument() {
        while (myParameterRead) {
            try {
                wait();
            } catch (InterruptedException e) {
                System.out.println("Interactive::askForParameter -- InterruptedException caught");
            }
        }
        LispMiniBuffer miniBuffer = myEnvironment.getMiniBuffer();
        miniBuffer.readArgument(this);
    }

    private synchronized void addArg (LObject arg) {
        System.out.println("Got: " + arg) ;
        myArgument = arg;
        ++myIndex;
        myParameterRead = true;

        if (!isFinished()) {
            String command = myParameters[myIndex];
            myParameterStartValue = null;
            myPrompt = command.substring(1);
            myNoMatch = false;
            myParameterCode = command.charAt(0);
        }

        notify();
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
