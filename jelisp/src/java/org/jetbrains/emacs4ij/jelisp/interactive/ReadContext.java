package org.jetbrains.emacs4ij.jelisp.interactive;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/20/12
 * Time: 11:02 AM
 * To change this template use File | Settings | File Templates.
 */
public class ReadContext {
    protected final static String ourStandardNoMatchMessage = JelispBundle.message("standard.no.match.msg");
    protected final static String ourEmptyMessage = "";
    protected String myPrompt;
    protected String myInitialInput;
    protected String myParameterDefaultValue;
    protected String myPromptDefaultValue = ourEmptyMessage;
    private LispSymbol myHistory;
    private int myHistoryPosition;
    private LispObject myDefault;



}
