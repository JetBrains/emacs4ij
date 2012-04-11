package org.jetbrains.emacs4ij.jelisp.subroutine;

import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.util.text.StringUtil;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/4/12
 * Time: 3:04 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class Minibuffer {
    private Minibuffer() {}

    @Subroutine("completing-read")
    public static LispObject completingRead (Environment environment, LispString prompt, LispObject collection,
                                           @Optional LispSymbol predicate, LispObject requireMatch,
                                           LispObject initialInput, LispObject history, LispObject defaultValue,
                                           LispObject inheritInputMethod) {

        LispObject minibufferCompletionFilenameValue = environment.find("minibuffer-completing-file-name").getValue();

        boolean isMinibufferCompletingFilename = !Predicate.isNil(minibufferCompletionFilenameValue)
                && !Core.eqs(minibufferCompletionFilenameValue, new LispSymbol("tForOneTime"));
        String keymapName = Predicate.isNil(requireMatch)
                ? isMinibufferCompletingFilename ? "minibuffer-local-filename-completion-map"
                : "minibuffer-local-completion-map"
                : isMinibufferCompletingFilename ? "minibuffer-local-filename-must-match-map"
                : "minibuffer-local-must-match-map";

        environment.setVariable(new LispSymbol("minibuffer-completion-table", collection));
        environment.setVariable(new LispSymbol("minibuffer-completion-predicate", Core.thisOrNil(predicate)));
        environment.setVariable(new LispSymbol("minibuffer-completion-confirm",
                requireMatch.equals(LispSymbol.ourT) ? LispSymbol.ourNil : requireMatch));

        String alreadyTyped = "";
        if (!Predicate.isNil(initialInput)) {
            LispObject init = initialInput;
            if (initialInput instanceof LispList) {
                // initialInput may be (string . position) but i ignore the position
                init = ((LispList) initialInput).car();
            }
            if (!(init instanceof LispString))
                throw new WrongTypeArgumentException("stringp", init);
            alreadyTyped = ((LispString) init).getData();
        }

        LispSymbol historySymbol = environment.find("minibuffer-history");
        int historyPosition = 0;
        if (!Predicate.isNil(history)) {
            if (history instanceof LispSymbol) {
                historySymbol = (LispSymbol) history;
            } else if (history instanceof LispList) {
                if (((LispList) history).car() instanceof LispSymbol)
                    throw new WrongTypeArgumentException("symbolp", ((LispList) history).car());
                historySymbol = (LispSymbol) ((LispList) history).car();

                LispObject cdr = ((LispList) history).cdr();
                if (!cdr.equals(LispSymbol.ourNil) && !(cdr instanceof LispInteger)) {
                    throw new WrongTypeArgumentException("integerp", cdr);
                }
                if (!cdr.equals(LispSymbol.ourNil)) {
                    historyPosition = ((LispInteger) cdr).getData();
                }
            } else {
                throw new WrongTypeArgumentException("symbolp or (symbol . integer)", history);
            }
        }

        LispObject read = readFromMinibuffer(environment, keymapName, alreadyTyped, prompt.getData(),
                historySymbol, historyPosition, defaultValue);

        if (Predicate.isNil(read))
            return Predicate.isNil(defaultValue)
                    ? LispSymbol.ourNil
                    : defaultValue instanceof LispList ? ((LispList) defaultValue).car() : defaultValue;
        return read;
    }

    private static LispObject readFromMinibuffer (Environment environment, String keymapName, String initialInput, String prompt,
                                              LispSymbol history, int historyPosition, LispObject default_) {
        LispKeymap oldKeymap = environment.getActiveKeymap();
        environment.setActiveKeymap(keymapName);




        environment.setActiveKeymap(oldKeymap);
        return null;
    }

    private static boolean isMinibufferWindow (Environment environment, @Nullable LispWindow window) {
        if (window == null)
            window = environment.getSelectedWindow();
        return window != null && window == environment.getMinibufferWindow();
    }
    
    @Subroutine("window-minibuffer-p")
    public static LispSymbol windowMinibufferP (Environment environment, @Optional LispObject window) {
        if (!Predicate.isNil(window) && !(window instanceof LispWindow))
            throw new WrongTypeArgumentException("window-live-p", window);
        if (Predicate.isNil(window))
            window = environment.getSelectedWindow();
        return LispSymbol.bool(isMinibufferWindow(environment, (LispWindow) window));
    }

    //todo: compiled lisp f
    @Subroutine(value = "minibuffer-complete", isCmd = true)
    public static void minibufferComplete (Environment environment) {
        try {
            LispMiniBuffer miniBuffer = environment.getMiniBuffer();
            String parameter = miniBuffer.readInputString();
            List<String> completions = miniBuffer.getCompletions(parameter);
            if (completions.isEmpty()) {
                miniBuffer.setNoMatch(parameter);
            } else {
                if (completions.size() == 1) {
                    parameter = completions.get(0);
                } else {
                    parameter = StringUtil.commonPrefix(completions.get(0), completions.get(completions.size() - 1));
                }
                miniBuffer.setInputStartValue(parameter);
                miniBuffer.updateEditorText();

                if (completions.size()>1) {
                    StringBuilder message = new StringBuilder(JelispBundle.message("possible.completions"));
                    for (String name: completions) {
                        message.append(name).append("\n");
                    }
                    Messages.showInfoMessage(message.toString(), JelispBundle.message("possible.completions.title"));
                }
            }
        } catch (LispException exc) {
            GlobalEnvironment.echoError(exc.getMessage());
//            Messages.showErrorDialog(exc.getMessage(), JelispBundle.message("auto.complete.error.title"));
        }
    }

    //todo: compiled lisp f
    @Subroutine(value = "minibuffer-complete-word", isCmd = true)
    public static void minibufferCompleteWord (Environment environment) {
        minibufferComplete(environment);
    }

    //todo: compiled lisp f
    @Subroutine(value = "minibuffer-completion-help", isCmd = true)
    public static void minibufferCompletionHelp (Environment environment) {
        //todo all possible completions
    }

    @Subroutine("redisplay")
    public static LispSymbol redisplay (Environment environment, @Optional LispObject force) {
        System.err.println("redisplay");
        return LispSymbol.ourT;
    }
}
