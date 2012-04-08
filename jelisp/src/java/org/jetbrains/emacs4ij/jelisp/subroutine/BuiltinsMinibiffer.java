package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/4/12
 * Time: 3:04 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class BuiltinsMinibiffer {
    private BuiltinsMinibiffer() {}

    @Subroutine("completing-read")
    public static LispObject completingRead (Environment environment, LispString prompt, LispObject collection,
                                           @Optional LispSymbol predicate, LispObject requireMatch,
                                           LispObject initialInput, LispObject history, LispObject defaultValue,
                                           LispObject inheritInputMethod) {

        LispObject minibufferCompletionFilenameValue = environment.find("minibuffer-completing-file-name").getValue();
        boolean isMinibufferCompletingFilename = !BuiltinPredicates.isNil(minibufferCompletionFilenameValue)
                && !BuiltinsCore.eqs(minibufferCompletionFilenameValue, new LispSymbol("tForOneTime"));
        String keymapName = BuiltinPredicates.isNil(requireMatch)
                ? isMinibufferCompletingFilename ? "minibuffer-local-filename-completion-map"
                : "minibuffer-local-completion-map"
                : isMinibufferCompletingFilename ? "minibuffer-local-filename-must-match-map"
                : "minibuffer-local-must-match-map";

        environment.setVariable(new LispSymbol("minibuffer-completion-table", collection));
        environment.setVariable(new LispSymbol("minibuffer-completion-predicate", BuiltinsCore.thisOrNil(predicate)));
        environment.setVariable(new LispSymbol("minibuffer-completion-confirm",
                requireMatch.equals(LispSymbol.ourT) ? LispSymbol.ourNil : requireMatch));

        String alreadyTyped = "";
        if (!BuiltinPredicates.isNil(initialInput)) {
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
        if (!BuiltinPredicates.isNil(history)) {
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

        if (BuiltinPredicates.isNil(read))
            return BuiltinPredicates.isNil(defaultValue)
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
        if (!BuiltinPredicates.isNil(window) && !(window instanceof LispWindow))
            throw new WrongTypeArgumentException("window-live-p", window);
        if (BuiltinPredicates.isNil(window))
            window = environment.getSelectedWindow();
        return LispSymbol.bool(isMinibufferWindow(environment, (LispWindow) window));
    }




}
