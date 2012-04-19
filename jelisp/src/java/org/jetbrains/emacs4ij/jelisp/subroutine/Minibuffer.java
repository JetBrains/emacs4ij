package org.jetbrains.emacs4ij.jelisp.subroutine;

import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.util.text.StringUtil;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.InternalException;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

import java.util.ArrayList;
import java.util.Collections;
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

    private static final LispSymbol ourMinibufferHistory;
    static {
        ourMinibufferHistory = GlobalEnvironment.INSTANCE.find("minibuffer-history");
        if (ourMinibufferHistory == null)
            throw new InternalException("null minibuffer-history");
    }

    private static LispList history (LispObject historyObject) {
        LispSymbol historySymbol = ourMinibufferHistory;
        int historyPosition = 0;
        if (!Predicate.isNil(historyObject)) {
            if (historyObject instanceof LispSymbol) {
                historySymbol = (LispSymbol) historyObject;
            } else if (historyObject instanceof LispList) {
                if (((LispList) historyObject).car() instanceof LispSymbol)
                    throw new WrongTypeArgumentException("symbolp", ((LispList) historyObject).car());
                historySymbol = (LispSymbol) ((LispList) historyObject).car();

                LispObject cdr = ((LispList) historyObject).cdr();
                if (!cdr.equals(LispSymbol.ourNil) && !(cdr instanceof LispInteger)) {
                    throw new WrongTypeArgumentException("integerp", cdr);
                }
                if (!cdr.equals(LispSymbol.ourNil)) {
                    historyPosition = ((LispInteger) cdr).getData();
                }
            } else {
                throw new WrongTypeArgumentException("symbolp or (symbol . integer)", historyObject);
            }
        }
        return LispList.cons(historySymbol, new LispInteger(historyPosition));
    }

    private static LispList historyValue() {
        return (LispList) ourMinibufferHistory.getValue();
    }

    private static void addToHistory (@Nullable LispObject read) {
        if (read == null)
            return;
        LispString value = read instanceof LispString ? (LispString) read : new LispString(read.toString());
        if (value.isEmpty())
            return;
        historyValue().addToBeginning(value);
    }

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

        LispList h = history(history);
        LispSymbol historySymbol = (LispSymbol) h.car();
        LispInteger historyPosition = (LispInteger) h.cdr();

        LispString read = readFromMinibuffer(environment, keymapName, alreadyTyped, prompt.getData(),
                historySymbol, historyPosition.getData(), defaultValue);
        LispObject result = read;
        if (read.isEmpty() && !Predicate.isNil(defaultValue))
            result = defaultValue instanceof LispList ? ((LispList) defaultValue).car() : defaultValue;
        addToHistory(result);
        return result;
    }

    private static LispObject thisOrCar (LispObject maybeList) {
        return maybeList instanceof LispList ? ((LispList) maybeList).car() : maybeList;
    }

    @Subroutine("read-from-minibuffer")
    public static LispObject readFromMinibuffer (Environment environment, LispString prompt,
                                                 @Optional LispObject initialInput, LispKeymap keymap, LispObject returnLispObject,
                                                 LispObject history, LispObject defaults, LispObject inheritInput) {
        if (!Predicate.isNil(initialInput) && !(initialInput instanceof LispString))
            throw new WrongTypeArgumentException("stringp", initialInput);
        String init = Predicate.isNil(initialInput) ? "" : ((LispString)initialInput).getData();

        String keymapName = keymap == null ? "minibuffer-local-map" : keymap.getName();

        LispList h = history(history);
        LispSymbol historySymbol = (LispSymbol) h.car();
        LispInteger historyPosition = (LispInteger) h.cdr();

        LispString read = readFromMinibuffer(environment, keymapName, init, prompt.getData(), historySymbol,
                historyPosition.getData(), defaults);
        if (read == null) {
            //todo
            GlobalEnvironment.showErrorMessage("Param was not read");
            return null;
        }

        LispObject result = Predicate.isNil(returnLispObject)
                ? (read.isEmpty() && !Predicate.isNil(defaults)
                ? thisOrCar(defaults)
                : read)
                : BString.readFromString(read, null, null).car();

        if (result == null) {
            //todo
            GlobalEnvironment.echoMessage(JelispBundle.message("file.ended.while.parsing"));
        }

        addToHistory(result);
        return result;
    }

    private static LispString readFromMinibuffer (final Environment environment, String keymapName, String initialInput, String prompt,
                                                  LispSymbol history, int historyPosition, LispObject defaults) {
        final LispKeymap oldKeymap = environment.getActiveKeymap();
        environment.setActiveKeymap(keymapName);

        //todo: read anything=)

        environment.setActiveKeymap(oldKeymap);
        return new LispString("");
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

    @Subroutine("read-buffer")
    public static LispObject readBuffer (Environment environment, LispObject prompt,
                                         @Optional LispObject def, LispObject requireMatch) {
        LispObject readBufferFunction = environment.find("read-buffer-function").getValue();
        if (!Predicate.isNil(readBufferFunction)) {
            return Core.functionCall(environment, readBufferFunction, prompt, def, requireMatch);
        }

        if (Predicate.isNil(def))
            def = new LispString("");
        else if (def instanceof LispList && !Predicate.isNil(((LispList) def).car()))
            def = ((LispList) def).car();
        if (def instanceof LispBuffer)
            def = new LispString(((LispBuffer) def).getName());

        String promptString = "";
        if (!def.equals(new LispString(""))) {
            promptString = prompt.toString() + (def instanceof LispString ? ((LispString) def).getData() : def.toString());
        } else if (!Predicate.isNil(prompt))
            promptString = prompt.toString();

        return completingRead (environment, new LispString(promptString), environment.find("internal-complete-buffer"),
                LispSymbol.ourNil, requireMatch, LispSymbol.ourNil, environment.find("buffer-name-history").getValue(),
                def, LispSymbol.ourNil);
    }

    private static List<LispObject> getCollectionFromArgument (LispObject collection) {
        List<LispObject> collectionValues = new ArrayList<>();
        if (collection instanceof LispList) {
            for (LispObject item: ((LispList) collection).toLispObjectList()) {
                if (item instanceof LispList
                        && (((LispList) item).car() instanceof LispString
                        || ((LispList) item).car() instanceof LispSymbol))
                    collectionValues.add(item);
                else if (item instanceof LispSymbol)
                    collectionValues.add(new LispString(((LispSymbol) item).getName()));
                else if (item instanceof LispString)
                    collectionValues.add(item);
            }
        }
        if (collection instanceof LispVector) { // aka obarray
            for (LispObject item: ((LispVector) collection).toLispObjectList()) {
                if (!(item instanceof LispSymbol))
                    Core.error(JelispBundle.message("wrong.obarray"));
                collectionValues.add(new LispString(((LispSymbol) item).getName()));
            }
        }
        //todo: collection may be a hash-table. Then return List<Cons> -- Cons = (key . value)
        // keys that fit are LispStrings only
        return collectionValues;
    }

    private static LispString getKey (LispObject possibleCompletion) {
        if (possibleCompletion instanceof LispString)
            return (LispString) possibleCompletion;
        if (possibleCompletion instanceof LispList) {
            return ((LispList) possibleCompletion).car() instanceof LispString
                    ? (LispString) ((LispList) possibleCompletion).car()
                    : new LispString (((LispSymbol) (((LispList) possibleCompletion).car())).getName());
        }
        throw new InternalException();
    }

    private static boolean fitsPredicate (Environment environment, LispObject predicate, LispObject value) {
        return Predicate.isNil(predicate) || !Core.functionCall(environment, predicate, value).equals(LispSymbol.ourNil);
    }

    private static boolean fitsRegexps (Environment environment, LispString value) {
        LispObject regexps = environment.find("completion-regexp-list").getValue();
        boolean isCaseFold = !environment.find("completion-ignore-case").getValue().equals(LispSymbol.ourNil);
        if (regexps.equals(LispSymbol.ourNil))
            return true;
        if (!(regexps instanceof LispList))
            throw new WrongTypeArgumentException("listp", "completion-regexp-list");
        for (LispObject regexp: ((LispList)regexps).toLispObjectList()) {
            if (!(regexp instanceof LispString))
                throw new WrongTypeArgumentException("stringp",regexp);
            if (value.match((LispString) regexp, 0, isCaseFold) == -1)
                return false;
        }
        return true;
    }

    private static boolean fitsNoSpace (String prefix, String completion, @Nullable LispObject noSpace) {
        return Predicate.isNil(noSpace) || prefix.startsWith(" ") || !completion.startsWith(" ");
    }

    private static boolean startsWith (String string, String prefix, boolean isCaseFold) {
        return isCaseFold
                ? string.toLowerCase().startsWith(prefix.toLowerCase())
                : string.startsWith(prefix);
    }

    private static String commonPrefix (Environment environment, String one, String two) {
        boolean isCaseFold = !environment.find("completion-ignore-case").getValue().equals(LispSymbol.ourNil);
        if (!isCaseFold)
            return StringUtil.commonPrefix(one, two);
        String cp = StringUtil.commonPrefix(one.toLowerCase(), two.toLowerCase());
        return one.substring(0, cp.length());
    }

    private static List<String> possibleCompletions (Environment environment, String prefix, LispObject collection,
                                                     LispObject predicate, @Nullable LispObject noSpace) {
        List<LispObject> collectionValues = getCollectionFromArgument(collection);
        List<String> possibleCompletions = new ArrayList<>();
        boolean isCaseFold = !environment.find("completion-ignore-case").getValue().equals(LispSymbol.ourNil);
        for (LispObject value: collectionValues) {
            LispString key = getKey(value);
            if (startsWith(key.getData(), prefix, isCaseFold)
                    && fitsPredicate(environment, predicate, value)
                    && fitsRegexps(environment, key)
                    && fitsNoSpace(prefix, key.getData(), noSpace))
                possibleCompletions.add(key.getData());
        }
        return possibleCompletions;
    }

    @Subroutine("try-completion")
    public static LispObject tryCompletion (Environment environment, LispString toComplete,
                                            LispObject collection, @Optional LispObject predicate) {
        if (collection instanceof LispSymbol
                || (collection instanceof LispList && ((LispList) collection).car() instanceof LispSymbol)) {
            if (predicate == null)
                predicate = LispSymbol.ourNil;
            return Core.functionCall(environment, collection, toComplete, predicate, LispSymbol.ourNil);
        }
        List<String> possibleCompletions = possibleCompletions(environment, toComplete.getData(),
                collection, predicate, null);
        if (possibleCompletions.isEmpty())
            return LispSymbol.ourNil;
        for (String match: possibleCompletions) {
            if (match.equals(toComplete.getData()))
                return LispSymbol.ourT;
        }
        if (possibleCompletions.size() == 1)
            return new LispString(possibleCompletions.get(0));
        Collections.sort(possibleCompletions);
        return new LispString(commonPrefix(environment, possibleCompletions.get(0),
                possibleCompletions.get(possibleCompletions.size() - 1)));
    }

    @Subroutine("all-completions")
    public static LispObject allCompletions (Environment environment, LispString toComplete, LispObject collection,
                                             @Optional LispObject predicate, @Optional @Nullable LispObject noSpace) {
        if (collection instanceof LispSymbol
                || (collection instanceof LispList && ((LispList) collection).car() instanceof LispSymbol)) {
            if (predicate == null)
                predicate = LispSymbol.ourNil;
            return Core.functionCall(environment, collection, toComplete, predicate, LispSymbol.ourT);
        }
        List<String> possibleCompletions = possibleCompletions(environment, toComplete.getData(),
                collection, predicate, noSpace);
        List<LispObject> list = new ArrayList<>();
        for (String completion: possibleCompletions) {
            list.add(new LispString(completion));
        }
        return LispList.list(list);
    }

    @Subroutine("test-completion")
    public static LispObject testCompletion (Environment environment, LispString toComplete, LispObject collection,
                                             @Optional LispObject predicate) {
        if (collection instanceof LispSymbol
                || (collection instanceof LispList && ((LispList) collection).car() instanceof LispSymbol)) {
            if (predicate == null)
                predicate = LispSymbol.ourNil;
            return Core.functionCall(environment, collection, toComplete, predicate, new LispSymbol("lambda"));
        }
        List<String> possibleCompletions = possibleCompletions(environment, toComplete.getData(),
                collection, predicate, null);
        return LispSymbol.bool(!possibleCompletions.isEmpty());
    }

    @Subroutine("internal-complete-buffer")
    public static LispObject internalCompleteBuffer (Environment environment, LispString toComplete,
                                                     LispObject predicate, LispObject flag) {
        List<LispObject> bufferNames = new ArrayList<>();
        for (LispBuffer buffer: environment.getBuffers()) {
            bufferNames.add(new LispString(buffer.getName()));
        }
        LispList collection = LispList.list(bufferNames);

        if (flag.equals(LispSymbol.ourNil))
            return tryCompletion(environment, toComplete, collection, predicate);
        if (flag.equals(LispSymbol.ourT)) {
            return allCompletions(environment, toComplete, collection, predicate, null);
        }
        return testCompletion(environment, toComplete, collection, predicate);
    }
}
