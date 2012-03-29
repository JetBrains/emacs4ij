package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.KeymapCell;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.InternalException;
import org.jetbrains.emacs4ij.jelisp.exception.NotImplementedException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/15/12
 * Time: 3:31 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class BuiltinsKey {
    private static LispSymbol ourKeyMapSymbol = new LispSymbol("keymap");

    private BuiltinsKey() {}

    private static boolean isKeymapItself(LispObject object) {
        return object instanceof LispKeymap;
    }

    private static boolean isKeymap (LispObject object) {
        return (object instanceof LispSymbol && isKeymapItself(((LispSymbol) object).getFunction()))
                || isKeymapItself(object);
    }

    private static LispKeymap getKeymap(LispObject object) {
        if (isKeymapItself(object)) {
            return (LispKeymap) object;
        }
        if (object instanceof LispSymbol && isKeymapItself(((LispSymbol) object).getFunction())) {
            return (LispKeymap) ((LispSymbol) object).getFunction();
        }
        return null;
    }

    private static void check (LispObject object) {
        if (!isKeymap(object))
            throw new WrongTypeArgumentException("keymapp", object);
    }

    @Subroutine("keymapp")
    public static LispSymbol keymapP (LispObject object) {
        return LispSymbol.bool(isKeymap(object));
    }

    @Subroutine("make-sparse-keymap")
    public static LispKeymap makeSparseKeymap (@Nullable @Optional LispObject prompt) {
        return makeKeymap(prompt);
    }

    private static LispKeymap makeSparseKeymap (String prompt) {
        return makeKeymap(new LispString(prompt));
    }

    @Subroutine("make-keymap")
    public static LispKeymap makeKeymap (@Nullable @Optional LispObject prompt) {
        return GlobalEnvironment.INSTANCE.createKeymap(prompt);
    }

    @Subroutine("copy-keymap")
    public static LispKeymap copyKeymap (LispObject object) {
        check(object);
        return getKeymap(object).copy();
    }

    @Subroutine("keymap-parent")
    public static LispObject keymapParent (LispObject object) {
        LispKeymap element = getKeymap(object);
        if (element == null)
            throw new WrongTypeArgumentException("keymapp", object);
        return element.getParent() == null ? LispSymbol.ourNil : element.getParent();
    }

    @Subroutine("set-keymap-parent")
    public static LispObject setKeymapParent (LispObject object, LispObject parent) throws WrongTypeArgumentException {
        LispKeymap element = getKeymap(object);
        if (element == null)
            throw new WrongTypeArgumentException("keymapp", object);
        LispKeymap trueParent = getKeymap(parent);
        if (trueParent == null && !parent.equals(LispSymbol.ourNil))
            throw new WrongTypeArgumentException("keymapp", parent);
        element.setParent(trueParent);
        return parent;
    }

    @Subroutine("define-prefix-command")
    public static LispSymbol definePrefixCommand (LispSymbol command, @Optional LispSymbol mapVar, @Optional LispObject name) {
        LispKeymap keymap = makeSparseKeymap(name);
        command.setFunction(keymap);
        if (mapVar != null && !mapVar.equals(LispSymbol.ourNil))
            mapVar.setValue(keymap);
        else command.setValue(keymap);
        return command;
    }

    @Subroutine("current-active-maps")
    public static LispObject currentActiveMaps (@Optional LispObject olp, LispObject position) {
        throw new NotImplementedException("current-active-maps");
    }

    @Subroutine("key-binding")
    public static LispObject keyBinding (Environment environment, LispStringOrVector key, @Optional LispObject acceptDefault, LispObject noReMap, LispObject position) {
        //note: optional parameters are ignored ignored
        return environment.getActiveKeymap().getKeyBinding(key);
    }
    
    @Subroutine("lookup-key")
    public static LispObject lookupKey (LispKeymap keymap, LispStringOrVector key, @Optional LispObject acceptDefault) {
        //note: acceptDefault is ignored, implemented for compatibility
        return keymap.getKeyBinding(key);
    }

    @Subroutine("define-key")
    public static LispObject defineKey(LispObject keymapObject, LispStringOrVector key, KeymapCell function) {
        check(keymapObject);
        LispKeymap keymap = getKeymap(keymapObject);
        keymap.defineKey(function, key);
        return function;
    }

    @Subroutine("key-description")
    public static LispString keyDescription (Environment environment, LispObject keys, @Optional LispObject prefix) {
        String first = BuiltinPredicates.isNil(prefix)
                ? ""
                : BuiltinsSequence.mapConcat(environment, new LispSymbol("single-key-description"), prefix, new LispString(" ")).getData();
        String second = BuiltinsSequence.mapConcat(environment, new LispSymbol("single-key-description"), keys, new LispString(" ")).getData();
        return new LispString(first + " " + second);
    }

    @Subroutine("single-key-description")
    public static LispString singleKeyDescription (LispObject key, @Optional LispObject noAngles) {
        throw new NotImplementedException("single-key-description");
    }
    
    @Subroutine("current-global-map")
    public static LispKeymap currentGlobalMap (Environment environment) {
        return environment.getActiveKeymap();
    }

    @Subroutine("use-global-map")
    public static LispSymbol useGlobalMap (Environment environment, LispKeymap keymap) {
        environment.setActiveKeymap(keymap);
        return LispSymbol.ourNil;
    }

    public static void init() {
        ourKeyMapSymbol.setProperty("char-table-extra-slots", new LispInteger(0));
        GlobalEnvironment.INSTANCE.defineSymbol(ourKeyMapSymbol);
        GlobalEnvironment.INSTANCE.defineSymbol("deactivate-mark");
        LispSymbol globalMap = makeKeymap("global-map");
        if (globalMap.getValue().equals(LispSymbol.ourNil))
            return;
        ((LispKeymap)globalMap.getValue()).defineKey(makeSparseKeymap("ctl-x-map"), new LispString("\\C-x"));
        ((LispKeymap)globalMap.getValue()).defineKey(makeSparseKeymap("esc-map"), new LispString("ESC"));
    }

    private static void setKey (LispSymbol keymap, String name, String key) {
        LispSymbol action = GlobalEnvironment.INSTANCE.find(name);
        if (action == null)
            throw new InternalException("BuiltinsKey.globalSetKey");
        setKey(keymap, action, key);
    }

    private static void setKey (LispSymbol keymap, LispSymbol action, String key) {
        ((LispKeymap)keymap.getValue()).defineKey(action, new LispString(key));
    }

    private static LispSymbol makeKeymap(String name, LispSymbol parentSymbol) {
        LispKeymap keymap = makeSparseKeymap(name);
        try {
            LispObject value = parentSymbol.getValue();
            LispKeymap parent = value.equals(LispSymbol.ourNil) ? null : (LispKeymap) value;
            //todo: this is only for test!
            if (keymap != null)
                keymap.setParent(parent);

            return GlobalEnvironment.INSTANCE.defineSymbol(name, keymap);
        } catch (ClassCastException e) {
            throw new InternalError("makeKeyMap: parent symbol value is not instance of LispKeymap");
        }
    }

    private static LispSymbol makeKeymap(String name) {
        return GlobalEnvironment.INSTANCE.defineSymbol(name, makeSparseKeymap(name));
    }
}