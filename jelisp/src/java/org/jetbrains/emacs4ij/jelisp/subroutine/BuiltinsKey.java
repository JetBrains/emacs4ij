package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.*;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
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
    private BuiltinsKey() {}

    public static LispKeymap ourGlobalMap;
    public static LispSymbol ourKeyMapSymbol = new LispSymbol("keymap");
    public static LispList ourExcludeKeys;

    private static boolean isListKeymap (LispObject object) {
        return (object instanceof LispList && ((LispList) object).car().equals(ourKeyMapSymbol));
    }

    public static boolean isKeymap (LispObject object) {
        return (object instanceof LispSymbol && isListKeymap(((LispSymbol) object).getFunction()))
                || isListKeymap(object);
    }

    public static LispKeymap getKeymap(LispObject object) {
        if (object instanceof LispKeymap)
            return (LispKeymap) object;
        if (isListKeymap(object)) {
            return (LispList) object;
        }
        if (object instanceof LispSymbol && isListKeymap(((LispSymbol) object).getFunction())) {
            return(LispList) ((LispSymbol) object).getFunction();
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
    public static LispKeymap makeSparseKeymap (Environment environment, @Nullable @Optional LispObject prompt) {
        return environment.makeSparseKeymap(prompt);
    }

    @Subroutine("make-keymap")
    public static LispKeymap makeKeymap (Environment environment, @Nullable @Optional LispObject prompt) {
        return environment.makeKeymap(prompt);
    }

    @Subroutine("copy-keymap")
    public static LispList copyKeymap (LispObject object) {
        check(object);
        throw new NotImplementedException("copy-keymap");
    }

    @Subroutine("keymap-parent")
    public static LispObject keymapParent (LispObject object) {
        LispKeymap element = getKeymap(object);
        if (element == null)
            throw new WrongTypeArgumentException("keymapp", object);
        return BuiltinsCore.thisOrNil(element.getParent());
    }

    @Subroutine("set-keymap-parent")
    public static LispObject setKeymapParent (LispObject object, LispObject parent) {
        LispKeymap element = getKeymap(object);
        if (element == null)
            throw new WrongTypeArgumentException("keymapp", object);
        LispKeymap trueParent = getKeymap(parent);
        if (trueParent == null && !parent.equals(LispSymbol.ourNil))
            throw new WrongTypeArgumentException("keymapp", parent);
        element.setParent(trueParent);
        return trueParent;
    }

    @Subroutine("define-prefix-command")
    public static LispSymbol definePrefixCommand (Environment environment, LispSymbol command, @Optional LispSymbol mapVar, @Optional LispObject name) {
        LispKeymap keymap = makeSparseKeymap(environment, name);
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
    public static LispObject keyBinding (LispStringOrVector key, @Optional LispObject acceptDefault, LispObject noReMap, LispObject position) {
        throw new NotImplementedException("key-binding");
    }

    @Subroutine("lookup-key")
    public static LispObject lookupKey (LispObject keymapObject, LispStringOrVector key, @Nullable @Optional LispObject acceptDefault) {
        LispKeymap keymap = getKeymap(keymapObject);
        if (key.length() == 0)
            return keymapObject;
        return keymap.getKeyDefinition(key, acceptDefault);
    }

    @Subroutine("define-key")
    public static LispObject defineKey(Environment environment, LispObject keymapObject, LispStringOrVector key, LispSymbol function) {
        LispKeymap keymap = getKeymap(keymapObject);
        if (keymap == null)
            throw new WrongTypeArgumentException("keymapp", keymapObject);
        keymap.defineKey(environment, function, key);
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

    @Subroutine("current-global-map")
    public static LispKeymap currentGlobalMap (Environment environment) {
        return environment.getActiveKeymap();
    }

    @Subroutine("use-global-map")
    public static LispSymbol useGlobalMap (Environment environment, LispKeymap keymap) {
        environment.setActiveKeymap(keymap);
        return LispSymbol.ourNil;
    }

    public static void defineKeyMaps (GlobalEnvironment g) {
        ourKeyMapSymbol.setProperty("char-table-extra-slots", new LispInteger(0));
        g.defineSymbol(ourKeyMapSymbol);
        ourGlobalMap = BuiltinsKey.makeKeymap(g, LispSymbol.ourNil);
        BuiltinsCore.set(g, new LispSymbol("global-map"), ourGlobalMap);

        LispObject meta_map = BuiltinsKey.makeKeymap(g, LispSymbol.ourNil);
        BuiltinsCore.set(g, new LispSymbol("esc-map"), meta_map);
        BuiltinsCore.functionSet(g, new LispSymbol("ESC-prefix"), meta_map);

        LispObject control_x_map = BuiltinsKey.makeKeymap(g, LispSymbol.ourNil);
        BuiltinsCore.set(g, new LispSymbol("ctl-x-map"), control_x_map);
        BuiltinsCore.functionSet(g, new LispSymbol("Control-X-prefix"), control_x_map);
        
        ourExcludeKeys = LispList.list (LispList.cons(new LispString("DEL"), new LispString("\\d")),
                LispList.cons(new LispString("TAB"), new LispString("\\t")),
                LispList.cons(new LispString("RET"), new LispString("\\r")),
                LispList.cons(new LispString("ESC"), new LispString("\\e")),
                LispList.cons(new LispString("SPC"), new LispString(" ")));

        g.defineSymbol("define-key-rebound-commands", LispSymbol.ourT);
        LispSymbol mblMap   = makeKeyMap(g, "minibuffer-local-map");
        LispSymbol mblNsMap = makeKeyMap(g, "minibuffer-local-ns-map", mblMap);
        LispSymbol mblCompletionMap = makeKeyMap(g, "minibuffer-local-completion-map", mblMap);
        LispSymbol mblFileNameCompletionMap = makeKeyMap(g, "minibuffer-local-filename-completion-map", mblCompletionMap);
        LispSymbol mblMustMatchMap = makeKeyMap(g, "minibuffer-local-must-match-map", mblCompletionMap);
        LispSymbol mblFileNameMustMatchMap = makeKeyMap(g, "minibuffer-local-filename-must-match-map", mblMustMatchMap);

        g.defineSymbol("minor-mode-map-alist");
        g.defineSymbol("minor-mode-overriding-map-alist");
        g.defineSymbol("emulation-mode-map-alists");
        g.defineSymbol("where-is-preferred-modifier");

        LispList mouse_events = LispList.list(new LispString("menu-bar"), new LispString("menu-bar"), new LispString("tool-bar"),
                new LispString("header-line"), new LispString("mode-line"),new LispString("mouse-1"), new LispString("mouse-2"),
                new LispString("mouse-3"), new LispString("mouse-4"), new LispString("mouse-5"));
    }

    public static void keys_of_keymap () {
//        initial_define_key (globalMap, 27, "ESC-prefix");
//        initial_define_key (globalMap, CharUtil.Ctl('X'), "Control-X-prefix");
    }

    private static LispSymbol makeKeyMap(GlobalEnvironment g, String name, LispSymbol parentSymbol) {
        LispKeymap keymap = makeSparseKeymap(g, null);
        try {
            LispObject value = parentSymbol.getValue();
            LispKeymap parent = value.equals(LispSymbol.ourNil) ? null : getKeymap(value);
            //todo: this hack is only for test!
            if (keymap != null)
                keymap.setParent(parent);

            return g.defineSymbol(name, keymap);
        } catch (ClassCastException e) {
            throw new InternalError("makeKeyMap: parent symbol value is not instance of LispKeymap");
        }
    }

    private static LispSymbol makeKeyMap(GlobalEnvironment g, String name) {
        return g.defineSymbol(name, makeSparseKeymap(g, LispSymbol.ourNil));
    }

    @Subroutine("event-convert-list")
    public static LispObject eventConvertList (LispObject event_desc) {
        LispObject base = LispSymbol.ourNil;
        LispObject rest = event_desc;
        int modifiers = 0;
        while (rest instanceof LispList) {
            LispObject item = ((LispList) rest).car();
            rest = ((LispList) rest).cdr();
            int index = 0;
            /* Given a symbol, see if it is a modifier name.  */
            if (item instanceof LispSymbol && rest instanceof LispList)
                index = KeyBoardModifier.parseSolitary((LispSymbol) item);
            if (index != 0)
                modifiers |= index;
            else if (!base.equals(LispSymbol.ourNil))
                BuiltinsCore.error("Two bases given in one event");
            else
                base = item;
        }
        /* Let the symbol A refer to the character A.  */
        if (base instanceof LispSymbol && ((LispSymbol) base).getName().length() == 1)
            base = new LispInteger(((LispSymbol) base).getName().charAt(0));

        if (base instanceof LispInteger) {
            /* Turn (shift a) into A.  */
            if (KeyBoardModifier.SHIFT.bitwiseAndNotZero(modifiers)
                    && (((LispInteger) base).getData() >= 'a' && ((LispInteger) base).getData() <= 'z'))
            {
                ((LispInteger) base).setData(((LispInteger) base).getData() - ('a' - 'A'));
                modifiers = KeyBoardModifier.SHIFT.bitwiseAndNot(modifiers);
            }

            /* Turn (control a) into C-a.  */
            if (KeyBoardModifier.CTRL.bitwiseAndNotZero(modifiers))
                return new LispInteger(KeyBoardModifier.CTRL.bitwiseAndNot(modifiers)
                        | CharUtil.makeCtrlChar(((LispInteger) base).getData()));
            else
                return new LispInteger(modifiers | ((LispInteger) base).getData());
        }
        else if (base instanceof LispSymbol)
            return KeyBoardUtil.applyModifiers(modifiers, base);
        else {
            BuiltinsCore.error("Invalid base event");
            return LispSymbol.ourNil;
        }
    }

    @Subroutine("single-key-description")
    public static LispObject singleKeyDescription (LispObject key, @Optional LispObject no_angles) {
//        if (key instanceof LispList && lucid_event_type_list_p (key))
//            key = eventConvertList(key);
        key = BuiltinPredicates.eventHead(key);
        if (key instanceof LispInteger) {		/* Normal character */
            String p = CharUtil.pushKeyDescription (((LispInteger) key).getData(), true);
            return new LispString(p); //make_specified_string (tem, -1, p - tem, 1);
        }
        else if (key instanceof LispSymbol)	{ /* Function key or event-symbol */
            if (no_angles == null || no_angles.equals(LispSymbol.ourNil))
                return new LispString('<' + ((LispSymbol) key).getName() + '>');
            else
                return new LispString(((LispSymbol) key).getName());
        }
        else if (key instanceof LispString)	/* Buffer names in the menubar.  */
            return BuiltinsSequence.copySequence(key);
        else
            BuiltinsCore.error("KEY must be an integer, cons, symbol, or string");
        return LispSymbol.ourNil;
    }


//    public static void initialDefineKey (LispKeymap keymap, int key, String name) {
//        keymap.store(GlobalEnvironment.INSTANCE, new LispInteger(key), new LispString(name));
//    }
//
//    public static LispObject storeInKeymap(Environment environment, LispObject keymap, LispObject idx, LispObject def) {
//        if (!isListKeymap(keymap))
//            BuiltinsCore.error("attempt to define a key in a non-keymap");
//        LispKeymap trueKeymap = getKeymap(keymap);
//        trueKeymap.store(environment, idx, def);
//        return def;
//    }

    public static int parseModifiersUncached(LispObject symbol, LispInteger lispModifierEnd) {
        if (!(symbol instanceof LispSymbol))
            throw new WrongTypeArgumentException("symbolp", symbol);
        LispString name = new LispString(((LispSymbol) symbol).getName());

        int modifiers = 0;
        int i;

        for (i = 0; i + 2 <= name.lengthInBytes(); ) {
            int thisModEnd = 0;
            int thisMod = 0;
            Pair pair = KeyBoardModifier.check(name.getData().substring(i));
            if (pair != null) {
                thisModEnd = i + pair.getLength();
                thisMod = pair.getModifier();
            }
            if (thisModEnd == 0)
                break;
            if (thisModEnd >= name.lengthInBytes() || name.charAt(thisModEnd) != '-')
                break;

            modifiers |= thisMod;
            i = thisModEnd + 1;
        }

        if (0 == (modifiers & KeyBoardModifier.bitwiseOr(KeyBoardModifier.DOWN, KeyBoardModifier.DRAG,
                KeyBoardModifier.DOUBLE, KeyBoardModifier.TRIPLE))
                && i + 7 == name.lengthInBytes()
                && name.getData().substring(i).startsWith("mouse-")
                && ('0' <= name.charAt(i + 6) && name.charAt(i + 6) <= '9'))
            modifiers = KeyBoardModifier.CLICK.bitwiseOr(modifiers);

        if (0 == (modifiers & KeyBoardModifier.bitwiseOr(KeyBoardModifier.DOUBLE, KeyBoardModifier.TRIPLE))
                && i + 6 < name.lengthInBytes()
                && name.getData().substring(i).startsWith("wheel-"))
            modifiers = KeyBoardModifier.CLICK.bitwiseOr(modifiers);

        lispModifierEnd.setData(i);
        return modifiers;
    }

    public static boolean lucidEventTypeListP(LispObject object) {
        for (LispObject tail = object; tail instanceof LispList; tail = ((LispList) tail).cdr()) {
            LispObject item = ((LispList) tail).car();
            if (!(item instanceof LispInteger || item instanceof LispSymbol))
                return false;
        }
        return true;
    }
}
