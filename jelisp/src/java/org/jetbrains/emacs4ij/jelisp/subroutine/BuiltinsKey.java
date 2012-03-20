package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.*;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

import static org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinPredicates.lucidEventTypeListP;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/15/12
 * Time: 3:31 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class BuiltinsKey {
    private BuiltinsKey() {}

    public static LispObject globalMap;
    private static LispObject currentGlobalMap;
    public static LispList ourExcludeKeys;
    public static LispSymbol ourKeyMapSymbol = new LispSymbol("keymap");

    private static boolean isListKeymap (LispObject object) {
        return (object instanceof LispList && ((LispList) object).car().equals(ourKeyMapSymbol));
    }

    public static boolean isKeymap (LispObject object) {
        return (object instanceof LispSymbol && isListKeymap(((LispSymbol) object).getFunction()))
                || isListKeymap(object);
    }

    public static LispKeymap getKeymap(LispObject object) {
        if (isListKeymap(object)) {
            return (LispList) object;
        }
        if (object instanceof LispSymbol && isListKeymap(((LispSymbol) object).getFunction())) {
            return (LispList) ((LispSymbol) object).getFunction();
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
        return GlobalEnvironment.INSTANCE.makeSparseKeymap(prompt);
    }

    @Subroutine("make-keymap")
    public static LispKeymap makeKeymap (@Optional LispObject prompt) {
        return GlobalEnvironment.INSTANCE.makeKeymap(prompt);
    }

    @Subroutine("copy-keymap")
    public static LispKeymap copyKeymap (LispObject object) {
        check(object);
        return getKeymap(object).copy();
    }

    @Subroutine("keymap-parent")
    public static LispObject keymapParent (LispObject object) {
        LispKeymap keymap = getKeymap(object);
        if (keymap == null)
            throw new WrongTypeArgumentException("keymapp", object);
        return BuiltinsCore.thisOrNil(keymap.getParent());
    }

    @Subroutine("set-keymap-parent")
    public static LispObject setKeymapParent (LispObject object, LispObject parent) {
        LispKeymap element = getKeymap(object);
        if (element == null)
            throw new WrongTypeArgumentException("keymapp", object);
        check(parent);
        element.setParent(getKeymap(parent));
        return parent;
    }

    @Subroutine("define-prefix-command")
    public static LispSymbol definePrefixCommand (LispSymbol command, @Optional LispSymbol mapVar, @Optional LispObject name) {
        LispKeymap keymap = makeSparseKeymap(name);
        command.setFunction(keymap);
        if (!BuiltinPredicates.isNil(mapVar))
            mapVar.setValue(keymap);
        else command.setValue(keymap);
        return command;
    }
// =====
    @Subroutine("current-active-maps")
    public static LispObject currentActiveMaps (@Optional LispObject olp, LispObject position) {
        return null;
    }

    @Subroutine("key-binding")
    public static LispObject keyBinding (LispStringOrVector key, @Optional LispObject acceptDefault, LispObject noReMap, LispObject position) {
        if (key instanceof LispString) {
            return null;
        }
        if (key instanceof LispVector) {
            return null;
        }
        throw new WrongTypeArgumentException("arrayp", key);
    }

    @Subroutine("define-key")
    public static LispObject defineKey(Environment environment, LispKeymap keymap, LispStringOrVector key, LispObject function) {
        check(keymap);
        return keymap.defineKey(environment, function, key);
    }

    @Subroutine("key-description")
    public static LispString keyDescription (Environment environment, LispObject keys, @Optional LispObject prefix) {
        String first = (prefix == null || prefix.equals(LispSymbol.ourNil))
                ? ""
                : BuiltinsSequence.mapConcat(environment, new LispSymbol("single-key-description"), prefix, new LispString(" ")).getData();
        String second = BuiltinsSequence.mapConcat(environment, new LispSymbol("single-key-description"), keys, new LispString(" ")).getData();
        return new LispString(first + " " + second);
    }

    @Subroutine("current-global-map")
    public static LispObject currentGlobalMap () {
        return currentGlobalMap;
    }

    public static void defineKeyMaps () {
        ourKeyMapSymbol.setProperty("char-table-extra-slots", new LispInteger(0));
        GlobalEnvironment g = GlobalEnvironment.INSTANCE;
        g.defineSymbol(ourKeyMapSymbol);
        globalMap = BuiltinsKey.makeKeymap(LispSymbol.ourNil);
        BuiltinsCore.set(g, new LispSymbol("global-map"), globalMap);
        currentGlobalMap = globalMap;
        LispObject meta_map = BuiltinsKey.makeKeymap(LispSymbol.ourNil);
        BuiltinsCore.set(g, new LispSymbol("esc-map"), meta_map);
        BuiltinsCore.functionSet(g, new LispSymbol("ESC-prefix"), meta_map);

        LispObject control_x_map = BuiltinsKey.makeKeymap(LispSymbol.ourNil);
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
    }

    public static void keys_of_keymap () {
        initial_define_key (globalMap, 27, "ESC-prefix");
        initial_define_key (globalMap, CharUtil.Ctl('X'), "Control-X-prefix");
    }

    private static LispSymbol makeKeyMap(GlobalEnvironment g, String name, LispSymbol parent) {
        LispSymbol keyMap = makeKeyMap(g, name);
        BuiltinsKey.setKeymapParent(keyMap.getValue(), parent.getValue());
        return keyMap;
    }

    private static LispSymbol makeKeyMap(GlobalEnvironment g, String name) {
        return g.defineSymbol(name, BuiltinsKey.makeSparseKeymap(LispSymbol.ourNil));
    }

    public static void initial_define_key (LispObject keymap, int key, String name) {
//        todo store_in_keymap (keymap, new LispInteger(key), new LispString(name));
    }

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
        if (key instanceof LispList && lucidEventTypeListP(key))
            key = eventConvertList(key);
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
            BuiltinsCore.error ("KEY must be an integer, cons, symbol, or string");
        return LispSymbol.ourNil;
    }
}
