package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.KeyBoardModifier;
import org.jetbrains.emacs4ij.jelisp.KeyBoardUtil;
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

    public static LispKeymap globalMap;
    private static LispSymbol myKeyMapSymbol = new LispSymbol("keymap");

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
    public static LispKeymap makeSparseKeymap (Environment environment, @Nullable @Optional LispObject prompt) {
        return makeKeymap(environment, prompt);
    }

    @Subroutine("make-keymap")
    public static LispKeymap makeKeymap (Environment environment, @Nullable @Optional LispObject prompt) {
        return environment.createKeymap(prompt == null || prompt.equals(LispSymbol.ourNil) ? null : prompt.toString());
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

    @Subroutine("define-key")
    public static LispObject defineKey(Environment environment, LispObject keymapObject, LispStringOrVector key, LispSymbol function) {
        check(keymapObject);
        LispKeymap keymap = getKeymap(keymapObject);
        keymap.defineKey(function, key);
        return function;
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
    public static LispKeymap currentGlobalMap (Environment environment) {
        return environment.getActiveKeymap();
    }

    @Subroutine("use-global-map")
    public static LispSymbol useGlobalMap (Environment environment, LispKeymap keymap) {
        environment.setActiveKeymap(keymap);
        //todo check that keymap manger now holds 'keymap' as active
        return LispSymbol.ourNil;
    }

    public static void defineKeyMaps (GlobalEnvironment g) {
        myKeyMapSymbol.setProperty("char-table-extra-slots", new LispInteger(0));
        g.defineSymbol(myKeyMapSymbol);
        globalMap = BuiltinsKey.makeKeymap(g, LispSymbol.ourNil);
        BuiltinsCore.set(g, new LispSymbol("global-map"), globalMap);

        LispObject meta_map = BuiltinsKey.makeKeymap(g, LispSymbol.ourNil);
        BuiltinsCore.set(g, new LispSymbol("esc-map"), meta_map);
        BuiltinsCore.functionSet(g, new LispSymbol("ESC-prefix"), meta_map);

        LispObject control_x_map = BuiltinsKey.makeKeymap(g, LispSymbol.ourNil);
        BuiltinsCore.set(g, new LispSymbol("ctl-x-map"), control_x_map);
        BuiltinsCore.functionSet(g, new LispSymbol("Control-X-prefix"), control_x_map);

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
        LispKeymap keymap = BuiltinsKey.makeSparseKeymap(g, null);
        try {
            LispObject value = parentSymbol.getValue();
            LispKeymap parent = value.equals(LispSymbol.ourNil) ? null : (LispKeymap) value;
            //todo: this hack is only for test!
            if (keymap != null)
                keymap.setParent(parent);

            return g.defineSymbol(name, keymap);
        } catch (ClassCastException e) {
            throw new InternalError("makeKeyMap: parent symbol value is not instance of LispKeymap");
        }
    }

    private static LispSymbol makeKeyMap(GlobalEnvironment g, String name) {
        return g.defineSymbol(name, BuiltinsKey.makeSparseKeymap(g, LispSymbol.ourNil));
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
                BuiltinsCore.error(GlobalEnvironment.INSTANCE, "Two bases given in one event");
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
            BuiltinsCore.error(GlobalEnvironment.INSTANCE, "Invalid base event");
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
            BuiltinsCore.error (GlobalEnvironment.INSTANCE, "KEY must be an integer, cons, symbol, or string");
        return LispSymbol.ourNil;
    }




}
