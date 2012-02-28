package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.elisp.*;
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

    //TODO: implement all
    
    private static boolean isListKeymap (LObject object) {
        return (object instanceof LispList && ((LispList) object).car().equals(new LispSymbol("keymap")));
    }

    private static boolean isKeymap (LObject object) {
        return (object instanceof LispSymbol && isListKeymap(((LispSymbol) object).getFunction()))
                || isListKeymap(object);
    }
    
    private static LispList getKeymap(LObject object) {
        if (isListKeymap(object)) {
            return (LispList) object;
        }
        if (object instanceof LispSymbol && isListKeymap(((LispSymbol) object).getFunction())) {
            return (LispList) ((LispSymbol) object).getFunction();
        }
        throw new WrongTypeArgumentException("keymapp", object.toString());
    }
    
    private static void check (LObject object) {
        if (!isKeymap(object))
            throw new WrongTypeArgumentException("keymapp", object.toString());        
    }

    @Subroutine("keymapp")
    public static LispSymbol keymapP (LObject object) {
        return LispSymbol.bool(isKeymap(object));
    }
    
    @Subroutine("make-sparse-keymap")
    public static LispList makeSparseKeymap (@Optional LispString prompt) {
        if (prompt != null) {
            throw new RuntimeException("not implemented");
        }    
        return LispList.list(new LispSymbol("keymap"));
    }

    //todo
    @Subroutine("make-keymap")
    public static LispList makeKeymap (@Optional LispString prompt) {
        if (prompt != null) {
            throw new RuntimeException("not implemented");
        }
        return null;
    }

    //todo
    @Subroutine("copy-keymap")
    public static LispList copyKeymap (LObject object) {
        check(object);
        return null;
    }

    @Subroutine("keymap-parent")
    public static LObject keymapParent (LObject object) {
        LispList element = getKeymap(object);
        return element.tail();
    }

    @Subroutine("set-keymap-parent")
    public static LObject setKeymapParent (LObject object, LObject parent) {
        LispList element = getKeymap(object);
        check(parent);
        //todo: If keymap has submaps (bindings for prefix keys), they too receive new parent keymaps
        // that reflect what parent specifies for those prefix keys.
        element.resetTail(parent);
        return parent;
    }
    
    @Subroutine("define-prefix-command")
    public static LispSymbol definePrefixCommand (LispSymbol command, @Optional LObject mapVar, @Optional LObject name) {
        //todo: =)
        return command;
    }
    
    @Subroutine("current-active-maps")
    public static LObject currentActiveMaps (@Optional LObject olp, LObject position) {
        return null;
    }

    @Subroutine("key-binding")
    public static LObject keyBinding (LObject key, @Optional LObject acceptDefault, LObject noReMap, LObject position) {
        if (key instanceof LispString) {
            return null;    
        }
        if (key instanceof LispVector) {
            return null;
        }
        throw new WrongTypeArgumentException("arrayp", key.toString());
    }

    
}
