package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.ArgumentOutOfRange;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 5/21/12
 * Time: 11:58 AM
 * To change this template use File | Settings | File Templates.
 */
public abstract class CharTable {
    private CharTable() {}

    @Subroutine("make-char-table")
    public static GenericCharTable makeCharTable (LispSymbol purpose, @Optional LispObject init) {
        return new GenericCharTable(purpose, init);
    }

    @Subroutine("char-table-p")
    public static LispSymbol charTableP(LispObject object) {
        return LispSymbol.bool(object instanceof LispCharTable);
    }

    @Subroutine("char-table-subtype")
    public static LispSymbol charTableSubtype(GenericCharTable charTable) {
        return charTable.getSubtype();
    }

    @Subroutine("char-table-parent")
    public static LispObject charTableParent (GenericCharTable table) {
        return table.getParent();
    }

    @Subroutine("set-char-table-parent")
    public static LispObject setCharTableParent (GenericCharTable table, LispObject newParent) {
        if (newParent instanceof GenericCharTable || newParent.equals(LispSymbol.ourNil)) {
            table.setParent(newParent);
            return newParent;
        }
        throw new WrongTypeArgumentException("char-table-p", newParent.toString());
    }

    @Subroutine("char-table-extra-slot")
    public static LispObject charTableExtraSlot (GenericCharTable table, LispInteger n) {
        if (n.getData() < 0 || n.getData() > 9)
            throw new ArgumentOutOfRange(table, n);
        return table.getExtraSlot(n.getData());
    }

    @Subroutine("set-char-table-extra-slot")
    public static LispObject setCharTableExtraSlot (GenericCharTable table, LispInteger n, LispObject value) {
        if (n.getData() < 0 || n.getData() > 9)
            throw new ArgumentOutOfRange(table, n);
        table.setExtraSlot(n.getData(), value);
        return value;
    }

    private static LispInteger checkChar(LispObject object) {
        if (!Predicate.isCharacter(object))
            throw new WrongTypeArgumentException("characterp", object.toString());
        return (LispInteger)object;
    }

    @Subroutine("char-table-range")
    public static LispObject charTableRange (GenericCharTable table, LispObject range) {
        if (range.equals(LispSymbol.ourNil)) {
            return table.getDefault();
        } else if (range instanceof LispInteger) { //i.e. char
            return table.ref(((LispInteger) range).getData());
        } else if (range instanceof LispList) {
            LispInteger from = checkChar(((LispList) range).car());
            LispInteger to = checkChar(((LispList) range).cdr());
            return table.refAndRange(from.getData(), from.getData(), to.getData());
        } else {
            Core.error("Invalid RANGE argument to `char-table-range'");
        }
        return null;
    }

    @Subroutine("set-char-table-range")
    public static LispObject setCharTableRange (GenericCharTable table, LispObject range, LispObject value) {
        if (range.equals(LispSymbol.ourNil)) {
            table.setDefault(value);
        } else if (range.equals(LispSymbol.ourT)) {
            table.setContent(value);
        } else if (range instanceof LispInteger) { //i.e. char
            table.setRange(((LispInteger) range).getData(), ((LispInteger) range).getData(), value);
        } else if (range instanceof LispList) {
            LispInteger from = checkChar(((LispList) range).car());
            LispInteger to = checkChar(((LispList) range).cdr());
            table.setRange(from.getData(), to.getData(), value);
        } else {
            Core.error("Invalid RANGE argument to `set-char-table-range'");
        }
        return value;
    }

    @Subroutine("set-char-table-default")
    public static LispObject setCharTableDefault (LispObject table, LispObject ch, LispObject value) {
        return LispSymbol.ourNil;
    }

    @Subroutine("map-char-table")
    public static LispSymbol mapCharTable (LispObject function, LispCharTable table) {
        return LispSymbol.ourNil;
    }

}
