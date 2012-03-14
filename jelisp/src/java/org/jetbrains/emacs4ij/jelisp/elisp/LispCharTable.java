package org.jetbrains.emacs4ij.jelisp.elisp;

import com.rits.cloning.Cloner;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.ArgumentOutOfRange;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinPredicates;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinsCore;

import java.util.Arrays;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/28/12
 * Time: 3:40 PM
 * To change this template use File | Settings | File Templates.
 */
public class LispCharTable implements LispObject, LispArray, LispSequence {
    private LispObject myDefault;
    private LispObject myParent;
    private LispSymbol mySubtype;
    private LispObject myAscii = null;
    private LispObject[] myContent = new LispObject[(1 << CharUtil.CHARTABLE_SIZE_BIT[0])];
    private LispObject[] myExtras = new LispObject[CharUtil.MAX_N_EXTRA_SLOTS];
    private int myNExtras = 0;

    @Override
    public LispObject evaluate(Environment environment) {
        return null;
    }
    
    public LispCharTable(LispSymbol purpose, @Nullable LispObject init) {
        LispObject n = purpose.getProperty("char-table-extra-slots");
        if (!n.equals(LispSymbol.ourNil)) {
            if (!BuiltinPredicates.isWholeNumber(n))
                throw new WrongTypeArgumentException("wholenump", n);
            myNExtras = ((LispInteger)n).getData();
            if (myNExtras > CharUtil.MAX_N_EXTRA_SLOTS)
                throw new ArgumentOutOfRange(n, LispSymbol.ourNil);
        }        
        myParent = LispSymbol.ourNil;
        myDefault = init == null ? LispSymbol.ourNil : init;
        mySubtype = purpose;
        Arrays.fill(myContent, myDefault);
        Arrays.fill(myExtras, myDefault);
        myAscii = myDefault;
    }

    public LispSymbol getSubtype() {
        return mySubtype;
    }

    public void setParent(LispObject parent) {
        myParent = parent;
    }

    public LispObject getParent() {
        return myParent;
    }

    public void setDefault(LispObject myDefault) {
        this.myDefault = myDefault;
    }

    public LispObject getDefault() {
        return myDefault;
    }

    @Override
    public String toString() {
        String s = "#^[" + myDefault + ' ' + myParent + ' ' + mySubtype + (myAscii == null ? "" : " " + myAscii);
        for (LispObject item: myContent) {
            s += ' ' + item.toString();
        }
        for (int i = 0; i < myNExtras; ++i) {
            s += ' ' + myExtras[i].toString();
        }
        s += ']';
        return s;        
    }
    
    public LispObject getExtraSlot(int n) {
        return myExtras[n];
    }
    
    public void setExtraSlot (int n, LispObject value) {
        myExtras[n] = value;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof LispCharTable)) return false;

        LispCharTable that = (LispCharTable) o;

        if (myNExtras != that.myNExtras) return false;
        if (myAscii != null ? !myAscii.equals(that.myAscii) : that.myAscii != null) return false;
        if (!Arrays.equals(myContent, that.myContent)) return false;
        if (myDefault != null ? !myDefault.equals(that.myDefault) : that.myDefault != null) return false;
        if (!Arrays.equals(myExtras, that.myExtras)) return false;
        if (myParent != null ? !myParent.equals(that.myParent) : that.myParent != null) return false;
        if (mySubtype != null ? !mySubtype.equals(that.mySubtype) : that.mySubtype != null) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = myDefault != null ? myDefault.hashCode() : 0;
        result = 31 * result + (myParent != null ? myParent.hashCode() : 0);
        result = 31 * result + (mySubtype != null ? mySubtype.hashCode() : 0);
        result = 31 * result + (myAscii != null ? myAscii.hashCode() : 0);
        result = 31 * result + (myContent != null ? Arrays.hashCode(myContent) : 0);
        result = 31 * result + (myExtras != null ? Arrays.hashCode(myExtras) : 0);
        result = 31 * result + myNExtras;
        return result;
    }

    @Override
    public LispObject getItem(int position) {
        try {
            return myContent[position];
        } catch (IndexOutOfBoundsException e) {
            return LispSymbol.ourNil;
        }
    }

    public void setContent (LispObject value) {
        for (int i = 0; i < myContent.length; ++i) {
            myContent[i] = value;
        }
    }
    
    @Override
    public void setItem(int c, LispObject value) {
        if (CharUtil.isAsciiChar(c) && myAscii instanceof LispSubCharTable) {
            ((LispSubCharTable) myAscii).setItem(c, value);
            return;
        }
        int i = CharUtil.index(c, 0, 0);
        if (!(myContent[i] instanceof LispSubCharTable)) {
            myContent[i] = new LispSubCharTable(1, i * CharUtil.charTableChars(0), myContent[i]);
        }
        ((LispSubCharTable)myContent[i]).set(c, value);
        if (CharUtil.isAsciiChar(c))
            myAscii = charTableAscii();
    }
    
    public void setRange (int from, int to, LispObject value) {
        if (from == to) {
            setItem(from, value);
            return;
        }
        for (int i = CharUtil.index(from, 0, 0), minChar = i * CharUtil.charTableChars(0);
             minChar <= to;
             i++, minChar += CharUtil.charTableChars(0))
        {
            myContent[i] = LispSubCharTable.setRange(myContent[i], 0, minChar, from, to, value);
        }
        if (CharUtil.isAsciiChar(from))
            myAscii = charTableAscii();
    }

    private LispObject charTableAscii () {
        if (!(myContent[0] instanceof LispSubCharTable))
            return myContent[0];
        LispObject item = ((LispSubCharTable) myContent[0]).getItem(0);
        if (!(item instanceof LispSubCharTable))
            return item;
        return ((LispSubCharTable) item).getItem(0);
    }
    
    public void setAscii (LispObject value) {
        myAscii = value;
    }

    public LispObject getAscii() {
        return myAscii;
    }

    private LispObject ref(int c) {
        LispObject value;
        if (CharUtil.isAsciiChar(c)) {
            value = myAscii;
            if (value instanceof LispSubCharTable)
                value = ((LispSubCharTable) value).getItem(c);
        } else {
            value = myContent[CharUtil.index(c, 0, 0)];
            if (value instanceof LispSubCharTable)
                value = ((LispSubCharTable) value).ref(c);
        }
        if (value.equals(LispSymbol.ourNil)) {
            value = myDefault;
            if (value.equals(LispSymbol.ourNil) && myParent instanceof LispCharTable)
                value = ((LispCharTable) myParent).ref(c);
        }
        return value;
    }

    private LispObject refAscii (int index) {
        LispCharTable table = null;
        LispObject value;
        do {								
            table = table != null ? (LispCharTable)myParent : this;
            value = !(table.getAscii() instanceof LispSubCharTable)
                    ? table.getAscii()
                    : ((LispSubCharTable) table.getAscii()).getItem(index);
            if (value.equals(LispSymbol.ourNil))
                value = table.getDefault();
        } while (value.equals(LispSymbol.ourNil) && !table.getParent().equals(LispSymbol.ourNil));
        return value;
    }

    public LispObject charTableRef (int index) {
        return CharUtil.isAsciiChar(index) ? refAscii(index) : ref(index);
    }

    public LispObject refAndRange (int c, int from, int to) {
        int index = CharUtil.index(c, 0, 0);
        int idx;
        LispObject val = myContent[index];
        if (from < 0)
            from = 0;
        if (to < 0)
            to = CharUtil.MAX_CHAR;
        if (val instanceof LispSubCharTable)
            val = ((LispSubCharTable) val).refAndRange(c, from, to, myDefault);
        else if (val.equals(LispSymbol.ourNil))
            val = myDefault;

        idx = index;
        while (from < idx * CharUtil.charTableChars(0)) {
            c = idx * CharUtil.charTableChars(0) - 1;
            idx--;
            LispObject this_val = myContent[idx];
            if (this_val instanceof LispSubCharTable)
                this_val = ((LispSubCharTable) this_val).refAndRange(c, from, to, myDefault);
            else if (this_val.equals(LispSymbol.ourNil))
                this_val = myDefault;
            if (!BuiltinsCore.eqs(this_val, val)) {
                from = c + 1;
                break;
            }
        }
        while (to >= (index + 1) * CharUtil.charTableChars(0)) {
            index++;
            c = index * CharUtil.charTableChars(0);
            LispObject this_val = myContent[index];
            if (this_val instanceof LispSubCharTable)
                this_val = ((LispSubCharTable) val).refAndRange(c, from, to, myDefault);
            else if (this_val.equals(LispSymbol.ourNil))
                this_val = myDefault;
            if (!BuiltinsCore.eqs(this_val, val)) {
                to = c - 1;
                break;
            }
        }
        return val;
    }


    @Override
    public int length() {
        return CharUtil.MAX_CHAR;
    }

    @Override
    public List<LispObject> toLispObjectList() {
        return null;
    }

    @Override
    public List<LispObject> mapCar(Environment environment, LispObject method) {
        return null;
    }

    @Override
    public LispObject copy() {
        Cloner c = new Cloner();
        return c.deepClone(this);
    }

    @Override
    public String toCharString() {
        return null;
    }
}
