package org.jetbrains.emacs4ij.jelisp.elisp;

import com.rits.cloning.Cloner;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.subroutine.Predicate;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/12/12
 * Time: 10:49 AM
 * To change this template use File | Settings | File Templates.
 */
public class LispSyntaxTable implements LispCharTable {
    private LispSyntaxTable myParent;
    private Map<Integer, LispList> myData = new HashMap<>(LispInteger.MAX_CHAR + 1, 1);

    /**
     * this constructor is for standard-syntax-table only!
     * @see org.jetbrains.emacs4ij.jelisp.subroutine.SyntaxTable
     */
    public LispSyntaxTable() {
        myParent = null;
    }

    public LispSyntaxTable (@NotNull LispSyntaxTable parent) {
        myParent = parent;
    }

    @Override
    public LispObject evaluate(Environment environment) {
        return this;
    }

    @Override
    public int size() {
        if (isEmpty())
            return 0;
        return Collections.max(myData.keySet());
    }

    @Override
    public List<LispObject> toLispObjectList() {
        throw new UnsupportedOperationException();
    }

    @Override
    public List<LispObject> mapCar(Environment environment, LispObject method) {
        throw new UnsupportedOperationException();
    }

    @Override
    public LispSyntaxTable copy () {
        return new Cloner().deepClone(this);
    }

    @Override
    public String toCharString() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isEmpty() {
        return myData.isEmpty();
    }

    @Override
    public LispSequence delete(LispObject element) {
        throw new UnsupportedOperationException();
    }

    public void setCharSyntax (char c, SyntaxDescriptor.ClassType value) {
        myData.put((int)c, SyntaxDescriptor.toSyntaxTableEntry(value));
    }

    private void setCharSyntax (int i, LispList value) {
        myData.put(i, value);
    }

    public void setCharSyntax (char c, SyntaxDescriptor.ClassType value, char matchingCharacter) {
        myData.put((int)c, SyntaxDescriptor.toSyntaxTableEntry(value, matchingCharacter));
    }

    public LispList getCharSyntax (int c) {
        if (myData.containsKey(c))
            return myData.get(c);
        if (myParent != null)
            return myParent.getCharSyntax(c);
        System.err.println("request for char syntax of " + c);
        return LispList.list();
    }

    public void setRange (int start, int end, LispList value) {
        for (int i = start; i <= end; i++)
            setCharSyntax(i, value);
    }

    public void modifyEntry (LispObject character, LispList value) {
        if (character instanceof LispInteger) {
            setCharSyntax(((LispInteger) character).getData(), value);
        } else if (character instanceof LispList) {
            LispObject car = ((LispList) character).car();
            LispObject cdr = ((LispList) character).cdr();
            if (!Predicate.isCharacter(car))
                throw new WrongTypeArgumentException("characterp", car);
            if (!Predicate.isCharacter(cdr))
                throw new WrongTypeArgumentException("characterp", cdr);
            setRange(((LispInteger)car).getData(), ((LispInteger)cdr).getData(), value);
        } else {
            throw new WrongTypeArgumentException("characterp", character);
        }
    }

    public String getAllCharsOfType (SyntaxDescriptor.ClassType type) {
        StringBuilder result = new StringBuilder("");
        for (Map.Entry<Integer, LispList> entry: myData.entrySet()) {
            LispObject car = entry.getValue().car();
            SyntaxDescriptor.ClassType current = car instanceof LispInteger
                    ? SyntaxDescriptor.classBySyntaxCode(((LispInteger)car).getData())
                    : SyntaxDescriptor.ClassType.WHITESPACE;
            if (current == type) {
                result.append((char)(int)entry.getKey());
            }
        }
        return result.toString();
    }

    public String getAllChars () {
        StringBuilder chars = new StringBuilder();
        for (int key: myData.keySet()) {
            chars.append((char)key);
        }
        return chars.toString();
    }

    @Override
    public void setItem(int position, LispObject value) {
        assert value instanceof LispList;
        myData.put(position, (LispList) value);
    }

    @Override
    public LispObject getItem(int position) {
        return getCharSyntax(position);
    }
}
