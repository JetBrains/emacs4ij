package org.jetbrains.emacs4ij.jelisp.elisp;

import com.intellij.openapi.actionSystem.Shortcut;
import com.intellij.openapi.util.text.StringUtil;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.ArgumentOutOfRange;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.subroutine.Core;
import org.jetbrains.emacs4ij.jelisp.subroutine.Predicate;
import org.jetbrains.emacs4ij.jelisp.subroutine.SyntaxTable;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/11/11
 * Time: 4:16 PM
 * To change this template use File | Settings | File Templates.
 *
 * elisp string = "anything between double quotation marks"
 */
public final class LispString extends TextPropertiesHolder implements LispAtom, LispSequence, LispArray, StringOrVector {
    private String myData;

    public LispString (String data) {
        if (data == null) {
            myData = "";
            return;
        }
        myData = data.replaceAll("\\\\\"", "\"");
    }

    public LispString (String data, List<TextPropertiesInterval> textProperties) {
        this(data);
        setTextProperties(textProperties);
    }

    public String getData() {
        return myData;
    }

    @Override
    public String toString() {
        if (noTextProperties())
            return '"' + myData + '"';
        return "#(\"" + myData + "\" " + intervalsString() + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        LispString that = (LispString) o;
        if (myIntervals != null ? !myIntervals.equals(that.myIntervals) : that.myIntervals != null) return false;
        return !(myData != null ? !myData.equals(that.myData) : that.myData != null);
    }

    @Override
    public int hashCode() {
        return myData != null ? myData.hashCode() : 0;
    }

    @Override
    /**
     * no parameters required
     */
    public LispObject evaluate(Environment environment) {
        return this;
    }

    @Override
    public int size() {
        return myData.length();
    }

    @Override
    public LispString substring(int from, int to) {
        return substring(from, to, true);
    }

    public LispString substring(int from, int to, boolean withProperties) {
        String data = myData.substring(from, to);
        return !noTextProperties() && withProperties
                ? new LispString(myData.substring(from, to), getTextPropertiesInRange(from, to))
                : new LispString(data);
    }

    @Override
    public List<LispObject> toLispObjectList() {
        ArrayList<LispObject> data = new ArrayList<>();
        for (int i = 0; i < myData.length(); ++i) {
            data.add(new LispInteger(myData.charAt(i)));
        }
        return data;
    }

    @Override
    public List<LispObject> mapCar(Environment environment, LispObject method) {
        ArrayList<LispObject> data = new ArrayList<>();
        for (LispObject item: toLispObjectList()) {
            data.add(Core.functionCall(environment, method, item));
        }
        return data;
    }

    @Override
    public LispObject copy() {
        return new LispString(myData);
    }

    @Override
    public String toCharString() {
        return myData;
    }

    @Override
    public boolean isEmpty() {
        return StringUtil.isEmpty(myData);
    }

    @Override
    public LispString delete(LispObject element) {
        if (!(Predicate.isCharacter(element)))
            return this;
        String s = ((LispInteger)element).toCharacterString();
        return new LispString(myData.replaceAll(s, ""));
    }

    public String capitalize (Environment environment) {
        StringBuilder capitalized = new StringBuilder();
        for (int i = 0; i < myData.length(); i++) {
            char c = myData.charAt(i);
            if (i == 0 || (i > 0 && !SyntaxTable.isWord(environment, myData.charAt(i - 1)))) {
                capitalized.append(Character.toUpperCase(c));
                continue;
            }
            capitalized.append(Character.toLowerCase(c));
        }
        return capitalized.toString();
    }

    public int match (Environment environment, LispString regexpStr, int from, boolean isCaseFoldSearch) {
        return StringRegexpUtil.match(environment, regexpStr.getData(), myData, from, isCaseFoldSearch);
    }

    @Override
    public void setItem(int position, LispObject value) {
        if (!Predicate.isCharacter(value))
            throw new WrongTypeArgumentException("characterp", value);
        myData = myData.substring(0, position) + ((LispInteger)value).toCharacterString() + myData.substring(position + 1);
    }

    @Override
    public LispObject getItem(int position) {
        char c = myData.charAt(position);
        return new LispInteger(c);
    }

    @Override
    public List<Shortcut> toKeyboardShortcutList() {
        return ShortcutStringUtil.toKeyboardShortcutList(this);
    }

    public LispNumber toNumber (int base) {
        StringBuilder sb = new StringBuilder();
        String source = myData.trim();
        for (int i = 0; i < source.length(); i++) {
            char c = source.charAt(i);
            if (c == '+' || c == '-' || c == 'e' || c == 'E' || c == '.' || Character.isDigit(c))
                sb.append(c);
            else break;
        }
        String data = sb.toString();
        try {
            return new LispInteger(Integer.valueOf(data, base));
        } catch (NumberFormatException e) {
            try {
                if (base != 10)
                    return new LispInteger(0);
                return new LispFloat(Double.valueOf(data));
            } catch (NumberFormatException e2) {
                return new LispInteger(0);
            }
        }
    }

    public LispString replace (int from, int to, String text) {
        try {
            String data = myData.substring(0, from) + text + myData.substring(to);
            return new LispString(data);
        } catch (StringIndexOutOfBoundsException e) {
            throw new ArgumentOutOfRange(from, to);
        }
    }

    public LispString getExactRegexp () {
        return new LispString(StringRegexpUtil.getExactRegexp(myData));
    }

    public int getFirstCharacter() {
        if (StringUtil.isEmpty(myData))
            return 0;
        return (int)myData.charAt(0);
    }
}
