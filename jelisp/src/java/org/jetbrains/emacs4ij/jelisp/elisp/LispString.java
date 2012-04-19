package org.jetbrains.emacs4ij.jelisp.elisp;

import com.intellij.openapi.actionSystem.Shortcut;
import com.intellij.openapi.util.text.StringUtil;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.ShortcutStringUtil;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.subroutine.Core;
import org.jetbrains.emacs4ij.jelisp.subroutine.Predicate;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/11/11
 * Time: 4:16 PM
 * To change this template use File | Settings | File Templates.
 *
 * elisp string = "anything between double quotation marks"
 */
public class LispString implements LispAtom, LispSequence, LispArray, StringOrVector {
    private String myData;    

    public LispString (String data) {
        if (data == null) {
            myData = "";
            return;
        }
        myData = data.replaceAll("\\\\\"", "\"");
    }

    public String getData() {
        return myData;
    }

    @Override
    public String toString() {
        return '"' + myData + '"';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        LispString that = (LispString) o;

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
    public int length() {
        return myData.length();
    }

    @Override
    public LispString substring(int from, int to) {
        return new LispString(myData.substring(from, to));
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
        return myData == null || myData.equals("");
    }

    public LispString capitalize() {
        return new LispString(StringUtil.capitalizeWords(myData.toLowerCase(), true));
    }
    
    public int match (LispString regexpStr, int from, boolean isCaseFoldSearch) {
        String regexp = regexpStr.getData();
        Pattern p1 = Pattern.compile("(\\\\)+\\(");
        Matcher m = p1.matcher(regexp);
        String s = m.replaceAll("(");
        p1 = Pattern.compile("(\\\\)+\\)");
        m = p1.matcher(s);
        s = m.replaceAll(")");
        Pattern p = isCaseFoldSearch ?
                Pattern.compile(s, Pattern.MULTILINE | Pattern.CASE_INSENSITIVE)
                : Pattern.compile(s, Pattern.MULTILINE);
        m = p.matcher(myData);
        if (m.find(from)) {
            GlobalEnvironment.INSTANCE.registerSearchResult(this, m);
            return m.start();
        }
        return -1;
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
        try {
            return new LispInteger(Integer.valueOf(myData, base));
        } catch (NumberFormatException e) {
            try {
                if (base != 10)
                    return new LispInteger(0);
                return new LispFloat(Double.valueOf(myData));
            } catch (NumberFormatException e2) {
                return new LispInteger(0);
            }
        }
    }
}
