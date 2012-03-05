package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.ForwardParser;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinPredicates;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinsCore;

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
public class LispString extends LispAtom implements LispSequence, LispArray {
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
    public LObject evaluate(Environment environment) {
        return this;
    }

    @Override
    public int length() {
        return myData.length();
    }

    @Override
    public List<LObject> toLObjectList() {
        ArrayList<LObject> data = new ArrayList<>();
        for (int i = 0; i < myData.length(); ++i) {
            data.add(new LispInteger(myData.charAt(i)));
        }
        return data;
    }

    @Override
    public List<LObject> mapCar(Environment environment, LObject method) {
        ArrayList<LObject> data = new ArrayList<>();
        for (LObject item: toLObjectList()) {
            data.add(BuiltinsCore.functionCall(environment, method, item));
        }
        return data;
    }

    @Override
    public LObject copy() {
        return new LispString(myData);
    }

    @Override
    public String toCharString() {
        return myData;
    }
    
    private boolean isDelimiter(int c) {
        return c < 48 || (c > 57 && c < 65) || (c > 90 && c < 97) || c > 122;
    }
    
    public LispString capitalize() {
        boolean firstAfterDelimiter = true;
        char[] s = myData.toCharArray();
        for (int c = 0; c < s.length; ++c) {            
            if (isDelimiter(s[c])) {
                firstAfterDelimiter = true;
                continue;
            }
            if (firstAfterDelimiter) {
                s[c] = Character.toUpperCase(s[c]);
                firstAfterDelimiter = false;
                continue;
            }            
            s[c] = Character.toLowerCase(s[c]);
        }        
        return new LispString(new String(s));
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


//        String[] data = myData.split("\\n");
//        int before = 0;
//        for (String s1: data) {
//            m = p.matcher(s1);
//            if (m.find(from)) {
//                GlobalEnvironment.INSTANCE.registerSearchResult(this, m);
//                return m.start() + before;
//            }
//            before += s1.length();
//        }
        return -1;

//        return data.indexOf(regexp, from);
    }

    @Override
    public void setItem(int position, LObject value) {
        if (!BuiltinPredicates.isCharacter(value))
            throw new WrongTypeArgumentException("characterp", value.toString());
        myData = myData.substring(0, position) + ((LispInteger)value).toCharacterString() + myData.substring(position + 1);
    }

    @Override
    public LObject getItem(int position) {
        char c = myData.charAt(position);
        String s = "?" + c;
        return new ForwardParser().parseLine(s);
    }
}
