package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.exception.NotImplementedException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinPredicates;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinsCore;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
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
public class LispString implements LispAtom, LispSequence, LispArray, LispStringOrVector {
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
            data.add(BuiltinsCore.functionCall(environment, method, item));
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
        return -1;
    }

    @Override
    public void setItem(int position, LispObject value) {
        if (!BuiltinPredicates.isCharacter(value))
            throw new WrongTypeArgumentException("characterp", value);
        myData = myData.substring(0, position) + ((LispInteger)value).toCharacterString() + myData.substring(position + 1);
    }

    @Override
    public LispObject getItem(int position) {
        char c = myData.charAt(position);
        return new LispInteger(c);
    }
    
    public int lengthInBytes() {
        try {
            return myData.getBytes("UTF-8").length;
        } catch (UnsupportedEncodingException e) {
            return myData.length();
        }
    }
    
    public char charAt (int position) {
        return myData.charAt(position);
    }
    
    public boolean isMultibyte () {
        //todo
        return false;
    }

    @Override
    public String toShortcutString() {
        String data = myData;
        Map<String, String> replaceMap = new HashMap<>();
        replaceMap.put("\\\\{0,2}M-", "meta ");
        replaceMap.put("\\\\{0,2}C-", "ctrl ");
//        replaceMap.put("H-", "hyper ");
        replaceMap.put("\\\\{0,2}S-", "shift ");
//        replaceMap.put("s-", "super ");
        replaceMap.put("\\\\{0,2}A-", "alt ");

        for (Map.Entry<String, String> entry: replaceMap.entrySet()) {
            data = data.replaceAll(entry.getKey(), entry.getValue());
        }
        Pattern p = Pattern.compile("(^|\\s)\\w(\\s|$)");
        Matcher m = p.matcher(data);
        StringBuffer sb = new StringBuffer();
        while (m.find()) {
            String replacement = m.group().toUpperCase();
            m.appendReplacement(sb, replacement);
        }
        m.appendTail(sb);
        return sb.toString();
    }

    @Override
    public boolean isInteractive() {
        throw new NotImplementedException("LispString.isInteractive()");
    }

    @Override
    public String getInteractiveString() {
        throw new NotImplementedException("LispString.getInteractiveString()");
    }

    @Override
    public LispList getInteractiveForm() {
        throw new NotImplementedException("LispString.getInteractiveForm()");
    }
}
