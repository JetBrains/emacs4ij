package org.jetbrains.emacs4ij.jelisp;

import com.intellij.openapi.actionSystem.KeyboardShortcut;
import com.intellij.openapi.actionSystem.Shortcut;
import com.intellij.openapi.util.text.StringUtil;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/28/12
 * Time: 1:39 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class ShortcutStringUtil {
    private ShortcutStringUtil() {}

    private static final List<String> myModifiers = Arrays.asList("meta", "ctrl", "shift", "alt");
    private static final List<String> myPunctuation = Arrays.asList("SPACE", "ESCAPE", "MINUS");
    private static final List<String> myFunctional;
    private static final Map<String, String> myReplaceMap;
    static {
        myReplaceMap = new LinkedHashMap<>();
        myReplaceMap.put(" ", " SPACE ");
        myReplaceMap.put("ESC", " ESCAPE ");
        myReplaceMap.put(regModifier('M'), " meta ");
        myReplaceMap.put(regModifier('C'), " ctrl ");
        myReplaceMap.put(regModifier('S'), " shift ");
        myReplaceMap.put(regModifier('A'), " alt ");

        myFunctional = new ArrayList<>();
        for (int i = 1; i < 13; ++i) {
            myFunctional.add("F"+i);
            myReplaceMap.put(regFunctional(i), " F" + i + " ");
        }
        
        myReplaceMap.put("-", " MINUS ");
    }


    //todo: it's public only for test
    public static String toShortcutString (LispString string) {        
        String data = string.getData();
        for (Map.Entry<String, String> entry: myReplaceMap.entrySet()) {
            data = data.replaceAll(entry.getKey(), entry.getValue());
        }

        String[] split = data.trim().split(" ");
        Pattern p = Pattern.compile(".");
        for (int i = 0, splitLength = split.length; i < splitLength; i++) {
            String item = split[i];
            if (!myModifiers.contains(item) && !myPunctuation.contains(item) && !myFunctional.contains(item)) {
                Matcher m = p.matcher(item);
                StringBuffer sb = new StringBuffer();
                while (m.find()) {
                    String replacement = " " + m.group().toUpperCase();
                    m.appendReplacement(sb, replacement);
                }
                m.appendTail(sb);
                data = data.replaceAll("(^|\\s)" + item + "(\\s|$)" , sb.toString()+ " ");
            }
        }

        return data.trim();
    }
    
    public static List<Shortcut> toKeyboardShortcutList(LispString string) {
        String[] keystrokeContent = toShortcutString(string).split(" ");
        List<Shortcut> keystrokes = new ArrayList<>();
        int sequenceStart = 0;
        for (int i = 0, keystrokeContentLength = keystrokeContent.length; i < keystrokeContentLength; i++) {
            String item = keystrokeContent[i];
            if (StringUtil.isEmptyOrSpaces(item))
                continue;
            if (!myModifiers.contains(item)) {
                StringBuilder keystrokeBuilder = new StringBuilder();
                for (int j = sequenceStart; j <= i; ++j)
                    keystrokeBuilder.append(keystrokeContent[j]).append(" ");
                sequenceStart = i + 1;
                keystrokes.add(KeyboardShortcut.fromString(keystrokeBuilder.toString()));
            }
        }
        return keystrokes;
    }

    private static String regModifier(char modifier) {
        return "\\s?\\\\" + modifier + "-";
    }
    
    private static String regFunctional (int n) {
        return "<[Ff]" + n + ">";
    }
    
    public static String fromShortcutList (List<Shortcut> list) {
        return null;
    }

}
