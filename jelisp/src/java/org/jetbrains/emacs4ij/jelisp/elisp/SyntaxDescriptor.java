package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.subroutine.Core;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/13/12
 * Time: 11:38 AM
 * To change this template use File | Settings | File Templates.
 */

public class SyntaxDescriptor {
    public static enum Type {WHITESPACE, PUNCTUATION, WORD, SYMBOL, OPEN_PARENTHESIS, CLOSE_PARENTHESIS,
        EXPRESSION_PREFIX, STRING_QUOTE, PAIRED_DELIMITER, ESCAPE, CHARACTER_QUOTE, COMMENT_START, COMMENT_END, INHERIT,
        GENERIC_COMMENT, GENERIC_STRING}

    private static final Map<Character, Integer> ourSyntaxClassMap;
    private static final Map<Character, Integer> ourFlagMap;
    static {
        ourSyntaxClassMap = new HashMap<>();
        ourSyntaxClassMap.put(' ', 0);
        ourSyntaxClassMap.put('-', 0);
        String classes = ".w_()\'\"$\\/<>@!|";
        for (int i = 0; i < classes.length(); i++) {
            ourSyntaxClassMap.put(classes.charAt(i), i + 1);
        }
        ourFlagMap = new HashMap<>();
        String flags = "1234pbn";
        for (int i = 0; i < flags.length(); i++) {
            ourFlagMap.put(flags.charAt(i), 1 << (16 + i));
        }
    }

    /**
     * @return (syntax code . matching char)
     */
    public static LispList toSyntaxTableEntry (String string) {
        if (string.length() == 0 || !ourSyntaxClassMap.keySet().contains(string.charAt(0)))
            Core.error(JelispBundle.message("invalid.syntax.descr.letter", string.charAt(0)));
        if (ourSyntaxClassMap.get(string.charAt(0)) == '@') //inherit
            return LispList.list();
        int syntaxCode = ourSyntaxClassMap.get(string.charAt(0));
        char matchingCharacter = ' ';
        if (string.length() > 1) {
            matchingCharacter = string.charAt(1);
            for (int i = 2; i < string.length(); i++) {
                if (ourFlagMap.get(string.charAt(i)) == null)
                    continue;
                syntaxCode |= ourFlagMap.get(string.charAt(i));
            }
        }
        return matchingCharacter == ' ' ? LispList.list(new LispInteger(syntaxCode))
                : LispList.cons(new LispInteger(syntaxCode), new LispInteger(matchingCharacter));
    }

    public static LispList toSyntaxTableEntry (Type type) {
        return LispList.list(new LispInteger(Arrays.asList(Type.values()).indexOf(type)));
    }

    static LispList toSyntaxTableEntry (Type type, char matchingCharacter) {
        return LispList.cons(new LispInteger(Arrays.asList(Type.values()).indexOf(type)),
                      new LispInteger(matchingCharacter));
    }
}