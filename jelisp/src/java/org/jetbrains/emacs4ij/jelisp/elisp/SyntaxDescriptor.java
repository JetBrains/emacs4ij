package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.exception.InternalException;
import org.jetbrains.emacs4ij.jelisp.subroutine.Core;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/13/12
 * Time: 11:38 AM
 * To change this template use File | Settings | File Templates.
 */

public class SyntaxDescriptor {
    public static enum CommentStyle {A, B, GENERIC}

    public static enum ClassType {WHITESPACE, PUNCTUATION, WORD, SYMBOL, OPEN_PARENTHESIS, CLOSE_PARENTHESIS,
        EXPRESSION_PREFIX, STRING_QUOTE, PAIRED_DELIMITER, ESCAPE, CHARACTER_QUOTE, COMMENT_START, COMMENT_END, INHERIT,
        GENERIC_COMMENT, GENERIC_STRING}

    public static enum FlagType {START_2CHAR_COMMENT_START, END_2CHAR_COMMENT_START,
        START_2CHAR_COMMENT_END,   END_2CHAR_COMMENT_END,
        PREFIX, PART_OF_B_COMMENT, PART_OF_NESTABLE_COMMENT}

    private static final Map<Character, Integer> ourSyntaxClassMap;
    private static final Map<Character, Integer> ourFlagMap;
    static {
        ourSyntaxClassMap = new LinkedHashMap<>();
        ourSyntaxClassMap.put(' ', 0);
        ourSyntaxClassMap.put('-', 0);
        String classes = ".w_()\'\"$\\/<>@!|";
        for (int i = 0; i < classes.length(); i++) {
            ourSyntaxClassMap.put(classes.charAt(i), i + 1);
        }
        ourFlagMap = new LinkedHashMap<>();
        String flags = "1234pbn";
        for (int i = 0; i < flags.length(); i++) {
            ourFlagMap.put(flags.charAt(i), 1 << (16 + i));
        }
    }

    protected static int makeFullSyntaxCode (String string) {
        int syntaxCode = ourSyntaxClassMap.get(string.charAt(0));
        for (int i = 2; i < string.length(); i++) {
            if (ourFlagMap.get(string.charAt(i)) == null)
                continue;
            syntaxCode |= ourFlagMap.get(string.charAt(i));
        }
        return syntaxCode;
    }

    /**
     * @return (syntax code . matching char)
     */
    public static LispList toSyntaxTableEntry (String string) {
        if (string.length() == 0 || !ourSyntaxClassMap.keySet().contains(string.charAt(0)))
            Core.error(JelispBundle.message("invalid.syntax.descr.letter", string.charAt(0)));
        if (string.charAt(0) == '@') //inherit
            return LispList.list();
        int syntaxCode = makeFullSyntaxCode(string);
        char matchingCharacter = string.length() > 1 ?  string.charAt(1) : ' ';
        return matchingCharacter == ' '
                ? LispList.list(new LispInteger(syntaxCode))
                : LispList.cons(new LispInteger(syntaxCode), new LispInteger(matchingCharacter));
    }

    public static int getSyntaxClass (ClassType type) {
        return Arrays.asList(ClassType.values()).indexOf(type);
    }

    public static LispList toSyntaxTableEntry (ClassType type) {
        return LispList.list(new LispInteger(getSyntaxClass(type)));
    }

    static LispList toSyntaxTableEntry (ClassType type, char matchingCharacter) {
        return LispList.cons(new LispInteger(getSyntaxClass(type)),
                new LispInteger(matchingCharacter));
    }

    private static int getFlagShift (FlagType type) {
        return 16 + Arrays.asList(FlagType.values()).indexOf(type);
    }

    public static boolean is(FlagType type, int code) {
        return  ((code >> getFlagShift(type)) & 1) == 1;
    }

    public static CommentStyle commentStyle (int code) {
        return  ((code >> 21) & 1) == 1 ? CommentStyle.A : CommentStyle.B;
    }

    public static ClassType classBySyntaxCode (int syntax) {
        int syntaxClass = syntax & 255;
        assert syntaxClass < SyntaxDescriptor.ClassType.values().length;
        return ClassType.values()[syntaxClass];
    }

    public static char getSyntaxClassChar(ClassType type) {
        for (Map.Entry<Character, Integer> entry: ourSyntaxClassMap.entrySet()) {
            if (ClassType.values()[entry.getValue()] == type)
                return entry.getKey();
        }
        throw new InternalException(JelispBundle.message("nonexistent.syntax.class", type.toString()));
    }
}