package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.NoOpenedBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/12/12
 * Time: 10:52 AM
 * To change this template use File | Settings | File Templates.
 */
public abstract class SyntaxTable {
    private final static LispSyntaxTable myStandardSyntaxTable;
    static {
        myStandardSyntaxTable = new LispSyntaxTable();
        for (char i = 0; i < ' '; i++)
            myStandardSyntaxTable.setCharSyntax(i, SyntaxDescriptor.ClassType.PUNCTUATION);
        String punctuation = ".,;:?!#@~^'`";
        for (int i = 0; i < punctuation.length(); i++) {
            myStandardSyntaxTable.setCharSyntax(punctuation.charAt(i), SyntaxDescriptor.ClassType.PUNCTUATION);
        }
        myStandardSyntaxTable.setCharSyntax(' ', SyntaxDescriptor.ClassType.WHITESPACE);
        myStandardSyntaxTable.setCharSyntax('\t', SyntaxDescriptor.ClassType.WHITESPACE);
        myStandardSyntaxTable.setCharSyntax('\n', SyntaxDescriptor.ClassType.WHITESPACE);
        myStandardSyntaxTable.setCharSyntax('\r', SyntaxDescriptor.ClassType.WHITESPACE);
        myStandardSyntaxTable.setCharSyntax((char)12, SyntaxDescriptor.ClassType.WHITESPACE);

        for (char i = '0'; i <= '9'; i++)
            myStandardSyntaxTable.setCharSyntax(i, SyntaxDescriptor.ClassType.WORD);
        for (char i = 'A'; i <= 'Z'; i++)
            myStandardSyntaxTable.setCharSyntax (i, SyntaxDescriptor.ClassType.WORD);
        for (char i = 'a'; i <= 'z'; i++)
            myStandardSyntaxTable.setCharSyntax (i, SyntaxDescriptor.ClassType.WORD);
        myStandardSyntaxTable.setCharSyntax ('$', SyntaxDescriptor.ClassType.WORD);
        myStandardSyntaxTable.setCharSyntax ('%', SyntaxDescriptor.ClassType.WORD);

        myStandardSyntaxTable.setCharSyntax ('(', SyntaxDescriptor.ClassType.OPEN_PARENTHESIS, '(');
        myStandardSyntaxTable.setCharSyntax (')', SyntaxDescriptor.ClassType.CLOSE_PARENTHESIS, ')');
        myStandardSyntaxTable.setCharSyntax ('[', SyntaxDescriptor.ClassType.OPEN_PARENTHESIS, '[');
        myStandardSyntaxTable.setCharSyntax (']', SyntaxDescriptor.ClassType.CLOSE_PARENTHESIS, ']');
        myStandardSyntaxTable.setCharSyntax ('{', SyntaxDescriptor.ClassType.OPEN_PARENTHESIS, '{');
        myStandardSyntaxTable.setCharSyntax ('}', SyntaxDescriptor.ClassType.CLOSE_PARENTHESIS, '}');

        myStandardSyntaxTable.setCharSyntax ('"', SyntaxDescriptor.ClassType.STRING_QUOTE);
        myStandardSyntaxTable.setCharSyntax ('\\', SyntaxDescriptor.ClassType.ESCAPE);

        String others = "_-+*/&|<>=";
        for (int i = 0; i < others.length(); i++) {
            myStandardSyntaxTable.setCharSyntax(others.charAt(i), SyntaxDescriptor.ClassType.SYMBOL);
        }
        myStandardSyntaxTable.setCharSyntax((char) 127, SyntaxDescriptor.ClassType.PUNCTUATION);
        myStandardSyntaxTable.setRange(128, LispInteger.MAX_CHAR, SyntaxDescriptor.toSyntaxTableEntry(SyntaxDescriptor.ClassType.WORD));
    }

    private SyntaxTable() {}

    public static boolean isSyntaxTable (LispObject object) {
        return object instanceof LispSyntaxTable;
    }

    @Subroutine("syntax-table-p")
    public static LispSymbol syntaxTableP (LispObject object) {
        return LispSymbol.bool(isSyntaxTable(object));
    }

    @Subroutine("set-syntax-table")
    public static LispSyntaxTable setSyntaxTable (Environment environment, LispSyntaxTable syntaxTable) {
        environment.setSyntaxTable(syntaxTable);
        return syntaxTable;
    }

    @Subroutine("syntax-table")
    public static LispSyntaxTable getSyntaxTable (@Nullable Environment environment) {
        try {
            return environment.getSyntaxTable();
        } catch (NoOpenedBufferException | NullPointerException e) {
            return myStandardSyntaxTable;
        }
    }

    @Subroutine("standard-syntax-table")
    public static LispSyntaxTable getStandardSyntaxTable () {
        return myStandardSyntaxTable;
    }

    private static LispSyntaxTable syntaxTable (LispObject table) {
        if (Predicate.isNil(table))
            table = myStandardSyntaxTable;
        if (!isSyntaxTable(table))
            throw new WrongTypeArgumentException("char-table-p", table);
        return (LispSyntaxTable) table;
    }

    @Subroutine("copy-syntax-table")
    public static LispSyntaxTable copy (@Optional LispObject table) {
        table = syntaxTable(table);
        return ((LispSyntaxTable)table).copy();
    }

    @Subroutine("make-syntax-table")
    public static LispSyntaxTable make (@Optional LispObject parent) {
        parent = syntaxTable(parent);
        return new LispSyntaxTable((LispSyntaxTable) parent);
    }

    @Subroutine("string-to-syntax")
    public static LispList stringToSyntax (LispString string) {
        return SyntaxDescriptor.toSyntaxTableEntry(string.getData());
    }

    @Subroutine(value = "modify-syntax-entry", isCmd = true, interactive = "cSet syntax for character: \nsSet syntax for %s to: ")
    public static LispSymbol modifySyntaxEntry (Environment environment, LispObject character, LispString newEntry,
                                     @Optional LispObject syntaxTable) {
        if (Predicate.isNil(syntaxTable))
            syntaxTable = environment.getSyntaxTable();
        if (!isSyntaxTable(syntaxTable))
            throw new WrongTypeArgumentException("syntax-table-p", syntaxTable);
        ((LispSyntaxTable)syntaxTable).modifyEntry(character, stringToSyntax(newEntry));
        return LispSymbol.ourNil;
    }

    public static int getCharFullSyntaxCode (Environment environment, int c) {
        LispObject car = getSyntaxTable(environment).getCharSyntax(c).car();
        return  car instanceof LispInteger
                ? ((LispInteger) car).getData()
                : SyntaxDescriptor.getSyntaxClass(SyntaxDescriptor.ClassType.WHITESPACE);
    }

    public static SyntaxDescriptor.ClassType getSyntaxClass (@Nullable Environment environment, char c) {
        LispObject car = getSyntaxTable(environment).getCharSyntax(c).car();
        if (!(car instanceof LispInteger))
            return SyntaxDescriptor.ClassType.WHITESPACE;
        return SyntaxDescriptor.classBySyntaxCode(((LispInteger) car).getData());
    }

    public static boolean isWord (Environment environment, char c) {
        return getSyntaxClass(environment, c) == SyntaxDescriptor.ClassType.WORD;
    }

    /**
     *
     * @param environment taken
     * @return String of all characters from current syntax table which are marked as WHITESPACE
     */
    public static String getWhitespaces (Environment environment) {
        return getSyntaxTable(environment).getAllCharsOfType(SyntaxDescriptor.ClassType.WHITESPACE);
    }
}
