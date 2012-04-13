package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
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
            myStandardSyntaxTable.setCharSyntax(i, SyntaxDescriptor.Type.PUNCTUATION);
        String punct = ".,;:?!#@~^'`";
        for (int i = 0; i < punct.length(); i++) {
            myStandardSyntaxTable.setCharSyntax(punct.charAt(i), SyntaxDescriptor.Type.PUNCTUATION);
        }
        myStandardSyntaxTable.setCharSyntax(' ', SyntaxDescriptor.Type.WHITESPACE);
        myStandardSyntaxTable.setCharSyntax('\t', SyntaxDescriptor.Type.WHITESPACE);
        myStandardSyntaxTable.setCharSyntax('\n', SyntaxDescriptor.Type.WHITESPACE);
        myStandardSyntaxTable.setCharSyntax('\r', SyntaxDescriptor.Type.WHITESPACE);
        myStandardSyntaxTable.setCharSyntax((char)12, SyntaxDescriptor.Type.WHITESPACE);

        for (char i = '0'; i <= '9'; i++)
            myStandardSyntaxTable.setCharSyntax(i, SyntaxDescriptor.Type.WORD);
        for (char i = 'A'; i <= 'Z'; i++)
            myStandardSyntaxTable.setCharSyntax (i, SyntaxDescriptor.Type.WORD);
        for (char i = 'a'; i <= 'z'; i++)
            myStandardSyntaxTable.setCharSyntax (i, SyntaxDescriptor.Type.WORD);
        myStandardSyntaxTable.setCharSyntax ('$', SyntaxDescriptor.Type.WORD);
        myStandardSyntaxTable.setCharSyntax ('%', SyntaxDescriptor.Type.WORD);

        myStandardSyntaxTable.setCharSyntax ('(', SyntaxDescriptor.Type.OPEN_PARENTHESIS, '(');
        myStandardSyntaxTable.setCharSyntax (')', SyntaxDescriptor.Type.CLOSE_PARENTHESIS, ')');
        myStandardSyntaxTable.setCharSyntax ('[', SyntaxDescriptor.Type.OPEN_PARENTHESIS, '[');
        myStandardSyntaxTable.setCharSyntax (']', SyntaxDescriptor.Type.CLOSE_PARENTHESIS, ']');
        myStandardSyntaxTable.setCharSyntax ('{', SyntaxDescriptor.Type.OPEN_PARENTHESIS, '{');
        myStandardSyntaxTable.setCharSyntax ('}', SyntaxDescriptor.Type.CLOSE_PARENTHESIS, '}');

        myStandardSyntaxTable.setCharSyntax ('"', SyntaxDescriptor.Type.STRING_QUOTE);
        myStandardSyntaxTable.setCharSyntax ('\\', SyntaxDescriptor.Type.ESCAPE);

        String others = "_-+*/&|<>=";
        for (int i = 0; i < others.length(); i++) {
            myStandardSyntaxTable.setCharSyntax(others.charAt(i), SyntaxDescriptor.Type.SYMBOL);
        }
        myStandardSyntaxTable.setCharSyntax((char) 127, SyntaxDescriptor.Type.PUNCTUATION);
        myStandardSyntaxTable.setRange(128, LispInteger.MAX_CHAR, SyntaxDescriptor.toSyntaxTableEntry(SyntaxDescriptor.Type.WORD));
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
    public static LispSyntaxTable getSyntaxTable (Environment environment) {
        return environment.getSyntaxTable();
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
}
