package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

import java.util.HashMap;
import java.util.Map;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 5/2/12
 * Time: 12:50 PM
 * To change this template use File | Settings | File Templates.
 */
@Error("invalid-regexp")
public class InvalidRegexpException extends LispException {
    public static enum Error {BACK_REFERENCE, COLLATION_CHAR, CHAR_CLASS_NAME, REGEXP, BACKSLASH, OPEN_ROUND_BRACKET,
        CLOSE_ROUND_BRACKET, SQUARE_BRACKET, FIGURE_BRACKET, FIGURE_BRACKET_CONTENT, RANGE_END, PRECEDING_REGEXP,
        PREMATURE_END}
    private static Map<Error, String> myMessageMap = new HashMap<>();
    static {
        myMessageMap.put(Error.BACK_REFERENCE, JelispBundle.message("invalid.back.ref"));
        myMessageMap.put(Error.COLLATION_CHAR, JelispBundle.message("invalid.collation.char"));
        myMessageMap.put(Error.CHAR_CLASS_NAME, JelispBundle.message("invalid.char.class.name"));
        myMessageMap.put(Error.REGEXP, JelispBundle.message("invalid.regexp"));
        myMessageMap.put(Error.BACKSLASH, JelispBundle.message("trailing.backslash"));
        myMessageMap.put(Error.OPEN_ROUND_BRACKET, JelispBundle.message("unmatched.open.round.bracket"));
        myMessageMap.put(Error.CLOSE_ROUND_BRACKET, JelispBundle.message("unmatched.closing.round.bracket"));
        myMessageMap.put(Error.SQUARE_BRACKET, JelispBundle.message("unmatched.square.bracket"));
        myMessageMap.put(Error.FIGURE_BRACKET, JelispBundle.message("unmatched.figure.bracket"));
        myMessageMap.put(Error.FIGURE_BRACKET_CONTENT, JelispBundle.message("invalid.figure.bracket.content"));
        myMessageMap.put(Error.RANGE_END, JelispBundle.message("invalid.range.end"));
        myMessageMap.put(Error.PRECEDING_REGEXP, JelispBundle.message("invalid.preceding.regexp"));
        myMessageMap.put(Error.PREMATURE_END, JelispBundle.message("premature.regexp.end"));
    }

    public InvalidRegexpException(Error type) {
        super("'(invalid-regexp \"" + myMessageMap.get(type) + "\")");
    }
}
