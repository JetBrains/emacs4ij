package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.parser.exception.ScanException;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/13/12
 * Time: 4:48 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class Scan {
    private Scan() {}

    @Subroutine("scan-sexps")
    public static LispObject scanSexps (Environment environment, LispInteger from, LispInteger count) {
        return scanLists(environment, from.getData(), count.getData(), 0, true);
    }

    @Subroutine("scan-lists")
    public static LispObject scanLists (Environment environment, LispInteger from, LispInteger count, LispInteger depth) {
        return scanLists(environment, from.getData(), count.getData(), depth.getData(), false);
    }

    private static LispObject scanLists (Environment environment, int from, int count, int depth, boolean sexpFlag) {
        int minDepth = depth > 0 ? depth : 0;
        if (from < environment.getBufferCurrentForEditing().pointMin())
            from = environment.getBufferCurrentForEditing().pointMin();
        if (from > environment.getBufferCurrentForEditing().pointMax())
            from = environment.getBufferCurrentForEditing().pointMax();

        boolean ignoreComments = environment.find("parse-sexp-ignore-comments").getValue().equals(LispSymbol.ourT);

        if (count > 0)
            return scanForward(environment, from, depth, minDepth, count, sexpFlag, ignoreComments);
        if (count < 0)
            return scanBackward(environment, from);
        return LispSymbol.ourNil;
    }

    private static LispObject scanForward (Environment environment, int from, int depth, int minDepth,
                                           int count, boolean sexpFlag, boolean ignoreComments) {
        int finish = environment.getBufferCurrentForEditing().pointMax();
        boolean metPairedDelimiter = false;
        SyntaxIterator iterator = new SyntaxIterator(environment, from, finish);
        while (iterator.valid() && count > 0) {

            SyntaxDescriptor.CommentStyle commentStyle = iterator.current.commentStyle();
            if (depth == minDepth)
                iterator.lastGood = from;
            SyntaxDescriptor.ClassType code = iterator.current.syntax;

            iterator.next();

            if (SyntaxDescriptor.is(SyntaxDescriptor.FlagType.PREFIX, iterator.previous.fullSyntaxCode))
                continue;

            if (iterator.valid()
                    && SyntaxDescriptor.is(SyntaxDescriptor.FlagType.START_2CHAR_COMMENT_START, iterator.previous.fullSyntaxCode)
                    && SyntaxDescriptor.is(SyntaxDescriptor.FlagType.END_2CHAR_COMMENT_START, iterator.current.fullSyntaxCode)
                    && ignoreComments) {
                code = SyntaxDescriptor.ClassType.COMMENT_START;
                commentStyle = iterator.current.commentStyle();
                iterator.next();
            }

            switch (code) {
                case ESCAPE: case CHARACTER_QUOTE:
                    iterator.checkUnbalancedParentheses();
                    iterator.next();
                    break;
                case WORD: case SYMBOL:
                    if (depth != 0 || !sexpFlag)
                        break;
                    while (iterator.valid()) {
                        switch (iterator.current.syntax) {
                            case ESCAPE: case CHARACTER_QUOTE:
                                iterator.next();
                                iterator.checkUnbalancedParentheses();
                                break;
                            case WORD: case SYMBOL: case EXPRESSION_PREFIX:
                                break;
                            default:
                                count--;
                                continue;
                        }
                        iterator.next();
                    }
                    count--;
                    continue;
                case GENERIC_COMMENT:
                    commentStyle = SyntaxDescriptor.CommentStyle.GENERIC;
                case COMMENT_START:
                    if (!ignoreComments)
                        break;
                    if (!skipCommentForward(commentStyle, false, iterator)) {
                        iterator.checkUnbalancedParentheses(depth);
                        count--;
                        continue;
                    }
                    iterator.next();
                    break;
                case PAIRED_DELIMITER:
                    if (!sexpFlag)
                        break;
                    if (!iterator.atEnd() && iterator.previous.character == iterator.current.character)
                        iterator.next();
                    if (metPairedDelimiter) {
                        metPairedDelimiter = false;
                        if (--depth == 0) {
                            count--;
                            continue;
                        }
                        if (depth < minDepth)
                            throw new ScanException(JelispBundle.message("unexpected.expression.end"));
                        break;
                    }
                    metPairedDelimiter = true;
                case OPEN_PARENTHESIS:
                    if (++depth == 0) {
                        count--;
                        continue;
                    }
                    break;
                case CLOSE_PARENTHESIS:
                    if (--depth == 0) {
                        count--;
                        continue;
                    }
                    if (depth < minDepth)
                        throw new ScanException(JelispBundle.message("unexpected.expression.end"));
                    break;
                case STRING_QUOTE: case GENERIC_STRING:
                    char stringTerminator = iterator.previous.character;
                    while (true) {
                        iterator.checkUnbalancedParentheses();
                        if (code == SyntaxDescriptor.ClassType.STRING_QUOTE
                                ? iterator.current.character == stringTerminator && iterator.current.syntax == SyntaxDescriptor.ClassType.STRING_QUOTE
                                : iterator.current.syntax == SyntaxDescriptor.ClassType.GENERIC_STRING)
                            break;

                        if (iterator.current.syntax == SyntaxDescriptor.ClassType.CHARACTER_QUOTE
                                || iterator.current.syntax == SyntaxDescriptor.ClassType.ESCAPE)
                            iterator.next();

                        iterator.next();
                    }
                    iterator.next();
                    if (depth == 0 && sexpFlag) {
                        count--;
                        continue;
                    }
                    break;
                default:
                    break;
            }
        }
        if (count < 0)
            throw new LispException("generic scan-forward error");
        iterator.checkUnbalancedParentheses(depth);
        return iterator.valid() ? new LispInteger(iterator.from + 1) : LispSymbol.ourNil;
    }

    private static LispObject scanBackward (Environment environment, int from) {
        int finish = environment.getBufferCurrentForEditing().pointMin();
        while (from > finish) {
            //todo
        }
        return new LispInteger(from); // or nil
    }

    //todo: compiled lisp f
    @Subroutine(value = "eval-last-sexp", isCmd = true, interactive = "P", key = "\\C-x\\C-e")
    public static void evalLastSexp (Environment environment, LispObject evalLastSexpArgInternal) {
        try {
            LispBuffer buffer = environment.getBufferCurrentForEditing();
            LispObject result = buffer.evaluateLastForm();
            if (result != null)
                GlobalEnvironment.echo(result.toString() + "\n", GlobalEnvironment.MessageType.OUTPUT);
        } catch (LispException exc) {
            GlobalEnvironment.echo(exc.getMessage() + "\n", GlobalEnvironment.MessageType.ERROR);
        }
    }

    private static boolean skipCommentForward (final SyntaxDescriptor.CommentStyle style, boolean prevValid, SyntaxIterator iterator) {
        if (iterator.nesting <= 0)
            iterator.nesting = -1;
        while (iterator.valid()) {
            if (prevValid) {
                //skip to comment end
                if (skipForwardInsideComment(iterator, style)) {
                    return true;
                }
                iterator.next();
                if (!iterator.valid()) {
                    return false;
                }
            }

            if (iterator.current.syntax == SyntaxDescriptor.ClassType.COMMENT_END
                    && iterator.current.commentStyle() == style
                    && (SyntaxDescriptor.is(SyntaxDescriptor.FlagType.PART_OF_NESTABLE_COMMENT, iterator.current.fullSyntaxCode)
                    ? iterator.nesting > 0 && --iterator.nesting == 0
                    : iterator.nesting < 0)) {
                return true;
            }
            if (iterator.current.syntax == SyntaxDescriptor.ClassType.GENERIC_COMMENT
                    && style == SyntaxDescriptor.CommentStyle.GENERIC) {
                return true;
            }
            if (iterator.nesting > 0 && iterator.current.syntax == SyntaxDescriptor.ClassType.COMMENT_START
                    && SyntaxDescriptor.is(SyntaxDescriptor.FlagType.PART_OF_NESTABLE_COMMENT, iterator.current.fullSyntaxCode)
                    && iterator.current.commentStyle() == style) {
                iterator.nesting++;
            }
            iterator.next();
            prevValid = true;
        }
        return false;
    }

    private static boolean skipForwardInsideComment(SyntaxIterator iterator, SyntaxDescriptor.CommentStyle style) {
        if (!iterator.valid())
            return false;

        if (SyntaxDescriptor.is(SyntaxDescriptor.FlagType.START_2CHAR_COMMENT_END, iterator.previous.fullSyntaxCode)
                && SyntaxDescriptor.is(SyntaxDescriptor.FlagType.END_2CHAR_COMMENT_END, iterator.current.fullSyntaxCode)
                && iterator.previous.commentStyle() == style
                && (SyntaxDescriptor.is(SyntaxDescriptor.FlagType.PART_OF_NESTABLE_COMMENT, iterator.previous.fullSyntaxCode)
                || (SyntaxDescriptor.is(SyntaxDescriptor.FlagType.PART_OF_NESTABLE_COMMENT, iterator.current.fullSyntaxCode)
                ? iterator.nesting > 0 : iterator.nesting < 0))) {
            if (--iterator.nesting <= 0) {
                return true;
            } else {
                iterator.next();
                return false;
            }
        }
        if (iterator.nesting > 0
                && SyntaxDescriptor.is(SyntaxDescriptor.FlagType.START_2CHAR_COMMENT_START, iterator.previous.fullSyntaxCode)
                && SyntaxDescriptor.is(SyntaxDescriptor.FlagType.END_2CHAR_COMMENT_START, iterator.current.fullSyntaxCode)
                && iterator.current.commentStyle() == style
                && (SyntaxDescriptor.is(SyntaxDescriptor.FlagType.PART_OF_NESTABLE_COMMENT, iterator.previous.fullSyntaxCode)
                || SyntaxDescriptor.is(SyntaxDescriptor.FlagType.PART_OF_NESTABLE_COMMENT, iterator.current.fullSyntaxCode))) {
            iterator.nesting++;
            iterator.next();
        }
        return false;
    }

    private static class SyntaxIterator {
        public int from;
        public int finish;
        public int nesting;

        private int lastGood;
        private final Environment environment;
        private final String text;
        private Descriptor current;
        private Descriptor previous;

        public SyntaxIterator (Environment e, int start, int end) {
            environment = e;
            text = environment.getBufferCurrentForEditing().getText();
            from = start - 1;
//            System.out.println(text.substring(from));
            finish = end;
            current = new Descriptor(text.charAt(from), environment);
            previous = null;
            lastGood = from;
        }

        public void next() {
            previous = current;
            from++;
            if (from == text.length())
                current = null;
            else current = new Descriptor(text.charAt(from), environment);
        }

        public boolean valid() {
            return from < finish;
        }

        public boolean atEnd() {
            return from == finish;
        }

        public void checkUnbalancedParentheses() {
            if (from >= finish) {
                throw new ScanException(JelispBundle.message("unbalanced.parentheses"), lastGood, from);
            }
        }

        public void checkUnbalancedParentheses (int depth) {
            if (depth != 0)
                throw new ScanException(JelispBundle.message("unbalanced.parentheses"), lastGood, from);
        }
    }

    private static class Descriptor {
        public final char character;
        public final int fullSyntaxCode;
        public final SyntaxDescriptor.ClassType syntax;

        Descriptor (char c, Environment environment) {
            character = c;
            fullSyntaxCode = SyntaxTable.getCharFullSyntaxCode(environment, c);
            syntax = SyntaxDescriptor.classBySyntaxCode(fullSyntaxCode);
        }

        public SyntaxDescriptor.CommentStyle commentStyle() {
            return SyntaxDescriptor.commentStyle(fullSyntaxCode);
        }
    }
}
