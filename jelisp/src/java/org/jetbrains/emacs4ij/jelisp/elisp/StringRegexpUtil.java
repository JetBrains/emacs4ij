package org.jetbrains.emacs4ij.jelisp.elisp;

import com.intellij.openapi.util.text.StringUtil;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.InvalidRegexpException;
import org.jetbrains.emacs4ij.jelisp.subroutine.Match;
import org.jetbrains.emacs4ij.jelisp.subroutine.SyntaxTable;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 5/2/12
 * Time: 5:28 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class StringRegexpUtil {
    private static List<Character> myCharsToRegexpQuote = Arrays.asList('*', '?', '^', '$', '+', '\\', '.', '[');
    private static Map<String, String> ourRegexReplacement = new LinkedHashMap<>();
    private static List<String> mySpecialEscaped = Arrays.asList("b", "B", "<", ">", "w", "W", "_<", "_>");
    static {
        ourRegexReplacement.put("\\\\\\\\\\|", "|");
        ourRegexReplacement.put("\\\\\\\\'", "\\\\z");
        ourRegexReplacement.put("\\\\`", "\\A");
        ourRegexReplacement.put("\\\\=", "");      //todo ?
    }

    private StringRegexpUtil () {}

    public static int match (Environment environment, final String regexp, final String data, int from, boolean isCaseFoldSearch) {
        String s = transformEmacsRegexpToJava(environment, regexp, data);
        Pattern p = isCaseFoldSearch
                ? Pattern.compile(s, Pattern.MULTILINE | Pattern.CASE_INSENSITIVE)
                : Pattern.compile(s, Pattern.MULTILINE);
        Matcher m = p.matcher(data);
        m.find(from);
        try {
            m.start();
        } catch (IllegalStateException e) {
            Match.clearHistory();
            return -1;
        }
        Match.registerSearchResult(m);
        return m.start();
    }

    private static String replace (final String where, final String whatRegexp, final String with) {
        Pattern p = Pattern.compile(whatRegexp);
        Matcher m = p.matcher(where);
        return m.replaceAll(with);
    }

    /**
     * for each c from given string
     * quote c when is is unquoted and unquote when quoted
     */
    protected static String invertQuotation (final String source, String toInvert) {
        List<Integer> unquotedPlace = new ArrayList<>();
        // find unquoted
        Pattern p = Pattern.compile("([^\\\\]|^)(\\\\(\\\\\\\\)*)?[" + toInvert + "]");
        Matcher m = p.matcher(source);
        int from = 0;
        while (m.find(from)) {
            unquotedPlace.add(m.end() - 1);
            from = m.end() - 1;
        }
        // unquote quoted
        p = Pattern.compile("(\\\\\\\\)+[" + toInvert + "]");
        m = p.matcher(source);
        from = 0;
        StringBuilder result = new StringBuilder();
        while (m.find(from)) {
            result.append(source.substring(from, m.start()));
            if (m.end() - m.start() - 3 > 0)
                result.append(source.substring(m.start(), m.end() - 3));
            result.append(source.charAt(m.end()-1));
            from = m.end();
            //shift indexes of unquoted c
            for (ListIterator<Integer> iterator = unquotedPlace.listIterator(); iterator.hasNext(); ) {
                Integer index = iterator.next();
                if (index < m.start())
                    continue;
                iterator.set(index - 2);
            }
        }
        result.append(source.substring(from));
        //quote unquoted
        for (int i = 0, unquotedPlaceSize = unquotedPlace.size(); i < unquotedPlaceSize; i++) {
            Integer index = unquotedPlace.get(i);
            result.insert(index + i, "\\");
        }
        return result.toString();
    }

    protected static String quoteOpeningSquareBracketInSquareBrackets(final String regexp) {
        Pattern p = Pattern.compile("\\[[^\\]\\[]*\\[[^\\]]*]");
        Matcher m = p.matcher(regexp);
        StringBuilder sb = new StringBuilder();
        int from = 0;
        while (m.find()) {
            String match = "[" + m.group().substring(1).replaceAll("\\[", "\\\\[");
            sb.append(regexp.substring(from, m.start())).append(match);
            from = m.end();
        }
        sb.append(regexp.substring(from));
        return sb.toString();
    }

    private static String escapeToRegexp (StringBuilder data) {
        if (data.length() == 0)
            return "";
        return StringUtil.escapeToRegexp(data.toString());
    }

    private static String syntaxClassBound (Environment environment, SyntaxDescriptor.ClassType type, final String interestingChars) {
        StringBuilder sbOfType = new StringBuilder();
        StringBuilder sbNotOfType = new StringBuilder();
        analyzeSyntaxClasses(environment, type, interestingChars, sbOfType, sbNotOfType);
        String ofType  = escapeToRegexp(sbOfType);
        String notType = escapeToRegexp(sbNotOfType);
        if (StringUtil.isEmpty(ofType) && StringUtil.isEmpty(notType))
            return "\\\\A\\\\z";
        if (StringUtil.isEmpty(ofType))
            return "\\\\A";
        if (StringUtil.isEmpty(notType))
            return "\\\\A";
        return  "(([" + ofType + "](\\\\z|[" + notType + "]))|((\\\\A|[" + notType + "])[" + ofType + "])|(\\\\A\\\\z))";
    }

    private static String syntaxClassStartBound (Environment environment, SyntaxDescriptor.ClassType type, final String interestingChars) {
        StringBuilder sbOfType = new StringBuilder();
        StringBuilder sbNotOfType = new StringBuilder();
        analyzeSyntaxClasses(environment, type, interestingChars, sbOfType, sbNotOfType);
        String ofType  = escapeToRegexp(sbOfType);
        String notType = escapeToRegexp(sbNotOfType);
        if (StringUtil.isEmpty(ofType))
            return ""; //nothing matches
        if (StringUtil.isEmpty(notType))
            return "\\\\A";
        return "((\\\\A|[" + notType + "])[" + ofType + "])";
    }

    private static String syntaxClassEndBound (Environment environment, SyntaxDescriptor.ClassType type, final String interestingChars) {
        StringBuilder sbOfType = new StringBuilder();
        StringBuilder sbNotOfType = new StringBuilder();
        analyzeSyntaxClasses(environment, type, interestingChars, sbOfType, sbNotOfType);
        String ofType  = escapeToRegexp(sbOfType);
        String notType = escapeToRegexp(sbNotOfType);
        if (StringUtil.isEmpty(ofType))
            return ""; //nothing matches
        if (StringUtil.isEmpty(notType))
            return "\\\\z)";
        return "([" + ofType + "](\\\\z|[" + notType + "]))";
    }

    private static String notSyntaxClassBound (Environment environment, SyntaxDescriptor.ClassType type, final String interestingChars) {
        String s = syntaxClassBound(environment, type, interestingChars);
        return s.isEmpty() ? s : "[^" + s + "]";
    }

    private static void analyzeSyntaxClasses(Environment environment, SyntaxDescriptor.ClassType type,
                                             final String interestingChars, StringBuilder ofType, StringBuilder notOfType) {
        for (int i = 0; i < interestingChars.length(); i++) {
            char c = interestingChars.charAt(i);
            if (type == SyntaxTable.getSyntaxClass(environment, c))
                ofType.append(c);
            else notOfType.append(c);
        }
    }

    private static String ofSyntaxClass(Environment environment, SyntaxDescriptor.ClassType type, final String interestingChars) {
        StringBuilder ofType = new StringBuilder();
        for (int i = 0; i < interestingChars.length(); i++) {
            char c = interestingChars.charAt(i);
            if (type == SyntaxTable.getSyntaxClass(environment, c))
                ofType.append(c);
        }
        String s = ofType.toString();
        return s.isEmpty() ? s : "[" + s + "]";
    }

    private static String notOfSyntaxClass(Environment environment, SyntaxDescriptor.ClassType type, final String interestingChars) {
        StringBuilder notOfType = new StringBuilder();
        for (int i = 0; i < interestingChars.length(); i++) {
            char c = interestingChars.charAt(i);
            if (type != SyntaxTable.getSyntaxClass(environment, c))
                notOfType.append(c);
        }
        String s = notOfType.toString();
        return s.isEmpty() ? s : "[" + s + "]";
    }

    private static String getCharacters (final String data) {
        String chars = "";
        for (int i = 0; i < data.length(); ++i) {
            char c = data.charAt(i);
            if (!chars.contains(Character.toString(c)))
                chars += c;
        }
        return chars;
    }

    private static Matcher containsUnescaped (final String data, final String what) {
//                    Pattern.compile("([^\\\\]|^)\\\\(\\\\\\\\)*?a");
        Pattern p = Pattern.compile("([^\\\\]|^)\\\\(\\\\\\\\)*?" + StringUtil.escapeToRegexp(what));
        Matcher m = p.matcher(data);
        m.find();
        try {
            m.start();
            return m;
        } catch (IllegalStateException e) {
            return null;
        }
    }

    private static String replaceSpecialChar(Matcher m, final String data, final String key, final String replacement) {
        int from = 0;
        StringBuilder sb = new StringBuilder();
        do {
            sb.append(data.substring(from, m.start()));
            if (m.end() - m.start() - key.length() - 1 > 0)
                sb.append(StringUtil.escapeToRegexp(data.substring(m.start(), m.end() - key.length() - 1)));
            sb.append(replacement);
            from = m.end();
        } while (m.find(from));
        return sb.toString();
    }

    private static String doSyntaxTableReplacement (final String key, final String data, Environment environment,
                                                    final String content, final String unreachable,
                                                    @Nullable SyntaxDescriptor.ClassType type) {
        String replacement = null;
        Matcher matcher = containsUnescaped(data, key);
        if (matcher != null) {
            if (key.startsWith("s"))
                replacement = ofSyntaxClass(environment, type, content);
            else if (key.startsWith("S"))
                replacement = notOfSyntaxClass(environment, type, content);
        }
        return replacement == null
                ? data
                : replaceSpecialChar(matcher, data, key, replacement.isEmpty() ? unreachable : replacement);
    }

    private static String doSyntaxTableReplacement (final String key, final String data, Environment environment,
                                                  final String content, final String unreachable) {
        String replacement = null;
        Matcher matcher = containsUnescaped(data, key);
        if (matcher != null) {
            switch (key) {
                case "b":  //word bound
                    // todo: If in buffer, matches buffer start & end regardless of neighbour char syntax class
                    replacement = syntaxClassBound(environment, SyntaxDescriptor.ClassType.WORD, content);
                    break;
                case "\\B":  //not word bound
                    replacement = notSyntaxClassBound(environment, SyntaxDescriptor.ClassType.WORD, content);
                    break;
                case "\\<":  //strict word start bound (at buffer too)
                    replacement = syntaxClassStartBound(environment, SyntaxDescriptor.ClassType.WORD, content);
                    break;
                case "\\>":  //strict word end bound (at buffer too)
                    replacement = syntaxClassEndBound(environment, SyntaxDescriptor.ClassType.WORD, content);
                    break;
                case "w":  //word
                    replacement = ofSyntaxClass(environment, SyntaxDescriptor.ClassType.WORD, content);
                    break;
                case "\\W":  //not word
                    replacement = notOfSyntaxClass(environment, SyntaxDescriptor.ClassType.WORD, content);
                    break;
                case "\\_<":
                    replacement =  "(" + syntaxClassStartBound(environment, SyntaxDescriptor.ClassType.WORD, content) + "|" +
                            syntaxClassStartBound(environment, SyntaxDescriptor.ClassType.SYMBOL, content) + ")";
                    break;
                case "\\_>":
                    replacement =  "(" + syntaxClassEndBound(environment, SyntaxDescriptor.ClassType.WORD, content) + "|" +
                            syntaxClassEndBound(environment, SyntaxDescriptor.ClassType.SYMBOL, content) + ")";
                    break;
            }
        }
        return replacement == null
                ? data
                : replaceSpecialChar(matcher, data, key, replacement.isEmpty() ? unreachable : replacement);
    }


    private static String doSyntaxTableReplacement(Environment environment, final String transformed, final String data) {
        String content = getCharacters(data);
        final String unreachable = "a" + data;
        String result = transformed;
        for (String special: mySpecialEscaped) {
            result = doSyntaxTableReplacement(special, result, environment, content, unreachable);
        }
        for (SyntaxDescriptor.ClassType type: SyntaxDescriptor.ClassType.values()) {
            char syntaxClassChar = SyntaxDescriptor.getSyntaxClassChar(type);
            result = doSyntaxTableReplacement("s" + syntaxClassChar, result, environment, content, unreachable, type);
            result = doSyntaxTableReplacement("S" + syntaxClassChar, result, environment, content, unreachable, type);
        }
        //todo: categories: \cc - any character that belongs to the category c, \Cc - not c category
        return result;
    }

    /*
    if (transformed.contains("\\b")) { //word bound
            // todo: If in buffer, matches buffer start & end regardless of neighbour char syntax class
            putReplacement(replacementMap, "\\\\\\\\b", syntaxClassBound(environment, SyntaxDescriptor.ClassType.WORD, content), unreachable);
        }
        if (transformed.contains("\\B")) { //not word bound
            putReplacement(replacementMap, "\\\\\\\\B", notSyntaxClassBound(environment, SyntaxDescriptor.ClassType.WORD, content), unreachable);
        }
        if (transformed.contains("\\<")) { //strict word start bound (at buffer too)
            putReplacement(replacementMap, "\\<", syntaxClassStartBound(environment, SyntaxDescriptor.ClassType.WORD, content), unreachable);
        }
        if (transformed.contains("\\>")) { //strict word end bound (at buffer too)
            putReplacement(replacementMap, "\\>", syntaxClassEndBound(environment, SyntaxDescriptor.ClassType.WORD, content), unreachable);
        }
        if (transformed.contains("\\w")) { //word
            putReplacement(replacementMap, "\\\\\\\\w", ofSyntaxClass(environment, SyntaxDescriptor.ClassType.WORD, content), unreachable);
        }
        if (transformed.contains("\\W")) { //not word
            putReplacement(replacementMap, "\\\\\\\\W", notOfSyntaxClass(environment, SyntaxDescriptor.ClassType.WORD, content), unreachable);
        }
        if (transformed.contains("\\_<")) { //strict symbol start bound (at buffer too). Symbol syntax class = {‘w’ or ‘_’}
            String s = "(" + syntaxClassStartBound(environment, SyntaxDescriptor.ClassType.WORD, content) + "|" +
                    syntaxClassStartBound(environment, SyntaxDescriptor.ClassType.SYMBOL, content) + ")";
            putReplacement(replacementMap, "\\_<", s, unreachable);
        }
        if (transformed.contains("\\_>")) { //strict symbol end bound (at buffer too)
            String s = "(" + syntaxClassEndBound(environment, SyntaxDescriptor.ClassType.WORD, content) + "|" +
                    syntaxClassEndBound(environment, SyntaxDescriptor.ClassType.SYMBOL, content) + ")";
            putReplacement(replacementMap, "\\_>", s, unreachable);
        }

        for (SyntaxDescriptor.ClassType type: SyntaxDescriptor.ClassType.values()) {
            char syntaxClassChar = SyntaxDescriptor.getSyntaxClassChar(type);
            if (transformed.contains("\\s" + syntaxClassChar)) { //chars of syntax class 'type'
                putReplacement(replacementMap, "\\\\\\\\s" + syntaxClassChar, ofSyntaxClass(environment, type, content), unreachable);
            }
            if (transformed.contains("\\S" + syntaxClassChar)) { //chars of syntax class !'type'
                putReplacement(replacementMap, "\\\\\\\\S" + syntaxClassChar, notOfSyntaxClass(environment, type, content), unreachable);
            }
        }
     */

    protected static String transformEmacsRegexpToJava (Environment environment, final String emacsRegexp, final String data) {
        String transformed = invertQuotation(emacsRegexp, "(){}");
        transformed = quoteOpeningSquareBracketInSquareBrackets(transformed);
        for (Map.Entry<String, String> replacement: ourRegexReplacement.entrySet()) {
            transformed = replace(transformed, replacement.getKey(), replacement.getValue());
        }
        transformed = doSyntaxTableReplacement(environment, transformed, data);
//        for (Map.Entry<String, String> replacement: doSyntaxTableReplacement(environment, transformed, data).entrySet()) {
//            transformed = replace(transformed, replacement.getKey(), replacement.getValue());
//        }
        System.out.println(transformed);
        return substituteBackReferences(transformed, data);
    }

    private static String substituteBackReferences (final String regexp, final String data) {
        Pattern p = Pattern.compile("\\\\\\\\\\d");
        Matcher backRefMatcher = p.matcher(regexp);
        int from = 0;
        StringBuilder result = new StringBuilder();
        while (backRefMatcher.find(from)) {
            int n = regexp.charAt(backRefMatcher.end() - 1) - '0';
            if (n == 0) {
                result.append(regexp.substring(from, backRefMatcher.end()));
            } else {
                String match = getNthMatchAlternative(regexp, n, backRefMatcher.start(), data);
                result.append(regexp.substring(from, backRefMatcher.start())).append(match);
            }
            from = backRefMatcher.end();
        }
        result.append(regexp.substring(from));
        return result.toString();
    }

    /*
    * returns the matched string of nth group for last alternative in ()
    * or null id none
     */
    protected static String getNthMatchAlternative (String regexp, int n, int end, final String data) {
        String reviewed = regexp.substring(0, end);

        int alternativeEnd = 0;
        int alternativeStart = reviewed.length();
        while (true) {
            Pattern unquotedClosingBracket = Pattern.compile("([^\\\\]|^)\\)");
            Matcher m = unquotedClosingBracket.matcher(reviewed);
            while (m.find(alternativeEnd)) {
                if (m.end() >= alternativeStart)
                    break;
                alternativeEnd = m.end();
            }
            if (alternativeEnd == 0 || reviewed.charAt(alternativeEnd - 1) != ')')
                throw new InvalidRegexpException(InvalidRegexpException.Error.BACK_REFERENCE);

            Pattern unquotedOpeningBracket = Pattern.compile("([^\\\\]|^)\\(");
            m = unquotedOpeningBracket.matcher(reviewed);
            alternativeStart = 0;
            while (m.find(alternativeStart)) {
                if (m.end() >= alternativeEnd)
                    break;
                alternativeStart = m.end();
            }
            alternativeStart--;
            if (alternativeStart == -1 || reviewed.charAt(alternativeStart) != '(')
                throw new InvalidRegexpException(InvalidRegexpException.Error.BACK_REFERENCE);

            if (alternativeStart + 2 < alternativeEnd
                    && reviewed.charAt(alternativeStart + 1) == '?' && reviewed.charAt(alternativeStart + 1) == ':') {
                alternativeEnd = 0;
                continue;
            }
            break;
        }

        String alternative = reviewed.substring(alternativeStart, alternativeEnd);
        Pattern p = Pattern.compile(alternative);
        Matcher m = p.matcher(data);
        m.find();
        try {
            return m.group(n);
        } catch (IllegalStateException | IndexOutOfBoundsException e) {
            throw new InvalidRegexpException(InvalidRegexpException.Error.BACK_REFERENCE);
        }
    }

    public static String getExactRegexp (final String data) {
        StringBuilder regexp = new StringBuilder();
        for (int i = 0; i < data.length(); i++) {
            char c = data.charAt(i);
            if (myCharsToRegexpQuote.contains(c))
                regexp.append("\\\\");
            regexp.append(c);
        }
        return regexp.toString();
    }
}
