package org.jetbrains.emacs4ij.jelisp.elisp;

import com.intellij.openapi.util.text.StringUtil;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.InternalException;
import org.jetbrains.emacs4ij.jelisp.exception.InvalidRegexpException;
import org.jetbrains.emacs4ij.jelisp.subroutine.Match;
import org.jetbrains.emacs4ij.jelisp.subroutine.SyntaxTable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public abstract class StringRegexpUtil {
  private static List<Integer> myDataInsertions = new ArrayList<>();
  protected static String myTransformedRegex;

  private static List<Character> myCharsToRegexpQuote = Arrays.asList('*', '?', '^', '$', '+', '\\', '.', '[');
  private static Map<String, String> ourRegexReplacement = new LinkedHashMap<>();
  private static List<String> mySpecialEscaped = Arrays.asList("w", "W");
  static {
    ourRegexReplacement.put("\\\\\\\\\\|", "|");
    ourRegexReplacement.put("\\\\\\\\'", "\\\\z");
    ourRegexReplacement.put("\\\\`", "\\A");
    ourRegexReplacement.put("\\\\=", "");      //todo ?
  }

  private StringRegexpUtil () {}

  public static int match(Environment environment, final String regexp, final String data, int from, boolean isCaseFoldSearch) {
    Matcher m = find(environment, regexp, data, from, isCaseFoldSearch);
    try {
      m.start();
    } catch (IllegalStateException e) {
      Match.clearData();
      return -1;
    }
    Match.registerSearchResult(m, myDataInsertions, data.length());
    return m.start();
  }

  public static Matcher find(Environment environment, final String regexp, final String data, int from, boolean isCaseFoldSearch) {
    String transformedData = transformEmacsRegexpToJava(environment, regexp, data);
    Pattern p = isCaseFoldSearch
        ? Pattern.compile(myTransformedRegex, Pattern.MULTILINE | Pattern.CASE_INSENSITIVE)
        : Pattern.compile(myTransformedRegex, Pattern.MULTILINE);
    Matcher m = p.matcher(transformedData);
    m.find(from);
    return m;
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
      from = from < m.end() - 1 ? m.end() - 1 : m.end();
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

  @Nullable
  private static String escapeToRegexp (StringBuilder data) {
    if (data.length() == 0) return null;
    return StringUtil.escapeToRegexp(data.toString());
  }

  @Nullable
  private static String syntaxClassesStartBound (Environment environment, final String interestingChars,
                                                 SyntaxDescriptor.ClassType... type ) {
    StringBuilder sbOfType = new StringBuilder();
    StringBuilder sbNotOfType = new StringBuilder();
    analyzeSyntaxClasses(environment, interestingChars, sbOfType, sbNotOfType, type);
    String ofType  = escapeToRegexp(sbOfType);
    String notType = escapeToRegexp(sbNotOfType);
    if (StringUtil.isEmpty(ofType))
      return null; //nothing matches
    if (StringUtil.isEmpty(notType))
      return "\\A";
    return "((\\A|[" + notType + "])[" + ofType + "])";
  }

  @Nullable
  private static String syntaxClassesEndBound (Environment environment, final String interestingChars,
                                               SyntaxDescriptor.ClassType... type) {
    StringBuilder sbOfType = new StringBuilder();
    StringBuilder sbNotOfType = new StringBuilder();
    analyzeSyntaxClasses(environment, interestingChars, sbOfType, sbNotOfType, type);
    String ofType  = escapeToRegexp(sbOfType);
    String notType = escapeToRegexp(sbNotOfType);
    if (StringUtil.isEmpty(ofType))
      return null; //nothing matches
    if (StringUtil.isEmpty(notType))
      return "\\z";
    return "([" + ofType + "](\\z|[" + notType + "]))";
  }

  private static void analyzeSyntaxClasses(Environment environment, final String interestingChars,
                                           StringBuilder ofType, StringBuilder notOfType, SyntaxDescriptor.ClassType... type) {
    List<SyntaxDescriptor.ClassType> types = Arrays.asList(type);
    for (int i = 0; i < interestingChars.length(); i++) {
      char c = interestingChars.charAt(i);
      if (types.contains(SyntaxTable.getSyntaxClass(environment, c)))
        ofType.append(c);
      else notOfType.append(c);
    }
  }

  @Nullable
  private static String ofSyntaxClass(Environment environment, SyntaxDescriptor.ClassType type, final String interestingChars) {
    StringBuilder ofType = new StringBuilder();
    for (int i = 0; i < interestingChars.length(); i++) {
      char c = interestingChars.charAt(i);
      if (type == SyntaxTable.getSyntaxClass(environment, c))
        ofType.append(c);
    }
    String s = ofType.toString();
    return s.isEmpty() ? null : "[" + s + "]";
  }

  @Nullable
  private static String notOfSyntaxClass(Environment environment, SyntaxDescriptor.ClassType type, final String interestingChars) {
    StringBuilder notOfType = new StringBuilder();
    for (int i = 0; i < interestingChars.length(); i++) {
      char c = interestingChars.charAt(i);
      if (type != SyntaxTable.getSyntaxClass(environment, c))
        notOfType.append(c);
    }
    String s = notOfType.toString();
    return s.isEmpty() ? null : "[" + s + "]";
  }

  @Nullable
  private static String notSyntaxClassBound (Environment environment, SyntaxDescriptor.ClassType type, final String interestingChars) {
    StringBuilder sbOfType = new StringBuilder();
    StringBuilder sbNotOfType = new StringBuilder();
    analyzeSyntaxClasses(environment, interestingChars, sbOfType, sbNotOfType, type);
    String ofType = escapeToRegexp(sbOfType);
    String notType = escapeToRegexp(sbNotOfType);
    if (StringUtil.isEmpty(ofType) && StringUtil.isEmpty(notType))
      return null;
    if (StringUtil.isEmpty(ofType))
      return  "(([" + notType + "](\\z|[" + notType + "]))|((\\A|[" + notType + "])[" + notType + "]))";
    if (StringUtil.isEmpty(notType))
      return "([" + ofType + "][" + ofType + "])";
    return "(([" + notType + "](\\z|[" + notType + "]))|((\\A|[" + notType + "])[" + notType + "])|([" + ofType + "][" + ofType + "]))";
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
    return matcher == null
        ? data
        : replaceSpecialChar(matcher, data, key, replacement == null ? unreachable : replacement);
  }

  private static String doSyntaxTableReplacement (final String key, final String data, Environment environment,
                                                  final String content, final String unreachable) {
    String replacement = null;
    Matcher matcher = containsUnescaped(data, key);
    if (matcher != null) {
      switch (key) {
        case "w":  //word
          replacement = ofSyntaxClass(environment, SyntaxDescriptor.ClassType.WORD, content);
          break;
        case "W":  //not word
          replacement = notOfSyntaxClass(environment, SyntaxDescriptor.ClassType.WORD, content);
          break;
      }
    }
    return matcher == null
        ? data
        : replaceSpecialChar(matcher, data, key, replacement == null ? unreachable : replacement);
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

  protected static String markBounds (Environment environment, final String emacsRegexp, String data) {
    myDataInsertions.clear();

    myTransformedRegex = emacsRegexp;
    String transformedData = data;
    final String content = getCharacters(data);
    for (BoundType boundType: BoundType.getTypes()) {

      Matcher matcher = containsUnescaped(myTransformedRegex, boundType.getRepresentation().substring(1));
      if (matcher != null) {
        // transform regexp
        StringBuilder sb = new StringBuilder();
        int from = 0;
        do {
          int index = matcher.end() - boundType.getRepresentation().length();
          sb.append(myTransformedRegex.substring(from, index))
              .append(boundType.getRegexReplacement());
          from = matcher.end();
        } while (matcher.find(from));
        sb.append(myTransformedRegex.substring(from));
        myTransformedRegex = sb.toString();


        //transform data
        if (boundType == BoundType.WORD_START || boundType == BoundType.WORD_END) {
          transformedData = markBound(environment, boundType, content, transformedData);
        } else if (boundType == BoundType.WORD_BOUND) {
          transformedData = markBound(environment, BoundType.WORD_START, content, transformedData);
          transformedData = markBound(environment, BoundType.WORD_END, content, transformedData);
          if (transformedData.isEmpty()) {
            transformedData = boundType.getDataReplacement();
            addToDataInsertions(0);
          }
        } else if (boundType == BoundType.SYMBOL_START) {
//                    transformedData = markBound(environment, BoundType.WORD_START, content, transformedData);
          transformedData = markBound(environment, BoundType.SYMBOL_START, content, transformedData);
        } else if (boundType == BoundType.SYMBOL_END) {
//                    transformedData = markBound(environment, BoundType.WORD_END, content, transformedData);
          transformedData = markBound(environment, BoundType.SYMBOL_END, content, transformedData);
        } else { // NOT_WORD_BOUND
//                    transformedData = markBound(environment, BoundType.SYMBOL_START, content, transformedData);
//                    transformedData = markBound(environment, BoundType.SYMBOL_END, content, transformedData);
          transformedData = markBound(environment, boundType, content, transformedData);
        }
      }
    }
    return transformedData;
  }

  protected static String markBound (Environment environment, BoundType boundType, final String content, final String data) {
    String regex = null;
    switch (boundType.getRepresentation()) {
      case "\\<":
        regex = syntaxClassesStartBound(environment, content, SyntaxDescriptor.ClassType.WORD);
        break;
      case "\\>":
        regex = syntaxClassesEndBound(environment, content, SyntaxDescriptor.ClassType.WORD);
        break;
      case "\\_<":
        regex = syntaxClassesStartBound(environment, content, SyntaxDescriptor.ClassType.WORD, SyntaxDescriptor.ClassType.SYMBOL);
        break;
      case "\\_>":
        regex = syntaxClassesEndBound(environment, content, SyntaxDescriptor.ClassType.WORD, SyntaxDescriptor.ClassType.SYMBOL);
        break;
      case "\\B":
        regex = notSyntaxClassBound(environment, SyntaxDescriptor.ClassType.WORD, content);
        break;
    }
    if (regex == null) //no match
      return data;
    Pattern p = Pattern.compile(regex);
    Matcher m = p.matcher(data);
    StringBuilder sb = new StringBuilder();
    int from = 0;
    while (from < data.length() && m.find(from)) {
      sb.append(data.substring(from, m.start()));
      switch (m.end() - m.start()) {
        case 2:
          addToDataInsertions(m.start() + 1);
          sb.append(m.group().charAt(0)).append(boundType.getDataReplacement()).append(m.group().charAt(1));
          break;
        case 0:
          addToDataInsertions(m.start());
          sb.append(boundType.getDataReplacement());
          if (m.start() < data.length()) {
            sb.append(data.substring(m.start(), m.start() + 1));
          }
          from = m.end() + 1;
          continue;
        case 1:
          if (m.start() == 0) {
            addToDataInsertions(0);
            sb.append(boundType.getDataReplacement()).append(m.group().charAt(0));
            break;
          }
          if (m.end() == data.length()) {
            addToDataInsertions(m.end());
            sb.append(m.group().charAt(0)).append(boundType.getDataReplacement());
            break;
          }
        default:
          throw new InternalException("Regex bound replacement error");
      }
      from = m.end();
    }
    if (from < data.length())
      sb.append(data.substring(from));
    return sb.toString();
  }

  protected static void addToDataInsertions (int position) {
    int before = 0;
    for (ListIterator<Integer> iterator = myDataInsertions.listIterator(); iterator.hasNext(); ) {
      Integer index = iterator.next();
      if (index < position) {
        before++;
        continue;
      }
      iterator.set(index + 1);
    }
    myDataInsertions.add(position + before);
  }

  protected static String transformEmacsRegexpToJava (Environment environment, final String emacsRegexp, final String data) {
    String transformedData = markBounds(environment, emacsRegexp, data);
    myTransformedRegex = invertQuotation(myTransformedRegex, "(){}");
    myTransformedRegex = quoteOpeningSquareBracketInSquareBrackets(myTransformedRegex);
    for (Map.Entry<String, String> replacement: ourRegexReplacement.entrySet()) {
      myTransformedRegex = replace(myTransformedRegex, replacement.getKey(), replacement.getValue());
    }
    myTransformedRegex = doSyntaxTableReplacement(environment, myTransformedRegex, transformedData);
    myTransformedRegex = substituteBackReferences(myTransformedRegex, transformedData);
    return transformedData;
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
 * or null if none
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

  private static class BoundType {
    public static final BoundType WORD_START   = new BoundType("\\<",  "1", "1");
    public static final BoundType WORD_END     = new BoundType("\\>",  "2", "2");
    public static final BoundType SYMBOL_START = new BoundType("\\_<", "3", "3");
    public static final BoundType SYMBOL_END   = new BoundType("\\_>", "4", "4");
    public static final BoundType WORD_BOUND   = new BoundType("\\b",  "0", "[012]");
    public static final BoundType NOT_WORD_BOUND   = new BoundType("\\B", "5", "5");

    private final String myRepresentation;
    private final String myDataReplacement;
    private final String myRegexReplacement;

    private BoundType (String view, String dataReplacement, final String regexReplacement) {
      myRepresentation = view;
      myDataReplacement = dataReplacement;
      myRegexReplacement = regexReplacement;
    }

    public String getRepresentation() {
      return myRepresentation;
    }

    public String getDataReplacement() {
      return myDataReplacement;
    }

    public String getRegexReplacement() {
      return myRegexReplacement;
    }

    public static List<BoundType> getTypes() {
      return Arrays.asList(WORD_START, WORD_END, SYMBOL_START, SYMBOL_END, WORD_BOUND, NOT_WORD_BOUND);
    }
  }
}
