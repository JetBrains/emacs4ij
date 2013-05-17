package org.jetbrains.emacs4ij.jelisp.elisp;

import com.intellij.openapi.actionSystem.KeyboardShortcut;
import com.intellij.openapi.actionSystem.Shortcut;
import com.intellij.openapi.util.text.StringUtil;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.parser.ForwardParser;
import org.jetbrains.emacs4ij.jelisp.parser.exception.ParserException;

import javax.swing.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

abstract class ShortcutStringUtil {
  private ShortcutStringUtil() {}

  private static final List<String> myModifiers = Arrays.asList("meta", "ctrl", "shift", "alt");
  private static final List<String> myPunctuation = Arrays.asList("SPACE", "ESCAPE", "MINUS", "ENTER", "TAB", "SLASH");
  private static final List<String> myFunctional;
  private static final Map<String, String> myReplaceMap;
  static {
    myReplaceMap = new LinkedHashMap<>();
    myReplaceMap.put(" ", " SPACE ");
    myReplaceMap.put("<SPC>", " SPACE ");
    myReplaceMap.put("<ESC>", " ESCAPE ");
    myReplaceMap.put("\\\\e", " ESCAPE ");
    myReplaceMap.put("<RET>", " ENTER ");
    myReplaceMap.put("<TAB>", " TAB ");
    myReplaceMap.put("\\^", " ctrl ");
    if (System.getProperty("os.name").toLowerCase().startsWith("mac os")) {
      myReplaceMap.put(regModifier('M'), " meta ");
    } else {
      myReplaceMap.put(regModifier('M'), " alt ");
    }
    myReplaceMap.put(regModifier('C'), " ctrl ");
    myReplaceMap.put(regModifier('S'), " shift ");
    myReplaceMap.put(regModifier('A'), " alt ");
    myReplaceMap.put("\\?", " shift SLASH ");

    myFunctional = new ArrayList<>();
    for (int i = 1; i < 13; ++i) {
      myFunctional.add("F"+i);
      myReplaceMap.put(regFunctional(i), " F" + i + " ");
    }

    myReplaceMap.put("-", " MINUS ");
  }

  protected static String toShortcutString (LispString string) {
    String data = string.getData();
    for (Map.Entry<String, String> entry: myReplaceMap.entrySet()) {
      data = data.replaceAll(entry.getKey(), entry.getValue());
    }
    String[] split = data.trim().split(" ");
    StringBuilder sb = new StringBuilder();
    for (String item : split) {
      if (item.length() > 0 && item.charAt(0) == '\\') {
        try {
          LispObject character = new ForwardParser().parseLine('?' + item);
          if (character instanceof LispInteger) {
            sb.append(character.toString()).append(' ');
            continue;
          }
        } catch (ParserException e) {
          //skip
        }
      }
      if (!myModifiers.contains(item) && !myPunctuation.contains(item) && !myFunctional.contains(item)) {
        for (int i = 0; i < item.length(); i++)
          sb.append(Character.toUpperCase(item.charAt(i))).append(' ');
        continue;
      }
      sb.append(item).append(' ');
    }
    return sb.toString().trim();
  }

  @Nullable
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
        String keyStroke = keystrokeBuilder.toString().trim();
        try {
          int c = Integer.parseInt(keyStroke);
          keystrokes.add(new KeyboardShortcut(KeyStroke.getKeyStroke((char) c), null));
        } catch (NumberFormatException e) {
          try {
            keystrokes.add(KeyboardShortcut.fromString(keyStroke));
          } catch (IllegalArgumentException | AssertionError e2) {
            return null;
          }
        }
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
