package org.jetbrains.emacs4ij.jelisp;

import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.text.StringUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.CyclicDefinitionLoadException;
import org.jetbrains.emacs4ij.jelisp.exception.EnvironmentException;
import org.jetbrains.emacs4ij.jelisp.exception.InternalException;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.ReadException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;
import org.jetbrains.emacs4ij.jelisp.parser.ForwardMultilineParser;
import org.jetbrains.emacs4ij.jelisp.subroutine.Predicate;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Deque;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class DefinitionLoader {
  static enum DefType {VAR, FUN} //todo: not private for test only  && IDENTIFIER
  static enum SymbolType {VAR, FUN, CMD} //todo: not private for test only  && IDENTIFIER
  protected static List<String> myDefVars = Arrays.asList("defcustom", "defvar", "defconst", "defgroup", "defface", "defvaralias");
  protected static List<String> myDefFuns = Arrays.asList("defun", "defmacro", "defsubst", "defalias", "define-derived-mode", "define-minor-mode");
  private static final Map<String, String> myUploadHistory = new LinkedHashMap<>();
  protected static DefinitionIndex myIndex = new DefinitionIndex();
  private static String myDefinitionSrcFile;
  //for test
  private static List<String> mySkipForms = new ArrayList<>();

  private static List<String> mySkipDirs = Arrays.asList("/language/", "/international/");

  public static void initialize (@Nullable DefinitionIndex index) {
    LogUtil.info("DefinitionLoader: init start");

    if (index == null) {
      index = new DefinitionIndex();
    }

    if (index.isEmpty() && TestMode.INIT_GLOBAL_ENV_FROM_EMACS_SOURCES) {
      scan(new File(GlobalEnvironment.getEmacsSource() + "/lisp/"));
      index.setWith(myIndex);
    } else {
      myIndex = index;
    }

    LogUtil.info("DefinitionLoader: init finish");
  }

  private static void scan (File file) {
    if (file.isDirectory() && notSkipped(file.getAbsolutePath())) {
      File[] files = file.listFiles();
      if (files == null)
        return;
      for (File f: files) {
        scan(f);
      }
      return;
    }
    if (file.getAbsolutePath().endsWith(".el") && notSkipped(file.getAbsolutePath())) {
      FileScanner.scan(file);
    }
  }

  private static boolean notSkipped(String path) {
    boolean ok = true;
    for (String skipDir: mySkipDirs) {
      ok &= !path.contains(skipDir);
    }
    return ok;
  }

  //for test
  public static void addSkipForms(String... forms) {
    Collections.addAll(mySkipForms, forms);
  }

  //for test
  public static void test () {}

  //for test
  public static Map<String, Long> getFileName(String name, DefType type) {
    return myIndex.get(new Identifier(name, type));
  }

  //for test skip
  public static boolean skip (String line) {
    if (mySkipForms == null)
      return false;
    for (String form: mySkipForms) {
      if (line.startsWith(form))
        return true;
    }
    return false;
  }

  private static Long getFileOffset(RandomAccessFile file, final String path) {
    try {
      return file.getFilePointer();
    } catch (IOException | NullPointerException e) {
      throw new ReadException(JelispBundle.message("invalid.file.state", path));
    }
  }

  public static void loadFile (String fileName) {
    String fullName = GlobalEnvironment.getEmacsSource() + "/lisp/" + fileName;
    RandomAccessFile reader;
    try {
      reader = new RandomAccessFile(fullName, "r");
    } catch (FileNotFoundException e) {
      throw new EnvironmentException(e.getMessage());
    }
    String line;
    ForwardMultilineParser p = new ForwardMultilineParser(reader, fullName);
    while (true){
      try {
        line = reader.readLine();
      } catch (IOException e) {
        //todo: line index
        throw new ReadException(fullName +  ", line " + 0);
      }
      if (line == null)
        break;

      boolean skip = skip(line);
      LispObject parsed = p.parse(line, getFileOffset(reader, fullName));
      if (Predicate.isNil(parsed) || skip)
        continue;

      while (true) {
        try {
          parsed.evaluate(GlobalEnvironment.INSTANCE);
        } catch (LispException e) {
          LogUtil.log(JelispBundle.message("loader.error", fullName, p.getLine(), e.getMessage()), GlobalEnvironment.MessageType.ERROR);
        }
        if (p.isFinished())
          break;
        parsed = p.parseNext();
      }
    }
  }

  public static LispList getDefFromFile(String fileName, String functionName, DefType type) {
    return FileScanner.getDefFromFile(fileName, -1, new Identifier(functionName, type));
  }

  protected static Pair<Integer, String> defStartIndex(String line, List<String> defForms) {
    Matcher m = defStart(line, null, defForms);
    if (m.find()) {
      String match = m.group();
      int length = match.length();
      char c = match.charAt(length - 1);
      int nameEnd = c == ')' || Character.isWhitespace(c) ? length - 1 : length;
      int nameReversedStart = length - nameEnd;
      String reversed = new StringBuffer(match).reverse().toString();
      for (; nameReversedStart < reversed.length(); nameReversedStart++) {
        c = reversed.charAt(nameReversedStart);
        if (Character.isWhitespace(c) || c == '\'')
          break;
      }
      String name = match.substring(length - nameReversedStart, nameEnd);
      return new Pair<>(m.start(), name);
    }
    return null;
  }

  protected static int defStartIndex(String line, @NotNull String name, List<String> defForms) {
    Matcher m = defStart(line, name, defForms);
    if (m.find()) {
      return m.start();
    }
    return -1;
  }

  private static Matcher defStart(String line, @Nullable String name, List<String> defForms) {
    StringBuilder defs = new StringBuilder();
    for (String def: defForms) {
      defs.append(def).append('|');
    }
    String defAlternative = defs.toString().substring(0, defs.length()-1);
    String nameRegex = name == null ? "[^\\s'\\(\\)]+" : StringUtil.escapeToRegexp(name);
    Pattern p = Pattern.compile("\\(\\s*(" + defAlternative + ")\\s+'?" + nameRegex + "([\\)\\s]|\\z)");
    return p.matcher(line);
  }

  private static LispSymbol processDef (LispList definition, Identifier id) {
    if (definition == null)
      return null;
    LispObject evaluated;
    try {
      evaluated = definition.evaluate(GlobalEnvironment.INSTANCE);
    } catch (LispException e) { //invalid definition
      myIndex.get(id).remove(myDefinitionSrcFile);
      return findAndRegisterEmacsForm(id.getName(), id.getType());
    }
    LispSymbol value = GlobalEnvironment.INSTANCE.find(id.getName());
    if (value == null) {
      if (evaluated instanceof LispSymbol && ((LispSymbol)evaluated).getName().equals(id.getName())) {
        GlobalEnvironment.INSTANCE.defineSymbol((LispSymbol) evaluated);
        return (LispSymbol) evaluated;
      } else {
        value = findAndRegisterEmacsForm(((LispSymbol) evaluated).getName(), id.getType());
      }
    }
    return value;
  }

  private static LispList lookInSubrFirst (Identifier id) {
    for (String filename: myIndex.get(id).keySet()) {
      if (filename.endsWith("/lisp/subr.el")) {
        return FileScanner.getDefFromFile(filename, myIndex.get(id).get(filename), id);
      }
    }
    return null;
  }

  private static LispList lookExceptSubr (Identifier id) {
    for (String filename: myIndex.get(id).keySet()) {
      if (filename.endsWith("/lisp/subr.el")) {
        continue;
      }
      LispList def = FileScanner.getDefFromFile(filename, myIndex.get(id).get(filename), id);
      if (def != null)
        return def;
    }
    return null;
  }

  private static void checkExistence (Identifier id) {
    if (!myIndex.containsKey(id)) {
//            if (!myUploadHistory.isEmpty()) {
////                Object[] entries = myUploadHistory.entrySet().toArray(new Object[myUploadHistory.size()]);
////                LogUtil.log("last upload call: " + entries[entries.length - 1] + ", search for: " + id.getName(), MessageType.ERROR);
//            }

      if (id.getType() == DefType.FUN)
        throw new VoidFunctionException(id.getName());
      throw new VoidVariableException(id.getName());
    }
  }

  private static LispSymbol findAndRegisterEmacsForm (String name, DefType type) {
    Identifier id = new Identifier(name, type);
    checkExistence(id);
    LispList definition;
    SortedMap<String, Long> map = myIndex.get(id);
    if (map.size() == 1) {
      definition = FileScanner.getDefFromFile(map.firstKey(), map.get(map.firstKey()), id);
    } else {
      definition = getDefFromInvokersSrc(id);
      if (definition == null) {
        definition = lookInSubrFirst(id);
        if (definition == null)
          definition = lookExceptSubr(id);
      }
    }
    return processDef(definition, id);
  }

  public static LispSymbol findAndRegisterEmacsFunction(String name) {
    return findAndRegisterEmacsForm(name, DefType.FUN);
  }

  public static LispSymbol findAndRegisterEmacsVariable(String name) {
    return findAndRegisterEmacsForm(name, DefType.VAR);
  }

  private static LispList getDefFromInvokersSrc(Identifier id) {
    SortedMap<String, Long> map = myIndex.get(id);
    for (Pair<String, Object> invoker: GlobalEnvironment.ourCallStack) {
      if (!myUploadHistory.containsKey(invoker.getFirst()))
        continue;
      String key = myUploadHistory.get(invoker.getFirst());
      if (map.containsKey(key)) {
        LispList def = FileScanner.getDefFromFile(myUploadHistory.get(invoker.getFirst()), map.get(key), id);
        if (def != null) {
          return def;
        }
      }
    }
    return null;
  }

  protected static class FileScanner {
    private static RandomAccessFile myFile = null;
    private static String myFilePath;
    private static Deque<Identifier> myLoadStack = new ArrayDeque<>();

    static void scan (File file) {
      try {
        myFile = new RandomAccessFile(file, "r");
      } catch (FileNotFoundException e) {
        throw new ReadException(JelispBundle.message("no.file", file.getAbsolutePath()));
      }
      myFilePath = file.getAbsolutePath();
      String line;
      while (true) {
        try {
          line = myFile.readLine();
        } catch (IOException e) {
          return;
        }
        if (line == null)
          break;
        scanLine(line);
      }
    }

    private static void closeFile() {
      if (myFile == null)
        return;
      try {
        myFile.close();
        myFile = null;
        myFilePath = null;
      } catch (IOException e) {
        e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
      }
    }

    static void checkForCyclicUploading(Identifier id) {
      for (Identifier uploading: myLoadStack) {
        if (uploading.equals(id))
          throw new CyclicDefinitionLoadException(id.toString());
      }
    }

    static void onUploadFinish (Identifier id) {
      try {
        Identifier first = myLoadStack.removeFirst();
        if (!id.equals(first)) {
          LogUtil.log(String.format("Load stack error: top of stack = %s, current id = %s, left stack = %s", first.toString(), id.toString(),
              myLoadStack.toString()), GlobalEnvironment.MessageType.ERROR);
        }
      } catch (NoSuchElementException e) {
        LogUtil.log(String.format("Load stack error: %s, current id = %s, left stack = %s", e.getMessage(), id.toString(),
            myLoadStack.toString()), GlobalEnvironment.MessageType.ERROR);
      }
    }

    static LispList getDefFromFile (final String fileName, long offset, Identifier id) {
      checkForCyclicUploading(id);
      myLoadStack.push(id);
      try {
        myFile = new RandomAccessFile(fileName, "r");
        myFilePath = fileName;

        if (offset == -1) {
          SortedMap<String, Long> map = myIndex.get(id);
          if (map != null && map.containsKey(myFilePath)) {
            offset = map.get(myFilePath);
          } else {
            //we don't know offset
            while (true) {
              LispList def = tryGetDefFromFile(id);
              if (def != null) return def;
            }
          }
        }

        try {
          myFile.seek(offset);
        } catch (IOException e) {
          throw new ReadException(JelispBundle.message("invalid.offset", myFilePath, offset));
        }

        LispList def = tryGetDefFromFile(id);
        if (def == null) {
          myDefinitionSrcFile = null;
          LogUtil.log("NULL def: " + id.toString() + ", file " + myFilePath, GlobalEnvironment.MessageType.ERROR);
          return null;
        } else {
          return def;
        }

      } catch (NullLineException e0) {
        return null;
      } catch (FileNotFoundException e1) {
        throw new ReadException(JelispBundle.message("no.file", fileName));
      } catch (IOException e2) {
        throw new ReadException(myFilePath);
      } finally {
        closeFile();
        onUploadFinish(id);
      }
    }

    private static LispList tryGetDefFromFile (Identifier id) throws IOException {
      String line = myFile.readLine();
      if (line == null) throw new NullLineException();
      int defStart = defStartIndex(line, id.getName(), (id.getType() == DefType.FUN ? myDefFuns : myDefVars));
      if (defStart != -1) {
        myDefinitionSrcFile = myFilePath;
        return getDef(line, defStart, id.getName());
      } else {
        return null;
      }
    }

    private static LispList getDef(String line, int index, String name) {
      ForwardMultilineParser p = new ForwardMultilineParser(myFile, myFilePath);
      LispObject parsed = p.parse(line, getFileOffset(myFile, myFilePath), index);
      if (parsed instanceof LispList) {
        myUploadHistory.put(name, myFilePath);
        return (LispList) parsed;
      }
      throw new InternalException(JelispBundle.message("unexpected.object.type"));
    }

    private static void scanLine(String line) {
      if (line.trim().startsWith(";"))
        return;
      if (line.isEmpty())
        return;
      long baseOffset = getFileOffset(myFile, myFilePath) - line.length() - 1;
//            if (line.getBytes().length != line.length())
//                System.out.print(1);
      scanLine(DefType.FUN, line, baseOffset);
      scanLine(DefType.VAR, line, baseOffset);
    }

    protected static void scanLine (DefType type, String line, long baseOffset) {
      List<String> defs = type == DefType.FUN ? myDefFuns : myDefVars;
      String substring = line;
      while (!substring.isEmpty()) {
        Pair<Integer, String> start = defStartIndex(substring, defs);
        if (start == null)
          return;
        Identifier id = new Identifier(start.getSecond(), type);
        saveId(id, baseOffset + start.getFirst());
        substring = substring.substring(start.getFirst() + 1);
        baseOffset += start.getFirst() + 1;
      }
    }

    private static void saveId (Identifier id, long offset) {
      //todo: back
      //reader == myFile
      /*Identifier id = null;
if (type == DefType.FUN) {//check if it is command
  try {
      LispList definition = getDef(reader, line, name);
      if (BuiltinPredicates.commandp(definition, null).equals(LispSymbol.T))
          id = new Identifier(name, SymbolType.CMD);
  } catch (ParserException e) {
      if (!e.getMessage().contains("Unknown code block: )")) //this means we've found def inside other def
          if (!e.getMessage().contains("\u001B") && !e.getMessage().contains("�")) {
//                LogUtil.log("Function " + name + ": " + e.getMessage(), MessageType.ERROR);
          } else {
//                LogUtil.log(myFile.getAbsolutePath(), MessageType.ERROR);
          }
      //skip here
  }
}
if (id == null)
  id = new Identifier(name, type);*/
      if (myIndex.containsKey(id)) {
        SortedMap<String, Long> map = myIndex.get(id);
        if (!map.containsKey(myFilePath))
          map.put(myFilePath, offset);
        return;
      }
      TreeMap<String, Long> map = new TreeMap<>(FileNamesComparator.INSTANCE);
      map.put(myFilePath, offset);
      myIndex.put(id, map);
    }
  }

  //prefer root sources among all others
  private static class FileNamesComparator implements Comparator<String> {
    private static FileNamesComparator INSTANCE = new FileNamesComparator();

    private static int countFileSeparators (String s) {
      int counter = 0;
      for (int i = 0; i < s.length(); i++) {
        if (s.charAt(i) == File.separatorChar)
          counter++;
      }
      return counter;
    }

    @Override
    public int compare(String o1, String o2) {
      if (o1.equals(o2))
        return 0;
      int n1 = countFileSeparators(o1);
      int n2 = countFileSeparators(o2);
      return n1 < n2
          ? -1
          : n1 == n2 ? o1.compareTo(o2) : 1;
    }
  }

  private static class NullLineException extends RuntimeException {
    public NullLineException() {
      super();
    }
  }
}
