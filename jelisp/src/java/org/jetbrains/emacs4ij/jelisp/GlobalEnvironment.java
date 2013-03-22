package org.jetbrains.emacs4ij.jelisp;

import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.text.StringUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSubroutine;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.elisp.LispVector;
import org.jetbrains.emacs4ij.jelisp.elisp.Primitive;
import org.jetbrains.emacs4ij.jelisp.exception.EnvironmentException;
import org.jetbrains.emacs4ij.jelisp.exception.InternalException;
import org.jetbrains.emacs4ij.jelisp.platformDependent.Ide;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispBufferFactory;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispKeymap;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispKeymapFactory;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispWindowFactory;
import org.jetbrains.emacs4ij.jelisp.subroutine.Core;
import org.jetbrains.emacs4ij.jelisp.subroutine.Key;
import org.jetbrains.emacs4ij.jelisp.subroutine.Match;
import org.jetbrains.emacs4ij.jelisp.subroutine.Predicate;
import org.jetbrains.emacs4ij.jelisp.subroutine.Subroutine;

import java.io.File;
import java.io.FilenameFilter;
import java.lang.reflect.Method;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Deque;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

public class GlobalEnvironment extends Environment {
  public static enum MessageType {INFO, WARNING, ERROR}

  private static final Logger LOG = Logger.getInstance("#org.jetbrains.emacs4ij.jelisp.GlobalEnvironment");

  private static String ourEmacsHome = "";
  private static String ourEmacsSource = "";
  private static boolean isEmacsSourceOk = false;
  private static boolean isEmacsHomeOk = false;
  private final Ide myIde;
  private DocumentationExtractor myDocumentationExtractor;
  private List<String> myBufferLocals = new ArrayList<>();
  private boolean isLoading = false;

  //for debug & extract definition on the fly
  public static Deque<Pair<String, Object>> ourCallStack = new ArrayDeque<>();

  //temporary solution while i'm not loading all sources
  private static List<String> myFilesToLoad = Arrays.asList("emacs-lisp/backquote.el", "jit-lock.el", "emacs-lisp/timer.el",
      "font-core.el", "font-lock.el"
//      , "help.el", "faces.el"
  );

  public static GlobalEnvironment INSTANCE = null;

  public static String getEmacsSource() {
    return ourEmacsSource;
  }

  public static void setEmacsHome(String emacsPath) {
    ourEmacsHome = emacsPath;
    isEmacsHomeOk = false;
  }

  public static void setEmacsSource(String emacsSource) {
    ourEmacsSource = emacsSource;
    isEmacsSourceOk = false;
  }

  public enum PropertyType {HOME, SOURCE}

  public static boolean isEmacsPropertyOk (PropertyType type) {
    switch (type) {
      case HOME:
        return isEmacsHomeOk;
      case SOURCE:
        return isEmacsSourceOk;
      default:
        return false;
    }
  }

  private static boolean testProperty (PropertyType type) {
    if (isEmacsPropertyOk(type))
      return true;
    switch (type) {
      case HOME:
        isEmacsHomeOk = testProperty(type, ourEmacsHome);
        return isEmacsHomeOk;
      case SOURCE:
        isEmacsSourceOk = testProperty(type, ourEmacsSource);
        return isEmacsSourceOk;
      default:
        return false;
    }
  }

  public static boolean testProperty (PropertyType type, String value) {
    switch (type) {
      case HOME:
        boolean isOk = false;
        String docDir = value + "/etc/";
        File file = new File (docDir);
        if (file.exists() && file.isDirectory()) {
          String[] docs = file.list(new FilenameFilter() {
            @Override
            public boolean accept(File file, String s) {
              return s.startsWith("DOC-");
            }
          });
          isOk =  docs != null && docs.length == 1;
        }
        return isOk;
      case SOURCE:
        return new File(value + "/lisp/simple.el").exists();
      default:
        return false;
    }
  }

  public static void initialize (@Nullable LispKeymapFactory keymapFactory, @Nullable LispBufferFactory bufferFactory,
                                 @Nullable LispWindowFactory windowFactory,
                                 @NotNull FrameManager frameManager,
                                 @Nullable Ide ide, @Nullable Runnable initMinibuffer) {
    LogUtil.info(LOG, "GLOBAL ENVIRONMENT INIT STARTED");
    ourKeymapManager = new EmacsKeymapManager(keymapFactory);
    ourBufferManager = new BufferManager(bufferFactory);
    ourWindowManager = new WindowManager(windowFactory);
    ourFrameManager = frameManager;
    INSTANCE = new GlobalEnvironment(ide);

    if (initMinibuffer != null) {
      initMinibuffer.run();
    }

    Key.init();
    INSTANCE.init();
    LogUtil.info(LOG, "GLOBAL ENVIRONMENT INIT FINISHED");
  }

  //input parameters are nullable only for test!!!
  public static void initialize (@Nullable LispKeymapFactory keymapFactory, @Nullable LispBufferFactory bufferFactory,
                                 @Nullable LispWindowFactory windowFactory, @Nullable Ide ide, @Nullable Runnable initMinibuffer) {
    initialize(keymapFactory, bufferFactory, windowFactory, new FrameManagerImpl(), ide, initMinibuffer);
  }

  private void init() {
    isLoading = true;
    setConstants();
    defineBufferLocalVariables();
    defineGlobalVariables();
    defineUserOptions();
    setSubroutines();

    if (TestMode.INIT_GLOBAL_ENV_FROM_EMACS_SOURCES) {
      //note: it's important to load backquote before defsubst
      DefinitionLoader.loadEmacsFile(myFilesToLoad.get(0));
      defineDefForms();

      if (TestMode.LOAD_FILES) {
        LogUtil.info(LOG, "start load files");
        for (int i = 1; i < myFilesToLoad.size(); ++i) {
          String fileName = myFilesToLoad.get(i);
          LogUtil.info(LOG, "load " + fileName);
          DefinitionLoader.loadEmacsFile(fileName);
        }
        LogUtil.info(LOG, "finish load files");
      }
    }
    isLoading = false;
  }

  private GlobalEnvironment (Ide ide) {
    myIde = ide;
    if (!testProperty(PropertyType.SOURCE)) {
      mySymbols.clear();
      throw new EnvironmentException(JelispBundle.message("invalid.emacs.dir", "source"));
    }
    myDocumentationExtractor = new DocumentationExtractor(ourEmacsSource + "/src");
    myDocumentationExtractor.scanAll();
    if (!testProperty(PropertyType.HOME)) {
      mySymbols.clear();
      throw new EnvironmentException(JelispBundle.message("invalid.emacs.dir", "home"));
    }
  }

  public LispSymbol defineSymbol (String name, @Nullable LispObject value) {
    LispSymbol symbol = new LispSymbol(name, Core.thisOrNil(value));
    defineSymbol(symbol);
    return symbol;
  }

  public LispSymbol defineConstant (String name, @Nullable LispObject value) {
    LispSymbol symbol = new LispSymbol(true, name, Core.thisOrNil(value));
    defineSymbol(symbol);
    return symbol;
  }

  public void defineSymbol (String name) {
    defineSymbol(new LispSymbol(name, LispSymbol.NIL));
  }

  @Override
  public void defineSymbol (LispSymbol symbol) {
    setGlobalVariableDocumentation(symbol);
    if (!isLoading && isRecording && !myRecordedSymbols.contains(symbol.getName())) {
      myRecordedSymbols.add(symbol.getName());
    }
    putSymbol(symbol);
  }

  public void defineSymbols(LispSymbol... symbols) {
    for (LispSymbol s: symbols) {
      defineSymbol(s);
    }
  }

  private void addBufferLocalVariable(String name) {
    addBufferLocalVariable(name, LispSymbol.NIL);
  }

  private void addBufferLocalVariable(String name, LispObject value) {
    LispSymbol symbol = new LispSymbol(name, value, true);
    setGlobalVariableDocumentation(symbol);
    myBufferLocals.add(name);
    putSymbol(symbol);
  }

  private void setGlobalVariableDocumentation(LispSymbol var) {
    if (!TestMode.EXTRACT_DOC) return;
    String doc = myDocumentationExtractor.getVariableDoc(var.getName());
    if (!StringUtil.isEmptyOrSpaces(doc)) var.setGlobalVariableDocumentation(new LispString(doc));
  }

  public Map<LispSymbol, LispObject> getBufferLocalVariables() {
    Map<LispSymbol, LispObject> map = new HashMap<>();
    for (String key: myBufferLocals) {
      LispSymbol symbol = mySymbols.get(key);
      map.put(new LispSymbol(symbol.getName()), symbol.getValue());
    }
    return map;
  }

  private void defineDefForms () {
    findAndRegisterEmacsFunction("defcustom");
    findAndRegisterEmacsFunction("defsubst");
    findAndRegisterEmacsFunction("defgroup");
    findAndRegisterEmacsFunction("defface");
    findAndRegisterEmacsFunction("define-derived-mode");
  }

  private void defineUserOptions() {
    defineSymbol("mark-even-if-inactive");   //callint.c
    defineSymbol("enable-recursive-minibuffers", LispSymbol.T);  //minibuf.c
  }

  private void defineBufferLocalVariables() {
    addBufferLocalVariable("mark-active"); //buffer.c
    addBufferLocalVariable("default-directory");  //BUFFER.C
    addBufferLocalVariable("enable-multibyte-characters");
    addBufferLocalVariable("major-mode", new LispSymbol("fundamental-mode"));
    addBufferLocalVariable("change-major-mode-hook");
    addBufferLocalVariable("char-property-alias-alist");
    addBufferLocalVariable("buffer-file-name");
  }

  private void defineGlobalVariables() {
    defineSymbol("load-history");
    defineSymbol("deactivate-mark");
    defineSymbol("purify-flag");
    defineSymbol("current-load-list");
    defineSymbol("executing-kbd-macro");
    defineSymbol("load-file-name");
    defineSymbol("overlay-arrow-variable-list");
    defineSymbol("case-fold-search");
    defineSymbol("obarray", new LispVector());
    defineSymbol("values", LispList.list());
    defineSymbol("standard-output", LispSymbol.T);
    defineSymbol("this-command");
    defineSymbol("last-command");
    defineSymbol("current-prefix-arg");
    defineSymbol("prefix-arg");
    defineSymbol("last-prefix-arg");
    defineSymbol("minibuffer-completing-file-name");
    defineSymbol("minibuffer-message-timeout", new LispInteger(2));
    defineSymbol("noninteractive");
    defineSymbol("parse-sexp-ignore-comments");
    defineSymbol("read-buffer-completion-ignore-case");
    defineSymbol("buffer-name-history");
    defineSymbol("completion-ignore-case");
    defineSymbol("completion-regexp-list", LispList.list());
    defineSymbol("minibuffer-history", LispList.list());
    defineSymbol("read-expression-map");
    defineSymbol("minibuffer-history-position");
    defineSymbol("timer-idle-list");
    defineSymbol("temp-buffer-show-function");
    defineSymbol("help-char", new LispInteger(8)); //ctrl-h
    defineSymbol("help-form");
    defineSymbol("prefix-help-command");
    defineSymbol("features", LispList.list());
    defineSymbol("face-new-frame-defaults");

    //todo check
    String os = System.getProperty("os.name").toLowerCase();
    String systemType;
    if (os.contains("win")) systemType = "windows-nt";
    else if (os.contains("linux")) systemType = "gnu/linux";
    else throw new IllegalStateException(os);
    defineSymbol("system-type", new LispString(systemType));

    defineSymbol("after-load-alist");  //lread.c
    defineSymbol("initial-window-system", new LispSymbol("x"));

    LispSymbol readOnly = new LispSymbol("read-only");
    GlobalEnvironment.INSTANCE.defineSymbol(readOnly); //just intern with no value

    defineSymbol("minibuffer-prompt-properties", LispList.list(readOnly, LispSymbol.T));  //minibuf.c

    defineSymbol("force-load-messages");
    defineSymbol("load-suffixes", LispList.list(new LispString(".el")));
    defineSymbol("load-path");
    defineSymbol("load-file-rep-suffixes", LispList.list(new LispString("")));
    defineSymbol("load-in-progress");
    defineSymbol("load-file-name");
  }

  public LispVector getObjectArray() {
    return new LispVector(mySymbols.values());
  }

  private void setSubroutinesFromClass (Class[] subroutineContainers, Primitive.Type type) {
    LispKeymap activeKeymap = getActiveKeymap();
    for (Class subroutineContainer: subroutineContainers) {
      Method[] methods = subroutineContainer.getMethods();
      for (Method m: methods) {
        Subroutine annotation = m.getAnnotation(Subroutine.class);
        if (annotation == null)
          continue;
        String name = annotation.value();
        LispSymbol symbol;
        if (mySymbols.containsKey(name)) {
          symbol = mySymbols.get(name);
          if (symbol.isFunction())
            throw new InternalException("Duplicate built-in: " + name + '!');
        } else {
          symbol = new LispSymbol(name);
          mySymbols.put(name, symbol);
        }
        symbol.setFunction(new Primitive(annotation, myDocumentationExtractor.getSubroutineDoc(name), type));
        if (activeKeymap != null && !StringUtil.isEmptyOrSpaces(annotation.key())) {
          activeKeymap.defineKey(symbol, new LispString(annotation.key()));
        }
      }
    }
  }

  private void setSubroutines () {
    setSubroutinesFromClass(LispSubroutine.getBuiltinsClasses(), Primitive.Type.BUILTIN);
    setSubroutinesFromClass(LispSubroutine.getSpecialFormsClasses(), Primitive.Type.SPECIAL_FORM);
  }

  private void setConstants() {
    defineConstant("nil", LispSymbol.NIL);
    defineConstant("t", LispSymbol.T);

    defineSymbol("void", LispSymbol.VOID);
    String docDir = ourEmacsHome + "/etc/";
    File file = new File (docDir);
    if (file.exists() && file.isDirectory()) {
      defineSymbol("doc-directory", new LispString(docDir));
      String[] docs = file.list(new FilenameFilter() {
        @Override
        public boolean accept(File file, String s) {
          return s.startsWith("DOC-");
        }
      });
      if (docs != null && docs.length == 1) {
        defineSymbol("internal-doc-file-name", new LispString(docs[0]));
      }
    }
  }

  public static void echo(String message, MessageType type) {
    if (INSTANCE == null || INSTANCE.myIde == null || TestMode.TEST) {
      LogUtil.log(LOG, "echo: " + message, type);
      return;
    }
    INSTANCE.myIde.echo(message, type);
  }

  public LispObject getBufferLocalSymbolValue (LispSymbol symbol) {
    LispSymbol real = mySymbols.get(symbol.getName());
    if (real == null || !real.isBufferLocal())
      return null;
    return real.getValue();
  }

  //============================= buffer processing =====================================
  @Override
  public void defineBuffer (LispBuffer buffer) {
    for (String local: myBufferLocals) {
      buffer.defineVariable(mySymbols.get(local));
    }
    ourBufferManager.define(buffer);
  }

  public void setVariableBufferLocal(final LispSymbol var) {
    if (myBufferLocals.contains(var.getName()))
      return;
    LispSymbol symbol = var;
    do {
      myBufferLocals.add(symbol.getName());
      LispSymbol existing = find(symbol.getName());
      if (existing == null) {
        symbol.setBufferLocal(true);
        defineSymbol(symbol);
      } else {
        existing.setBufferLocal(true);
      }
    } while ((symbol = symbol.next()) != null);
  }

  public List<String> getCommandList (String begin) {
    //todo: add data retrieved after source index
    Iterator<Map.Entry<String, LispSymbol>> iterator = mySymbols.entrySet().iterator();
    List<String> commandList = new ArrayList<>();
    while (iterator.hasNext()) {
      LispSymbol symbol = iterator.next().getValue();
      if (Predicate.commandp(symbol, null).equals(LispSymbol.T)) {
        if (symbol.getName().length() < begin.length())
          continue;
        if (begin.equals(symbol.getName().substring(0, begin.length())))
          commandList.add(symbol.getName());
      }
    }
    return commandList;
  }

  public List<String> getFunctionList (String begin) {
    //todo: add data retrieved after source index
    Iterator<Map.Entry<String, LispSymbol>> iterator = mySymbols.entrySet().iterator();
    List<String> functionList = new ArrayList<>();
    while (iterator.hasNext()) {
      LispSymbol symbol = iterator.next().getValue();
      if (Predicate.fboundp(this, symbol).equals(LispSymbol.T)) {
        if (symbol.getName().length() < begin.length())
          continue;
        if (begin.equals(symbol.getName().substring(0, begin.length())))
          functionList.add(symbol.getName());
      }
    }
    return functionList;
  }

  public List<String> getUserOptions (String begin) {
    //todo extract user options from lisp code
    Iterator<Map.Entry<String, LispSymbol>> iterator = mySymbols.entrySet().iterator();
    List<String> userOptions = new ArrayList<>();
    while (iterator.hasNext()) {
      LispSymbol symbol = iterator.next().getValue();
      if (Predicate.isUserOption(symbol)) {
        if (symbol.getName().length() < begin.length())
          continue;
        if (begin.equals(symbol.getName().substring(0, begin.length())))
          userOptions.add(symbol.getName());
      }
    }
    return userOptions;
  }

  //==============

  @Override
  public void clearRecorded() {
    Match.clearData();
    ourCallStack.clear();
    for (String name: myRecordedSymbols) {
      myBufferLocals.remove(name);
    }
    super.clearRecorded();
  }

  public boolean isVariableBufferLocal (String name) {
    return myBufferLocals.contains(name);
  }

  public static boolean isInteractiveCall() {
    for (Pair<String, Object> function: ourCallStack) {
      if (function.getFirst().equals("call-interactively"))
        return true;
    }
    return false;
  }

  public static boolean isHelpLoaded() {
    return myFilesToLoad.contains("help.el");
  }
}