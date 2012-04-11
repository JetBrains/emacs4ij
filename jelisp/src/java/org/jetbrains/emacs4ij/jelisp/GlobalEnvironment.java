package org.jetbrains.emacs4ij.jelisp;

import com.intellij.openapi.util.text.StringUtil;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.EnvironmentException;
import org.jetbrains.emacs4ij.jelisp.exception.InternalException;
import org.jetbrains.emacs4ij.jelisp.subroutine.Key;
import org.jetbrains.emacs4ij.jelisp.subroutine.Predicate;
import org.jetbrains.emacs4ij.jelisp.subroutine.Subroutine;

import java.io.File;
import java.io.FilenameFilter;
import java.lang.reflect.Method;
import java.util.*;
import java.util.regex.Matcher;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 11/14/11
 * Time: 11:20 AM
 * To change this template use File | Settings | File Templates.
 */
public class GlobalEnvironment extends Environment {
    private static String ourEmacsHome = "";
    private static String ourEmacsSource = "";
    private static boolean isEmacsSourceOk = false;
    private static boolean isEmacsHomeOk = false;
    private final Ide myIde;
    private final DocumentationExtractor myDocumentationExtractor;
    private List<String> myBufferLocals = new ArrayList<>();
    
    //for debug & extract definition on the fly
    public static Deque<String> ourCallStack = new ArrayDeque<>();

    //temporary solution while i'm not loading all sources
    private static List<String> myFilesToLoad = Arrays.asList("emacs-lisp/backquote.el");

    public static final String ourMiniBufferName = " *Minibuf-0*";

    private LinkedList<SearchItem> mySearchHistory = new LinkedList<>();

    public static GlobalEnvironment INSTANCE = null;

    private class SearchItem {
        private LispObject mySource; //may be a LispString or LispBuffer
        private Matcher myResult;

        public SearchItem (LispString string, Matcher matcher) {
            mySource = string;
            myResult = matcher;
        }

        public Matcher getResult() {
            return myResult;
        }
    }

    public static String getEmacsHome() {
        return ourEmacsHome;
    }

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

    //input parameters are nullable only for test!!!
    public static void initialize (@Nullable LispKeymapFactory keymapFactory,
                                   @Nullable Ide ide, @Nullable FrameManager frameManager) {
        ourKeymapManager = new EmacsKeymapManager(keymapFactory);
        INSTANCE = new GlobalEnvironment(frameManager, ide);
        Key.init();
        INSTANCE.init();
    }

    private void init() {
        setConstants();
        defineBufferLocalVariables();
        defineGlobalVariables();
        defineUserOptions();
        setSubroutines();

        //note: it's important to load backquote before defsubst
//        DefinitionLoader.init();
        DefinitionLoader.loadFile(myFilesToLoad.get(0));
        defineDefForms();
        for (int i = 1; i < myFilesToLoad.size(); ++i)
            DefinitionLoader.loadFile(myFilesToLoad.get(i));
    }

    private GlobalEnvironment (FrameManager frameManager, Ide ide) {
        myFrameManager = frameManager;
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
        LispSymbol symbol = new LispSymbol(name, value == null ? LispSymbol.ourNil : value);
        defineSymbol(symbol);
        return symbol;
    }

    public void defineSymbol (String name) {
        defineSymbol(new LispSymbol(name, LispSymbol.ourNil));
    }

    @Override
    public void defineSymbol (LispSymbol symbol) {
        String doc = myDocumentationExtractor.getVariableDoc(symbol.getName());
        if (!StringUtil.isEmptyOrSpaces(doc))
            symbol.setGlobalVariableDocumentation(new LispString(doc));
        else if (isRecording && !myRecordedSymbols.contains(symbol.getName())) {
            myRecordedSymbols.add(symbol.getName());
        }
        mySymbols.put(symbol.getName(), symbol);
    }

    private void addBufferLocalVariable(String name) {
        LispSymbol symbol = new LispSymbol(name, LispSymbol.ourNil, true);
        symbol.setGlobalVariableDocumentation(new LispString(myDocumentationExtractor.getVariableDoc(name)));
        myBufferLocals.add(name);
        mySymbols.put(name, symbol);
    }

    private void addBufferLocalVariable(String name, LispObject value) {
        LispSymbol symbol = new LispSymbol(name, value, true);
        symbol.setGlobalVariableDocumentation(new LispString(myDocumentationExtractor.getVariableDoc(name)));
        myBufferLocals.add(name);
        mySymbols.put(name, symbol);
    }

    private void defineDefForms () {
        findAndRegisterEmacsFunction("defcustom");
        findAndRegisterEmacsFunction("defsubst");
        findAndRegisterEmacsFunction("defgroup");
        findAndRegisterEmacsFunction("defface");
    }

    private void defineUserOptions() {
        defineSymbol("mark-even-if-inactive");   //callint.c
        defineSymbol("enable-recursive-minibuffers", LispSymbol.ourT);  //minibuf.c
    }

    private void defineBufferLocalVariables() {
        addBufferLocalVariable("mark-active"); //buffer.c
        addBufferLocalVariable("default-directory");  //BUFFER.C
        addBufferLocalVariable("enable-multibyte-characters");
        addBufferLocalVariable("major-mode", new LispSymbol("fundamental-mode"));
    }

    private void defineGlobalVariables() {
        defineSymbol("load-history");   //lread.c
        defineSymbol("deactivate-mark");  //keyboard.c
        defineSymbol("purify-flag");   //alloc.c
        defineSymbol("current-load-list");//lread.c
        defineSymbol("executing-kbd-macro"); //macros.c
        defineSymbol("load-file-name"); //lread.c
        defineSymbol("overlay-arrow-variable-list");//xdisp.c
        defineSymbol("case-fold-search");
        defineSymbol("obarray", new LispVector()); //lread.c
        defineSymbol("values", LispList.list());
        defineSymbol("standard-output", LispSymbol.ourT);
        defineSymbol("this-command");
        defineSymbol("last-command");
        defineSymbol("current-prefix-arg");
        defineSymbol("prefix-arg");
        defineSymbol("last-prefix-arg");
        defineSymbol("minibuffer-completing-file-name");
        defineSymbol("minibuffer-message-timeout", new LispInteger(2));
        defineSymbol("noninteractive");
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
                if (mySymbols.containsKey(name))
                    throw new InternalException("Duplicate symbol: " + name + '!');
                LispSymbol subroutine = new LispSymbol(name);
                subroutine.setFunction(new Primitive(annotation, myDocumentationExtractor.getSubroutineDoc(name), type));
                if (activeKeymap != null && !StringUtil.isEmptyOrSpaces(annotation.key())) {
                    activeKeymap.defineKey(subroutine, new LispString(annotation.key()));
                }
                mySymbols.put(name, subroutine);
                //System.out.print(name + ' ');
            }
        }
    }

    private void setSubroutines () {
        //int n = mySymbols.size();
        setSubroutinesFromClass(LispSubroutine.getBuiltinsClasses(), Primitive.Type.BUILTIN);
        setSubroutinesFromClass(LispSubroutine.getSpecialFormsClasses(), Primitive.Type.SPECIAL_FORM);
        //System.out.println("implemented " + (mySymbols.size()-n) + " subroutines");
    }

    private void setConstants() {
        defineSymbol("nil", LispSymbol.ourNil);
        defineSymbol("nil", LispSymbol.ourNil);
        defineSymbol("t", LispSymbol.ourT);
        defineSymbol("void", LispSymbol.ourVoid);
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

    public static void showErrorMessage (String message) {
        if (INSTANCE == null || INSTANCE.myIde == null) {
            System.out.println(JelispBundle.message("emacs4ij.error", message));
            return;
        }
        INSTANCE.myIde.showErrorMessage(message);
    }

    public static void showInfoMessage (String message) {
        if (INSTANCE == null || INSTANCE.myIde == null) {
            System.out.println(JelispBundle.message("emacs4ij.message", message));
            return;
        }
        INSTANCE.myIde.showInfoMessage(message);
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
            buffer.defineLocalVariable(mySymbols.get(local), true);
        }
        getFrameManager().openBuffer(buffer);
    }

    public void defineBufferLocalVariable (LispSymbol symbol) {
        if (myBufferLocals.contains(symbol.getName()))
            return;
        symbol.setBufferLocal(true);
        myBufferLocals.add(symbol.getName());
        defineSymbol(symbol);
        getFrameManager().defineBufferLocalVariable(symbol);
    }

    // for test
    public void removeBuffer(String name) {
        LispBuffer buffer = findBufferSafe(name);
        getFrameManager().getSelectedFrame().closeWindow(buffer);
    }

    public List<String> getCommandList (String begin) {
        Iterator<Map.Entry<String, LispSymbol>> iterator = mySymbols.entrySet().iterator();
        List<String> commandList = new ArrayList<>();
        while (iterator.hasNext()) {
            LispSymbol symbol = iterator.next().getValue();
            if (Predicate.commandp(symbol, null).equals(LispSymbol.ourT)) {
                if (symbol.getName().length() < begin.length())
                    continue;
                if (begin.equals(symbol.getName().substring(0, begin.length())))
                    commandList.add(symbol.getName());
            }
        }
        return commandList;
    }

    public List<String> getFunctionList (String begin) {
        Iterator<Map.Entry<String, LispSymbol>> iterator = mySymbols.entrySet().iterator();
        List<String> functionList = new ArrayList<>();
        while (iterator.hasNext()) {
            LispSymbol symbol = iterator.next().getValue();
            if (Predicate.fboundp(this, symbol).equals(LispSymbol.ourT)) {
                if (symbol.getName().length() < begin.length())
                    continue;
                if (begin.equals(symbol.getName().substring(0, begin.length())))
                    functionList.add(symbol.getName());
            }
        }
        return functionList;
    }

    //==============

    @Override
    public void clearRecorded() {
        super.clearRecorded();
        mySearchHistory.clear();
        ourCallStack.clear();
    }

    public void registerSearchResult (LispString src, Matcher result) {
        mySearchHistory.add(new SearchItem(src, result));
    }

    public Matcher getLastSearchResult() {
        try {
            return mySearchHistory.getLast().getResult();
        } catch (NoSuchElementException e) {
            return null;
        }
    }
}