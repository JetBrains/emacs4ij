package org.jetbrains.emacs4ij.jelisp;

import com.intellij.openapi.util.text.StringUtil;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.Predicate;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.EnvironmentException;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinPredicates;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinsKey;
import org.jetbrains.emacs4ij.jelisp.subroutine.Subroutine;

import java.io.*;
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
    private List<LispFrame> myFrames = new ArrayList<>();
    private LispFrame myCurrentFrame = null;
    private static String ourEmacsHome = "";
    private static String ourEmacsSource = "";
    public static final String ourMiniBufferName = " *Minibuf-0*";
    public static final String ourUnsetInteractiveString = "0";
    public static GlobalEnvironment INSTANCE = null;
    public static final LispSymbol ourFinder = new LispSymbol("find-lisp-object-file-name");
    private static Ide myIde = null;
    private static boolean isEmacsSourceOk = false;
    private static boolean isEmacsHomeOk = false;
    private static DocumentationExtractor myDocumentationExtractor;
    private List<String> myBufferLocals = new ArrayList<>();
    
    //for debug & extract definition on th fly
    public static Deque<String> ourCallStack = new ArrayDeque<>();

    //temporary solution while i'm not loading all sources
    private static List<String> myFilesToLoad = Arrays.asList("emacs-lisp/backquote.el");

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

    private LinkedList<SearchItem> mySearchHistory = new LinkedList<>();

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
    public static void initialize (@Nullable LispKeymapFactory keymapFactory, @Nullable LispBufferFactory bufferFactory, @Nullable Ide ide) {
        INSTANCE = new GlobalEnvironment();
        ourKeymapManager = new KeymapManager(keymapFactory);
        ourBufferManager = new BufferManager(bufferFactory);

        myIde = ide;
        if (INSTANCE.myCurrentFrame != null) {
            INSTANCE.onFrameOpened(INSTANCE.myCurrentFrame);
        }
        if (!testProperty(PropertyType.SOURCE)) {
            INSTANCE.mySymbols.clear();
            throw new EnvironmentException("Emacs source directory is invalid!");
        }
        myDocumentationExtractor = new DocumentationExtractor(ourEmacsSource + "/src");
        myDocumentationExtractor.scanAll();
        if (!testProperty(PropertyType.HOME)) {
            INSTANCE.mySymbols.clear();
            throw new EnvironmentException("Emacs home directory is invalid!");
        }

        INSTANCE.setConstants();
        INSTANCE.defineBufferLocalVariables();
        INSTANCE.defineGlobalVariables();
        INSTANCE.defineUserOptions();
        INSTANCE.setSubroutines();

        //note: it's important to load backquote before defsubst
        INSTANCE.loadFile(myFilesToLoad.get(0));
        INSTANCE.defineDefForms();

        BuiltinsKey.defineKeyMaps(INSTANCE);
        BuiltinsKey.keys_of_keymap();
        KeyBoardUtil.defineKbdSymbols(INSTANCE);
        KeyBoardUtil.keys_of_keyboard();

        for (int i = 1; i < myFilesToLoad.size(); ++i)
            INSTANCE.loadFile(myFilesToLoad.get(i));

    }

    private GlobalEnvironment () {
    }

    //-------- loading ------------------
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

    public static enum SymbolType {VAR, FUN}
    private static List<String> myDefVars = Arrays.asList("defcustom", "defvar", "defconst", "defgroup", "defface");
    private static List<String> myDefFuns = Arrays.asList("defun", "defmacro", "defsubst", "defalias");
    private static HashMap<String, File> myUploadHistory = new HashMap<>();

    private void defineDefForms () {
        findAndRegisterEmacsForm("defcustom", "/lisp/custom.el", SymbolType.FUN); //macro
        findAndRegisterEmacsForm("defsubst", "/lisp/emacs-lisp/byte-run.el", SymbolType.FUN); //macro
        findAndRegisterEmacsForm("defgroup", "/lisp/custom.el", SymbolType.FUN); //macro
        findAndRegisterEmacsForm("defface", "/lisp/custom.el", SymbolType.FUN); //macro
    }

    private void defineUserOptions() {
        defineSymbol("mark-even-if-inactive");   //callint.c
        defineSymbol("enable-recursive-minibuffers", LispSymbol.ourT);  //minibuf.c
    }

    private void defineBufferLocalVariables() {
        addBufferLocalVariable("mark-active"); //buffer.c
        addBufferLocalVariable("default-directory");  //BUFFER.C
        addBufferLocalVariable("enable-multibyte-characters");
    }

    private void defineGlobalVariables() {
        defineSymbol("load-history");   //lread.c
//        defineSymbol("deactivate-mark");  //keyboard.c    => moved to KeyBoardUtil
        defineSymbol("purify-flag");   //alloc.c
        defineSymbol("current-load-list");//lread.c
        defineSymbol("executing-kbd-macro"); //macros.c
        defineSymbol("load-file-name"); //lread.c
        defineSymbol("overlay-arrow-variable-list");//xdisp.c
        defineSymbol("case-fold-search");
        defineSymbol("obarray", new LispVector()); //lread.c      //todo: obarray or mySymbols?
    }

    private void setSubroutinesFromClass (Class[] subroutineContainers, Primitive.Type type) {
        for (Class subroutineContainer: subroutineContainers) {
            Method[] methods = subroutineContainer.getMethods();
            for (Method m: methods) {
                Subroutine annotation = m.getAnnotation(Subroutine.class);
                if (annotation == null)
                    continue;
                String name = annotation.value();
                if (mySymbols.containsKey(name))
                    throw new LispException("Duplicate symbol: " + name + '!');
                if (annotation.isCmd() && annotation.interactive().equals(ourUnsetInteractiveString))
                    throw new LispException("Interactive string not set! Subroutine " + name);

                LispSymbol subroutine = new LispSymbol(name);
                subroutine.setFunction(new Primitive(annotation, myDocumentationExtractor.getSubroutineDoc(name), type));

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
        mySymbols.put("nil", LispSymbol.ourNil);
        mySymbols.put("t", LispSymbol.ourT);
        mySymbols.put("void", LispSymbol.ourVoid);
        String docDir = ourEmacsHome + "/etc/";
        File file = new File (docDir);
        if (file.exists() && file.isDirectory()) {
            defineSymbol("doc-directory", new LispString(docDir));     //callproc.c
            String[] docs = file.list(new FilenameFilter() {
                @Override
                public boolean accept(File file, String s) {
                    return s.startsWith("DOC-");
                }
            });
            if (docs != null && docs.length == 1) {
                //"DOC-23.2.1"
                defineSymbol("internal-doc-file-name", new LispString(docs[0]));  //doc.c
            }
        }
    }

    //for test
    private ArrayList<LispSymbol> mySkipFunctions = new ArrayList<>();

    //for test
    public void addSkipFunctions (String... names) {
        for (String name: names)
            mySkipFunctions.add(new LispSymbol(name));
    }

    //todo it is public only for test
    public void loadFile (String fileName) {
        String fullName = ourEmacsSource + "/lisp/" + fileName;
        BufferedReader reader;
        try {
            reader = new BufferedReader(new FileReader(fullName));
        } catch (FileNotFoundException e) {
            System.err.println(e.getMessage());
            return;
        }
        String line;
        int index = 0;
        BufferedReaderParser p = new BufferedReaderParser(reader);
        while (true){
            try {
                line = reader.readLine();
                index++;
            } catch (IOException e) {
                throw new RuntimeException("Error while reading " + fullName + ", line " + index);
            }
            if (line == null)
                break;
            LispObject parsed = p.parse(line);
            index += p.getLine();
            if (parsed == null || LispSymbol.ourNil.equals(parsed))
                continue;
            if (parsed instanceof LispList && mySkipFunctions.contains(((LispList) parsed).car()))
                continue;
            try {
                parsed.evaluate(this);
            } catch (LispException e) {
                System.err.println(fullName + ", line " + index + ": " + e.getMessage());
                throw e;
            }
        }
    }

    //TODO: its only for test
    public static LispList getDefFromFile(String fileName, String functionName, SymbolType type) {
        return getDefFromFile(new File(fileName), functionName, type);
    }

    private static boolean containsDef (String line, String name, List<String> defForms) {
        for (String defForm: defForms) {
            if (line.contains('(' + defForm + ' ' + name + ' ')
                    || line.contains('(' + defForm + " '" + name + ' ')) {
                return true;
            }
        }
        return false;
    }

    private static LispList getDefFromFile(File file, String name, SymbolType type) {
        BufferedReader reader;
        try {
            reader = new BufferedReader(new FileReader(file));
        } catch (FileNotFoundException e) {
            throw new RuntimeException("File not found: " + file.getName());
        }
        String line;
        while (true) {
            try {
                line = reader.readLine();
            } catch (IOException e) {
                throw new RuntimeException("Error while reading " + file.getName());
            }
            if (line == null)
                return null;
            if (containsDef(line, name, (type == SymbolType.FUN ? myDefFuns : myDefVars)))
                break;
        }
        BufferedReaderParser p = new BufferedReaderParser(reader);
        LispObject parsed = p.parse(line);
        if (parsed instanceof LispList) {
            myUploadHistory.put(name, file);
            return (LispList) parsed;
        }
        throw new RuntimeException("Parsed object is not a LispList!");
    }

    private static LispList getDefFromInvokersSrc(String name, SymbolType type) {
        for (String invoker: ourCallStack) {
            if (myUploadHistory.containsKey(invoker)) {
                LispList def = getDefFromFile(myUploadHistory.get(invoker), name, type);
                if (def != null)
                    return def;
            }
        }
        return null;
    }

    private static LispList findEmacsDefinition(String name, File sourceDir, SymbolType type) {
        List<File> src = Arrays.asList(sourceDir.listFiles(new FileFilter() {
            @Override
            public boolean accept(File file) {
                return (file.isDirectory() || file.getName().endsWith(".el"));
            }
        }));
        //subr.el has priority. This is done to avoid uploading wrong defs while we are not loading code in appropriate order
        File subr = (File) CollectionUtils.find(src, new Predicate() {
            @Override
            public boolean evaluate(Object o) {
                return (((File) o).getAbsolutePath().endsWith("lisp/subr.el"));
            }
        });
        LispList def = null;
        if (subr != null) {
            def = getDefFromFile(subr, name, type);
        }
        for (int i = 0, srcSize = src.size(); i < srcSize && def == null; i++) {
            File file = src.get(i);
            if (file.getName().endsWith("lisp/subr.el"))
                continue;
            if (file.isDirectory()) {
                def = findEmacsDefinition(name, file, type);
            } else
                def = getDefFromFile(file, name, type);
        }
        return def;
    }

    private LispSymbol processDef (LispList definition, String name, SymbolType type) {
        if (definition == null)
            return null;
        LispObject evaluated = definition.evaluate(this);
        if (!(evaluated instanceof LispSymbol))
            throw new RuntimeException("findAndRegisterEmacsForm FAILED : " + name);
        LispSymbol value = find(((LispSymbol) evaluated).getName());
        if (value == null) {
            value = findAndRegisterEmacsForm((LispSymbol) evaluated, type);
        }
        return value;
    }

    public LispSymbol findAndRegisterEmacsForm (String name, String file, SymbolType type) {
        LispList definition = getDefFromFile(new File(ourEmacsSource + file), name, type);
        return processDef(definition, name, type);
    }

    public LispSymbol findAndRegisterEmacsForm(String name, SymbolType type) {
        LispList definition = getDefFromInvokersSrc(name, type);
        if (definition == null)
            definition = findEmacsDefinition(name, new File(ourEmacsSource + "/lisp"), type);
        return processDef(definition, name, type);
    }

    public LispSymbol findAndRegisterEmacsForm(LispSymbol name, SymbolType type) {
        return findAndRegisterEmacsForm(name.getName(), type);
    }

    public static void showErrorMessage (String message) {
        myIde.showErrorMessage(message);
    }

    public static void showInfoMessage (String message) {
        myIde.showInfoMessage(message);
    }

    public LispObject getBufferLocalSymbolValue (LispSymbol symbol) {
        LispSymbol real = mySymbols.get(symbol.getName());
        if (real == null || !real.isBufferLocal())
            return null;
        return real.getValue();
    }

    //============================= buffer processing =====================================

    @Override
    public void defineServiceBuffer (LispBuffer buffer) {
        ourBufferManager.defineServiceBuffer(buffer);
        if (myCurrentFrame != null)
            myCurrentFrame.openWindow(buffer);
    }

    @Override
    public void defineBuffer (LispBuffer buffer) {
        for (String local: myBufferLocals) {
            buffer.defineLocalVariable(mySymbols.get(local), true);
        }
        if (ourBufferManager.defineBuffer(buffer) && myCurrentFrame != null) {
            myCurrentFrame.openWindow(buffer);
        }
    }

    public void defineBufferLocalVariable (LispSymbol symbol) {
        if (myBufferLocals.contains(symbol.getName()))
            return;
        symbol.setBufferLocal(true);
        myBufferLocals.add(symbol.getName());
        defineSymbol(symbol);
        ourBufferManager.defineBufferLocalVariable(symbol);
    }

    @Override
    public void closeCurrentBuffer () {
        LispBuffer b = ourBufferManager.getCurrentBuffer();
        if (myCurrentFrame != null)
            myCurrentFrame.closeWindow(b);
        ourBufferManager.removeBuffer(b);
    }

    // for test
    public void removeBuffer(String name) {
        LispBuffer buffer = findBufferSafe(name);
        if (myCurrentFrame != null)
            myCurrentFrame.closeWindow(buffer);
        ourBufferManager.removeBuffer(buffer);
    }

    @Override
    public void killBuffer (LispBuffer buffer) {
        super.killBuffer(buffer);
        if (myCurrentFrame != null)
            myCurrentFrame.closeWindow(buffer);
    }

    @Override
    public void killBuffer (String bufferName) {
        killBuffer(findBufferSafe(bufferName));
    }

    public ArrayList<String> getCommandList (String begin) {
        Iterator<Map.Entry<String, LispSymbol>> iterator = mySymbols.entrySet().iterator();
        ArrayList<String> commandList = new ArrayList<>();
        while (iterator.hasNext()) {
            LispSymbol symbol = iterator.next().getValue();
            if (BuiltinPredicates.commandp(symbol, null).equals(LispSymbol.ourT)) {
                if (symbol.getName().length() < begin.length())
                    continue;
                if (begin.equals(symbol.getName().substring(0, begin.length())))
                    commandList.add(symbol.getName());
            }
        }
        return commandList;
    }

    public ArrayList<String> getFunctionList (String begin) {
        Iterator<Map.Entry<String, LispSymbol>> iterator = mySymbols.entrySet().iterator();
        ArrayList<String> functionList = new ArrayList<>();
        while (iterator.hasNext()) {
            LispSymbol symbol = iterator.next().getValue();
            if (BuiltinPredicates.fboundp(this, symbol).equals(LispSymbol.ourT)) {
                if (symbol.getName().length() < begin.length())
                    continue;
                if (begin.equals(symbol.getName().substring(0, begin.length())))
                    functionList.add(symbol.getName());
            }
        }
        return functionList;
    }

    public ArrayList<String> getBufferNamesList (String begin) {
        ArrayList<String> bufferNamesList = new ArrayList<>();
        for (String bufferName: ourBufferManager.getBuffersNames()) {
            if (bufferName.length() >= begin.length()) {
                if (bufferName.substring(0, begin.length()).equals(begin)) {
                    bufferNamesList.add(bufferName);
                }
            }
        }
        return bufferNamesList;
    }

    //------------------------------------------- FRAMES ------------------------------------------------------

    public void onFrameOpened (LispFrame newFrame) {
        if (frameIndex(newFrame) != -1)
            return;
        myFrames.add(newFrame);
    }

    public void onFrameReleased (LispFrame frame) {
        myFrames.remove(frame);
    }

    public void setSelectedFrame (LispFrame frame) {
        int k = frameIndex(frame);
        if (k < 0) {
            System.out.println("fail");
        }
        myCurrentFrame = myFrames.get(k);
    }

    public LispFrame getSelectedFrame() {
        return myCurrentFrame;
    }

    private int frameIndex (LispFrame frame) {
        for (int i = 0; i != myFrames.size(); ++i) {
            if (myFrames.get(i).areIdeFramesEqual(frame))
                return i;
        }
        return -1;
    }

    public static void setFrameVisible (LispFrame frame, boolean status) {
        if (INSTANCE == null)
            return;
        int k = INSTANCE.frameIndex(frame);
        INSTANCE.myFrames.get(k).setVisible(status);
    }

    public static void setFrameIconified (LispFrame frame, boolean status) {
        if (INSTANCE == null)
            return;
        int k = INSTANCE.frameIndex(frame);
        INSTANCE.myFrames.get(k).setIconified(status);
    }

    public static boolean isFrameAlive (LispFrame frame) {
        return INSTANCE != null && INSTANCE.frameIndex(frame) >= 0;
    }

    public static ArrayList<LispFrame> getVisibleFrames () {
        ArrayList<LispFrame> visibleFrames = new ArrayList<>();
        if (INSTANCE == null)
            return visibleFrames;
        for (LispFrame frame: INSTANCE.myFrames) {
            if (BuiltinPredicates.frameVisibleP(frame).equals(LispSymbol.ourT))
                visibleFrames.add(frame);
        }
        return visibleFrames;
    }

    public static ArrayList<LispFrame> getVisibleAndIconifiedFrames () {
        ArrayList<LispFrame> frames = new ArrayList<>();
        if (INSTANCE == null)
            return frames;
        for (LispFrame frame: INSTANCE.myFrames) {
            LispSymbol predicate = BuiltinPredicates.frameVisibleP(frame);
            if (predicate.equals(LispSymbol.ourT) || predicate.equals(new LispSymbol("icon")))
                frames.add(frame);
        }
        return frames;
    }

    public static List<LispFrame> getAllFrames () {
        if (INSTANCE == null)
            return new ArrayList<>();
        return INSTANCE.myFrames;
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
    
    public LispKeymap makeKeymap (@Nullable LispObject name) {
        return ourKeymapManager.createKeymap(name);
    }
}