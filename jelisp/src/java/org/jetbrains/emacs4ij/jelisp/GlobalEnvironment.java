package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinPredicates;
import org.jetbrains.emacs4ij.jelisp.subroutine.Subroutine;

import java.io.*;
import java.lang.reflect.Method;
import java.util.*;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 11/14/11
 * Time: 11:20 AM
 * To change this template use File | Settings | File Templates.
 */
public class GlobalEnvironment extends Environment {
    //private ArrayList<String> myBufferDescriptors = new ArrayList<>();
    // private BufferManager myBufferManager = null;
    // private ArrayList<LispBuffer> myBuffers = new ArrayList<>();
    // private LispBufferFactory myBufferFactory = null;
    private ArrayList<LispFrame> myFrames = new ArrayList<>();
    private LispFrame myCurrentFrame = null;

    public static String ourEmacsPath = "";
    public static String ourEmacsSource = "";

    private Object myProject = null;

    public static final String ourMiniBufferName = " *Minibuf-0*";
    public static final String ourScratchBufferName = "*scratch*";
    public static final String ourUnsetInteractiveString = "0";

    public static GlobalEnvironment INSTANCE = null;

    private HashMap<String, LObject> ourUserOptions = new HashMap<String, LObject>();

    public static final LispSymbol ourFinder = new LispSymbol("find-lisp-object-file-name");
    private static final String ourFinderPath = "/lisp/help-fns.el";

    private static Ide myIde = null;


    //for debug
    public static ArrayDeque<String> ourCallStack = new ArrayDeque<String>();

    public static int initialize (@Nullable LispBufferFactory bufferFactory, @Nullable Ide ide) {
        INSTANCE = new GlobalEnvironment();
        if (INSTANCE.myCurrentFrame != null) {
            INSTANCE.onFrameOpened(INSTANCE.myCurrentFrame);
        }

        if (!INSTANCE.setConstants()) {
            INSTANCE.mySymbols.clear();
            return -1;
        }

        INSTANCE.defineBufferLocalVariables();
        INSTANCE.defineGlobalVariables();
        INSTANCE.defineUserOptions();
        if (!INSTANCE.setSubroutines()) {
            INSTANCE.mySymbols.clear();
            return -2;
        }
        INSTANCE.defineDefForms();

        myIde = ide;
        ourBufferManager = new BufferManager(bufferFactory);

        //INSTANCE.myProject = project;
        return 0;
        //findAndRegisterEmacsFunction(ourFinder);
    }


    /* public static void setProject (Object project) {
        INSTANCE.myProject = project;
    }*/

    private GlobalEnvironment () {
        // myProject = project;
        // myBufferFactory = bufferFactory;
        // myOuterEnv = null;
    }

    private void addVariable(String name, @Nullable LObject value, int documentation) {
        LispSymbol symbol = new LispSymbol(name, value);
        symbol.setGlobalVariableDocumentation(new LispInteger(documentation));
        mySymbols.put(name, symbol);
    }

    private void addBufferLocalVariable(String name, @Nullable LObject value, int documentation) {
        LispSymbol symbol = new LispSymbol(name, value, true);
        symbol.setGlobalVariableDocumentation(new LispInteger(documentation));
        mySymbols.put(name, symbol);
    }

    private void defineDefForms () {
        findAndRegisterEmacsForm("defcustom");
    }

    private void defineUserOptions() {
        addVariable("transient-mark-mode", LispSymbol.ourT, -2109012);
        addVariable("mark-even-if-inactive", LispSymbol.ourNil, -467784);
        addVariable("mark-ring-max", new LispInteger(16), 2103241);
        addVariable("global-mark-ring", LispSymbol.ourNil, 2103330);
        addVariable("global-mark-ring-max", new LispInteger(16), 2103403);
    }

    private void defineBufferLocalVariables() {
        addBufferLocalVariable("mark-active", LispSymbol.ourNil, 329910);
        addBufferLocalVariable("default-directory", LispSymbol.ourNil, 316938);
        addBufferLocalVariable("mark-ring", LispSymbol.ourNil, 2103159);
    }

    private void defineGlobalVariables() {
        addVariable("load-history", LispSymbol.ourNil, 550505);
        addVariable("deactivate-mark", LispSymbol.ourNil, 264600);
        addVariable("purify-flag", LispSymbol.ourNil, 415585);
        addVariable("current-load-list", LispSymbol.ourNil, 552006);

        //wtf?
        addVariable("activate-mark-hook", LispSymbol.ourNil, 2100203);
        addVariable("deactivate-mark-hook", LispSymbol.ourNil, 2100379);
    }

    private void setSubroutinesFromClass (HashMap<String, String> documentation,  Class[] subroutineContainers, Primitive.Type type) {
        for (Class subroutineContainer: subroutineContainers) {
            Method[] methods = subroutineContainer.getMethods();
            for (Method m: methods) {
                Subroutine annotation = m.getAnnotation(Subroutine.class);
                if (annotation == null)
                    continue;
                String name = annotation.value();
                if (mySymbols.containsKey(name))
                    throw new RuntimeException("Duplicate symbol: " + name + '!');
                if (annotation.isCmd() && annotation.interactive().equals(ourUnsetInteractiveString))
                    throw new RuntimeException("Interactive string not set! Subroutine " + name);

                LispSymbol subroutine = new LispSymbol(name);
                subroutine.setFunction(new Primitive(annotation, documentation.get(name), type));

                mySymbols.put(name, subroutine);
                //System.out.print(name + ' ');
            }
        }
    }

    private boolean setSubroutines () {
        //int n = mySymbols.size();
        DocumentationExtractor d = new DocumentationExtractor(ourEmacsSource + "/src");
        if (d.scanAll() > 2) {
            return false;
        }
        setSubroutinesFromClass(d.getSubroutineDoc(), LispSubroutine.getBuiltinsClasses(), Primitive.Type.BUILTIN);
        setSubroutinesFromClass(d.getSubroutineDoc(), LispSubroutine.getSpecialFormsClasses(), Primitive.Type.SPECIAL_FORM);
        // System.out.println("implemented " + (mySymbols.size()-n) + " subroutines");
        return true;
    }

    private boolean setConstants() {
        mySymbols.put("nil", LispSymbol.ourNil);
        mySymbols.put("t", LispSymbol.ourT);
        mySymbols.put("void", LispSymbol.ourVoid);


        String docDir = ourEmacsPath + "/etc/";
        File file = new File (docDir);
        if (file.isDirectory()) {
            addVariable("doc-directory", new LispString(docDir), 601973);

            String[] docs = file.list(new FilenameFilter() {
                @Override
                public boolean accept(File file, String s) {
                    return s.startsWith("DOC-");
                }
            });
            if (docs != null && docs.length == 1) {
                //"DOC-23.2.1"
                addVariable("internal-doc-file-name", new LispString(docs[0]), 432776);
            } else
                return false;
        } else
            return false;


        return true;

    }

    public static void showMessage (String message) {
        myIde.showMessage(message);
    }

    public static void showErrorMessage (String message) {
        myIde.showErrorMessage(message);
    }



    /*public Object getProject () {
        return myProject;
    }*/


    @Override
    public void updateFunction (LispSymbol symbol) {
        LispSymbol function = mySymbols.get(symbol.getName());
        if (function == null) {
            throw new EnvironmentException("Trying to update nonexistent function!");
        }
        defineSymbol(symbol);
    }
    
    public LObject getBufferLocalSymbolValue (LispSymbol symbol) {
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
        if (ourBufferManager.defineBuffer(buffer) && myCurrentFrame != null) {
            myCurrentFrame.openWindow(buffer);
        }
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







    /* public ArrayList<LispBuffer> getBuffersWithNameNotBeginningWithSpace () {
        CustomEnvironment main = getMainEnvironment();
        ArrayList<LispBuffer> noSpace = new ArrayList<LispBuffer>();
        for (LispBuffer buffer: main.myBuffers) {
            if (buffer.getName().charAt(0) != ' ')
                noSpace.add(buffer);
        }
        return noSpace;
    }

    public LispBuffer getFirstNotServiceBuffer () {
        CustomEnvironment main = getMainEnvironment();
        ArrayList<LispBuffer> myBuffers1 = main.myBuffers;
        for (int i = myBuffers1.size() - 1; i != -1; --i) {
            LispBuffer buffer = myBuffers1.get(i);
            char start = buffer.getName().charAt(0);
            if (start != ' ' && start != '*')
                return buffer;
        }
        throw new NoOpenedBufferException();
    }

    public LispBuffer getFirstBufferWithNameNotBeginningWithSpace () {
        CustomEnvironment main = getMainEnvironment();
        for (LispBuffer buffer: main.myBuffers) {
            if (buffer.getName().charAt(0) != ' ')
                return buffer;
        }
        throw new NoBufferException("Buffer with name not beginning with space");
    }*/







    public ArrayList<String> getCommandList (String begin) {
        Iterator<Map.Entry<String, LispSymbol>> iterator = mySymbols.entrySet().iterator();
        ArrayList<String> commandList = new ArrayList<String>();
        while (iterator.hasNext()) {
            LispSymbol symbol = iterator.next().getValue();
            if (BuiltinPredicates.commandp(this, symbol, null).equals(LispSymbol.ourT)) {
                if (symbol.getName().length() < begin.length())
                    continue;
                if (begin.equals(symbol.getName().substring(0, begin.length())))
                    commandList.add(symbol.getName());
            }
        }
        //Collections.sort(commandList);
        return commandList;
    }

    public ArrayList<String> getFunctionList (String begin) {
        Iterator<Map.Entry<String, LispSymbol>> iterator = mySymbols.entrySet().iterator();
        ArrayList<String> functionList = new ArrayList<String>();
        while (iterator.hasNext()) {
            LispSymbol symbol = iterator.next().getValue();
            if (BuiltinPredicates.fboundp(this, symbol).equals(LispSymbol.ourT)) {
                if (symbol.getName().length() < begin.length())
                    continue;
                if (begin.equals(symbol.getName().substring(0, begin.length())))
                    functionList.add(symbol.getName());
            }
        }
        //Collections.sort(functionList);
        return functionList;
    }

    public ArrayList<String> getBufferNamesList (String begin) {
        ArrayList<String> bufferNamesList = new ArrayList<String>();
        for (String bufferName: ourBufferManager.getBuffersNames()) {
            if (bufferName.length() >= begin.length()) {
                if (bufferName.substring(0, begin.length()).equals(begin)) {
                    bufferNamesList.add(bufferName);
                }
            }
        }
        return bufferNamesList;
    }

    //TODO: its only for test
    public static LispList getDefFromFile(String fileName, String functionName) {
        return getDefFromFile(new File(fileName), functionName);
    }

    private static LispList getDefFromFile(File file, String name) {
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
                throw new RuntimeException("definition " + name + " not found in " + file.getName());
            if (line.contains("(defun " + name + ' '))
                break;
            if (line.contains("(defmacro " + name + ' '))
                break;
            if (line.contains("(defcustom " + name + ' '))
                break;
            if (line.contains("(defvar " + name + ' '))
                break;
        }
        BufferedReaderParser p = new BufferedReaderParser(reader);
        LispObject parsed = p.parse(line);
        if (parsed instanceof LispList) {
            String first = ((LispSymbol)((LispList) parsed).car()).getName();
            if (first.equals("defun") || first.equals("defmacro") || first.equals("defcustom") || first.equals("defvar"))
                return (LispList) parsed;
            throw new RuntimeException("Parsed list is not a specified definition!");
        }
        throw new RuntimeException("Parsed object is not a LispList!");
    }

    public static LispList findEmacsDefinition(String name, File sourceDir) {
        File[] src = sourceDir.listFiles(new FileFilter() {
            @Override
            public boolean accept(File file) {
                return (file.isDirectory() || file.getName().endsWith(".el"));
            }
        });
        for (File file: src) {
            if (file.isDirectory()) {
                LispList searchResult = findEmacsDefinition(name, file);
                if (searchResult != null)
                    return searchResult;
            }
            if (!file.getName().endsWith(".el"))
                continue;
            try {
                return getDefFromFile(file, name);
            } catch (RuntimeException e) {
                //skip
            }
        }
        return null;
    }

    //TODO: it is public only for test
    /* public static String findEmacsFunctionFileName(LispSymbol functionName) {
if (ourEmacsPath.equals("")) {
    throw new RuntimeException("Emacs path is not set!");
}

LispSymbol finder = INSTANCE.find(ourFinder.getName());
if (finder == null) {
    if (functionName.getName().equals(ourFinder.getName()))
        return ourEmacsPath + ourFinderPath;
    throw new RuntimeException("I don't know where to find function " + functionName);
}
if (functionName.getName().equals("symbol-file"))
    return ourEmacsPath + "/lisp/subr.el";

INSTANCE.setArgumentsEvaluated(true);


try {
        LObject symbolFunction = BuiltinsSymbol.symbolFunction(INSTANCE, functionName);
        LObject f = finder.evaluateFunction(INSTANCE, new LispList(functionName, symbolFunction).getData());
    } catch (VoidFunctionException e) {
        LObject f = null;
    }

LObject f = finder.evaluateFunction(INSTANCE, new LispList(functionName, new LispSymbol("variable")).getData());
if (f == null || f == LispSymbol.ourNil) {
    try {
        LObject symbolFunction = BuiltinsSymbol.symbolFunction(INSTANCE, functionName);
        f = finder.evaluateFunction(INSTANCE, new LispList(functionName, symbolFunction).getData());
    } catch (VoidFunctionException e) {
        f = null;
    }
    if (f == null || f == LispSymbol.ourNil) {
        f = finder.evaluateFunction(INSTANCE, new LispList(functionName, new LispSymbol("face")).getData());
    }
}
if (f == null || f == LispSymbol.ourNil) {
    throw new RuntimeException("I don't know where to find function " + functionName);
}
return ((LispString)f).getData();

}        */

    public LispSymbol findAndRegisterEmacsForm(String name) {
        LispSymbol emacsFunction = find(name);
        if (emacsFunction != null)
            return emacsFunction;

        LispList definition = findEmacsDefinition(name, new File(ourEmacsSource + "/lisp"));

        if (definition == null)
            throw new RuntimeException("Form " + name + " not found.");

        LObject evaluated = definition.evaluate(this);
        
        if (!(evaluated instanceof LispSymbol) || !name.equals(((LispSymbol) evaluated).getName())) {
            throw new RuntimeException("findAndRegisterEmacsForm FAILED : " + name);
        }
        return find(name);
    }
    
    public LispSymbol findAndRegisterEmacsForm(LispSymbol name) {
        return findAndRegisterEmacsForm(name.getName());
    }

    //TODO: for test only
    public void findAndRegisterEmacsFunction (String file, String name) {
        LispList function = getDefFromFile(file, name);
        function.evaluate(this);
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

    private static int frameIndex (LispFrame frame) {
        for (int i = 0; i != INSTANCE.myFrames.size(); ++i) {
            if (INSTANCE.myFrames.get(i).areIdeFramesEqual(frame))
                return i;
        }
        return -1;
    }

    public static void setFrameVisible (LispFrame frame, boolean status) {
        if (INSTANCE == null)
            return;
        int k = frameIndex(frame);
        INSTANCE.myFrames.get(k).setVisible(status);
    }

    public static void setFrameIconified (LispFrame frame, boolean status) {
        if (INSTANCE == null)
            return;
        int k = frameIndex(frame);
        INSTANCE.myFrames.get(k).setIconified(status);
    }

    public static boolean isFrameAlive (LispFrame frame) {
        return INSTANCE != null && frameIndex(frame) >= 0;
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
    
    public static ArrayList<LispFrame> getAllFrames () {
        if (INSTANCE == null)
            return new ArrayList<>();
        return INSTANCE.myFrames;
    }
}
