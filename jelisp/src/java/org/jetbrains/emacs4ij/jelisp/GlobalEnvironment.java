package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.DoubleBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.NoBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.NoOpenedBufferException;

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
    private ArrayList<LispBuffer> myBuffers = new ArrayList<LispBuffer>();
    private ArrayList<LispBuffer> myDeadBuffers = new ArrayList<LispBuffer>();
    private ArrayList<LispBuffer> myServiceBuffers = new ArrayList<LispBuffer>();

    public static String ourEmacsPath = "";
    public static String ourEmacsSource = "";
    private final LispBufferFactory myBufferFactory;
    private final Object myProject;

    public static final String ourMiniBufferName = " *Minibuf-0*";
    public static final String ourScratchBufferName = "*scratch*";
    public static final String ourUnsetInteractiveString = "0";

    private static GlobalEnvironment myInstance = null;
    private HashMap<String, LObject> ourUserOptions = new HashMap<String, LObject>();

    public static final LispSymbol ourFinder = new LispSymbol("find-lisp-object-file-name");
    private static final String ourFinderPath = "/lisp/help-fns.el";

    private static Ide myIde;

    //for debug
    public static ArrayDeque<String> ourCallStack = new ArrayDeque<String>();

    public static void initialize (LispBufferFactory bufferFactory, Object project, Ide ide) {
        myInstance = new GlobalEnvironment(bufferFactory, project, ide);
        findAndRegisterEmacsFunction(ourFinder);
    }

    public static GlobalEnvironment getInstance () {
        return myInstance;
    }

    private GlobalEnvironment (LispBufferFactory bufferFactory, Object project, Ide ide) {
        myProject = project;
        myBufferFactory = bufferFactory;
        myOuterEnv = null;
        myIde = ide;
        setConstants();
        defineBufferLocalVariables();
        defineGlobalVariables();
        defineUserOptions();
        setSubroutines();
    }

    private void addSymbol (String name, LObject value, int documentation) {
        LispSymbol symbol = new LispSymbol(name, value);
        symbol.setVariableDocumentation(new LispInteger(documentation));
        mySymbols.put(name, symbol);
    }

    private void defineUserOptions() {
        addSymbol("transient-mark-mode", LispSymbol.ourT, -2109012);
        addSymbol("mark-even-if-inactive", LispSymbol.ourNil, -467784);
        addSymbol("mark-ring-max", new LispInteger(16), 2103241);
    }

    private void defineBufferLocalVariables() {
        addSymbol("mark-active", LispSymbol.ourBufferLocalVariable, 329910);
        addSymbol("addSymbol", LispSymbol.ourBufferLocalVariable, 2103159);
    }

    private void defineGlobalVariables() {
        addSymbol("load-history", LispSymbol.ourNil, 550505);
        addSymbol("deactivate-mark", LispSymbol.ourNil, 264600);

        //wtf?
        addSymbol("activate-mark-hook", LispSymbol.ourNil, 2100203);
        addSymbol("deactivate-mark-hook", LispSymbol.ourNil, 2100379);
    }

    private void setSubroutines () {
        Class[] subroutineContainers = LispSubroutine.getSubroutineClasses();
        int n = mySymbols.size();
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
                mySymbols.put(name, LispSymbol.newSubroutine(name, annotation.isCmd(), annotation.interactive(), annotation.doc()));
                //System.out.print(name + ' ');
            }

        }
       // System.out.println("implemented " + (mySymbols.size()-n) + " subroutines");
    }

    private void setConstants() {
        mySymbols.put("nil", LispSymbol.ourNil);
        mySymbols.put("t", LispSymbol.ourT);
        mySymbols.put("void", LispSymbol.ourVoid);
        addSymbol("internal-doc-file-name", new LispString("DOC-23.2.1"), 432776);
        addSymbol("doc-directory", new LispString(ourEmacsPath + "/etc/"), 601973);
    }

/*private void indexEmacsSources() {
        if (ourEmacsPath.equals("")) {
            ourEmacsPath = "c:\\Users\\ekaterina.polishchuk\\Downloads\\emacs-23.3";
            //TODO:
            //throw new RuntimeException("Emacs path is not set!");
        }
        indexDirectory(ourEmacsPath + "\\lisp");
    }

    private void indexDirectory (String dir) {
        File[] fList = new File(dir).listFiles();
        for (File f: fList) {
            if (f.isDirectory())
                indexDirectory(f.getAbsolutePath());
            else if (f.getName().substring(f.getName().lastIndexOf('.'), f.getName().length()).equals("el")) {

                //TODO: parse and save
            }
        }


    }   */

    public static void showMessage (String message) {
        myIde.showMessage(message);
    }

    public static void showErrorMessage (String message) {
        myIde.showErrorMessage(message);
    }

    private LispBufferFactory getBufferFactory() {
        return myBufferFactory;
    }

    public Object getProject () {
        return myProject;
    }

    public LispBuffer createBuffer (String bufferName) {
        LispBuffer buffer = myBufferFactory.createBuffer(bufferName, this);
        //getMainEnvironment().defineBuffer(buffer);
        return buffer;
    }

    public void updateFunction (LispSymbol symbol) {
        LispSymbol function = mySymbols.get(symbol.getName());
        if (function == null) {
            throw new EnvironmentException("Trying to update nonexistent function!");
        }
        defineSymbol(symbol);
    }

    //============================= buffer processing =====================================

    @Override
    public void defineBuffer(LispBuffer buffer) {
        if (containsBuffer(buffer.getName())) {
            throw new DoubleBufferException("double "+buffer.getName());
        }
        if (!isDead(buffer.getName())) {
            myBuffers.add(buffer);
            return;
        }
        myDeadBuffers.remove(getIndexByName(myDeadBuffers, buffer.getName()));
    }

    public void defineServiceBuffer (LispBuffer buffer) {
        myServiceBuffers.add(buffer);
    }

    public void updateBuffer(LispBuffer buffer) {
        myBuffers.set(getIndexByName(myBuffers, buffer.getName()), buffer);
    }

    public void updateServiceBuffer (LispBuffer buffer) {
        myServiceBuffers.set(getIndexByName(myServiceBuffers, buffer.getName()), buffer);
    }

    protected LispBuffer getCurrentBuffer () {
        if (myBuffers.size() == 0)
            throw new NoOpenedBufferException();
        return myBuffers.get(myBuffers.size() - 1);
    }

    private int getIndexByName(ArrayList<LispBuffer> buffers, String bufferName) {
        for (int i=0; i!= buffers.size(); ++i) {
            if (buffers.get(i).getName().equals(bufferName))
                return i;
        }
        return -1;
    }

    public void switchToBuffer(String bufferName) {
        if (myBuffers.size() == 0)
            return;
        if (myBuffers.get(myBuffers.size() - 1).getName().equals(bufferName))
            return;
        int newCurrentBufferIndex = getIndexByName(myBuffers, bufferName);
        if (newCurrentBufferIndex == -1)
            throw new EnvironmentException("this buffer is not opened");
        Collections.rotate(myBuffers.subList(newCurrentBufferIndex, myBuffers.size()), -1);
    }

    public LispBuffer findBuffer (String bufferName) {
        for (LispBuffer buffer: myBuffers) {
            if (buffer.getName().equals(bufferName))
                return buffer;
        }
        return null;
    }

    public LispBuffer getServiceBuffer (String bufferName) {
        for (LispBuffer buffer: myServiceBuffers) {
            if (buffer.getName().equals(bufferName))
                return buffer;
        }
        return null;
    }
   /* public ArrayList<LispBuffer> getBuffersWithNameNotBeginningWithSpace () {
        Environment main = getMainEnvironment();
        ArrayList<LispBuffer> noSpace = new ArrayList<LispBuffer>();
        for (LispBuffer buffer: main.myBuffers) {
            if (buffer.getName().charAt(0) != ' ')
                noSpace.add(buffer);
        }
        return noSpace;
    }

    public LispBuffer getFirstNotServiceBuffer () {
        Environment main = getMainEnvironment();
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
        Environment main = getMainEnvironment();
        for (LispBuffer buffer: main.myBuffers) {
            if (buffer.getName().charAt(0) != ' ')
                return buffer;
        }
        throw new NoBufferException("Buffer with name not beginning with space");
    }*/

    public ArrayList<LispBuffer> getBuffers () {
        return myBuffers;
    }

    public LispBuffer getOtherBuffer (String bufferName) {
        //ArrayList<LispBuffer> noSpace = getBuffersWithNameNotBeginningWithSpace();
        if (myBuffers.isEmpty())
            throw new NoOpenedBufferException();
        if (myBuffers.size() == 1) {
            return myBuffers.get(0);
        }
        for (int i = myBuffers.size() - 1; i!=-1; --i) {
            if (!myBuffers.get(i).getName().equals(bufferName))
                return myBuffers.get(i);
        }
        throw new RuntimeException("other-buffer " + bufferName);
    }

    public int getBuffersSize() {
        return myBuffers.size();
    }

    public LispList getBufferList() {
        LispList bufferList = new LispList();
        for (LispBuffer buffer: myBuffers) {
            bufferList.add(buffer);
        }
        return bufferList;
    }

    public void closeCurrentBuffer () {
        myBuffers.remove(getCurrentBuffer());
    }

    public void killBuffer (String bufferName) {
        LispBuffer buffer = findBuffer(bufferName);
        if (buffer == null)
            throw new NoBufferException(bufferName);
        killBuffer(buffer);
    }

    public void killBuffer (LispBuffer buffer) {
        buffer.kill();
        myDeadBuffers.add(buffer);
        myBuffers.remove(buffer);
    }

    public void closeAllBuffers () {
        myBuffers.clear();
        //myDeadBuffers.clear();
        //myServiceBuffers.clear();
    }

    public LispBuffer getBufferByIndex (int index) {
        return myBuffers.get(index);
    }

    public void printBuffers() {
        for (int i=0; i!= myBuffers.size(); ++i) {
            System.out.print(myBuffers.get(i).getName()+"; ");
        }
        System.out.println();
    }

    public String[] getBuffersNames () {
        String[] buffersNames = new String[getBuffersSize()];
        for (int i=0; i!=getBuffersSize(); ++i) {
            buffersNames[i] = myBuffers.get(i).getName();
        }
        return buffersNames;
    }

    /*public LispString getDefaultDirectory () {
        return (LispString) getBufferCurrentForEditing().getLocalVariableValue("directory");
    }     */

    public void buryBuffer (LispBuffer buffer) {
        myBuffers.remove(buffer);
        myBuffers.add(0, buffer);
    }

    public LispBuffer lastBuffer () {
        return lastBuffer("");
    }

    public LispBuffer lastBuffer (String bufferName) {
        for (int i=0; i!=myBuffers.size(); ++i)
            if (!myBuffers.get(i).getName().equals(bufferName))
                return myBuffers.get(i);
        //todo: create and return *scratch*
        throw new NoOpenedBufferException();
    }

    public boolean isDead (String bufferName) {
        for (LispBuffer buffer: myDeadBuffers) {
            if (buffer.getName().equals(bufferName))
                return true;
        }
        return false;
    }

    public ArrayList<String> getCommandList (String begin) {
        Iterator<Map.Entry<String, LispSymbol>> iterator = mySymbols.entrySet().iterator();
        ArrayList<String> commandList = new ArrayList<String>();
        while (iterator.hasNext()) {
            LispSymbol symbol = iterator.next().getValue();
            if (BuiltinsCheck.commandp(this, symbol, null).equals(LispSymbol.ourT)) {
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
            if (BuiltinsCheck.fboundp(this, symbol).equals(LispSymbol.ourT)) {
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
        for (LispBuffer buffer: myBuffers) {
            if (buffer.getName().length() >= begin.length()) {
                if (buffer.getName().substring(0, begin.length()).equals(begin)) {
                    bufferNamesList.add(buffer.getName());
                }
            }
        }
        return bufferNamesList;
    }

    //TODO: its only for test
    public static LispList getFunctionFromFile(String fileName, String functionName) {
        return getFunctionFromFile(new File(fileName), functionName);
    }

    private static LispList getFunctionFromFile(File file, String functionName) {
        //File file = new File(fileName);
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
                throw new RuntimeException("function " + functionName + " not found in " + file.getName());
            if (line.contains("(defun " + functionName + ' '))
                break;
            if (line.contains("(defmacro " + functionName + ' '))
                break;
        }
        BufferedReaderParser p = new BufferedReaderParser(reader);
        LispObject parsed = p.parse(line);
        if (parsed instanceof LispList) {
            String first = ((LispSymbol)((LispList) parsed).car()).getName();
            if (first.equals("defun") || first.equals("defmacro"))
                return (LispList) parsed;
            throw new RuntimeException("Parsed list is not a function or macro definition!");
        }
        throw new RuntimeException("Parsed object is not a LispList!");
    }

    public static LispList findEmacsFunctionFileName(LispSymbol functionName, File sourceDir) {
       // File lispSource = new File(path);
        File[] src = sourceDir.listFiles(new FileFilter() {
            @Override
            public boolean accept(File file) {
                return (file.isDirectory() || file.getName().endsWith(".el"));
            }
        });

        for (File file: src) {
            if (file.isDirectory()) {
                LispList searchResult = findEmacsFunctionFileName(functionName, file);
                if (searchResult != null)
                    return searchResult;
            }
            if (!file.getName().endsWith(".el"))
                continue;
            try {
                return getFunctionFromFile(file, functionName.getName());
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

        LispSymbol finder = myInstance.find(ourFinder.getName());
        if (finder == null) {
            if (functionName.getName().equals(ourFinder.getName()))
                return ourEmacsPath + ourFinderPath;
            throw new RuntimeException("I don't know where to find function " + functionName);
        }
        if (functionName.getName().equals("symbol-file"))
            return ourEmacsPath + "/lisp/subr.el";

        myInstance.setArgumentsEvaluated(true);


        try {
                LObject symbolFunction = BuiltinsSymbol.symbolFunction(myInstance, functionName);
                LObject f = finder.evaluateFunction(myInstance, new LispList(functionName, symbolFunction).getData());
            } catch (VoidFunctionException e) {
                LObject f = null;
            }

        LObject f = finder.evaluateFunction(myInstance, new LispList(functionName, new LispSymbol("variable")).getData());
        if (f == null || f == LispSymbol.ourNil) {
            try {
                LObject symbolFunction = BuiltinsSymbol.symbolFunction(myInstance, functionName);
                f = finder.evaluateFunction(myInstance, new LispList(functionName, symbolFunction).getData());
            } catch (VoidFunctionException e) {
                f = null;
            }
            if (f == null || f == LispSymbol.ourNil) {
                f = finder.evaluateFunction(myInstance, new LispList(functionName, new LispSymbol("face")).getData());
            }
        }
        if (f == null || f == LispSymbol.ourNil) {
            throw new RuntimeException("I don't know where to find function " + functionName);
        }
        return ((LispString)f).getData();

    }        */

    public static LispSymbol findAndRegisterEmacsFunction (LispSymbol name) {
        LispSymbol emacsFunction = myInstance.find(name.getName());
        if (emacsFunction != null)
            return emacsFunction;

        LispList function = findEmacsFunctionFileName(name, new File(ourEmacsSource + "/lisp"));

        if (function == null)
            throw new RuntimeException("Function " + name.getName() + " not found.");

        LObject evaluated = function.evaluate(myInstance);
        if (!name.equals(evaluated)) {
            throw new RuntimeException("findAndRegisterEmacsFunction FAILED : " + name.getName());
        }
        return myInstance.find(name.getName());
    }

    //TODO: for test only
    public static void findAndRegisterEmacsFunction (String file, String name) {
        LispList function = getFunctionFromFile(file, name);
        function.evaluate(myInstance);
    }

}
