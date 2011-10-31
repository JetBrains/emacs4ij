package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.DoubleBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.NoBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.NoOpenedBufferException;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;


/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/11/11
 * Time: 5:37 PM
 * To change this template use File | Settings | File Templates.
 */
public class Environment {
    private HashMap<String, LispSymbol> mySymbols = new HashMap<String, LispSymbol>();
    private ArrayList<LispBuffer> myBuffers = new ArrayList<LispBuffer>();
    private Environment myOuterEnv;
    private LispBuffer myBufferCurrentForEditing = null;
    private boolean selectionManagedBySubroutine = false;

    public static String ourEmacsPath = "";
    private final Environment myGlobalEnvironment;

    private final LispBufferFactory myBufferFactory;

    /**
     * Constructor for global environment
     * @param bufferFactory
     */
    public Environment (@NotNull LispBufferFactory bufferFactory) {
        this.myBufferFactory = bufferFactory;
        myGlobalEnvironment = this;
        setGlobal();
    }

    //for test only!!!
    public Environment () {
        this.myBufferFactory = null;
        myGlobalEnvironment = this;
        setGlobal();
    }

    public Environment (@NotNull final Environment outerEnv) {
        myOuterEnv = outerEnv;
        myGlobalEnvironment = outerEnv.getGlobalEnvironment();
        myBufferFactory = outerEnv.getBufferFactory();
    }

    private LispBufferFactory getBufferFactory() {
        return myBufferFactory;
    }

    public LispBuffer createBuffer (String bufferName) {
        LispBuffer buffer = myBufferFactory.createBuffer(bufferName, this);
        //getMainEnvironment().defineBuffer(buffer);
        return buffer;
    }

    public boolean isSelectionManagedBySubroutine() {
        return selectionManagedBySubroutine;
    }

    public void setSelectionManagedBySubroutine(boolean selectionManagedBySubroutine) {
        this.selectionManagedBySubroutine = selectionManagedBySubroutine;
    }

    private Environment getGlobalEnvironment() {
        return myGlobalEnvironment;
    }

    public Environment getMainEnvironment () {
        if (myOuterEnv.equals(myGlobalEnvironment))
            return this;
        return myOuterEnv.getMainEnvironment();
    }

    private void setSubroutines () {
        Class[] subroutineContainers = LispSubroutine.getSubroutineClasses();
        for (Class subroutineContainer: subroutineContainers) {
            Method[] methods = subroutineContainer.getMethods();
            for (Method m: methods) {
                Subroutine annotation = m.getAnnotation(Subroutine.class);
                if (annotation == null)
                    continue;
                String name = annotation.value();
                if (mySymbols.containsKey(name))
                    throw new RuntimeException("Duplicate symbol: " + name + '!');
                mySymbols.put(name, LispSymbol.newSubroutine(name));
            }
        }
    }

    private void setGlobal() {
        mySymbols.put("nil", LispSymbol.ourNil);
        mySymbols.put("t", LispSymbol.ourT);
        setSubroutines();
       // mySymbols.put("*scratch*",  new LispSymbol("*scratch*", new LispBuffer("*scratch*")));
    }

    public boolean isMainEnvironment () {
        return (myOuterEnv.equals(myGlobalEnvironment));
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

    public LObject find(String name, String methodName) {
        return find(name, methodName, null);
    }

    public LispSymbol find(String name) {
        return (LispSymbol) find(name, "", null);
    }

    public LObject find(String name, String methodName, Class[] parameterTypes, Object... methodParameters) {
        LispSymbol lispObject = mySymbols.get(name);
        if (lispObject != null)
            return lispObject.invokeMethod(methodName, parameterTypes, methodParameters);
        if (myOuterEnv != null) {
            return myOuterEnv.find(name, methodName, parameterTypes, methodParameters);
        }
        return null;
    }

    public void defineSymbol (LispSymbol symbol) {
        mySymbols.put(symbol.getName(), symbol);
    }

    private boolean containsSymbol (String name) {
        return mySymbols.containsKey(name);
    }

    public void setVariable(LispSymbol symbol) {
        if (myOuterEnv == null)
            throw new EnvironmentException("You are not allowed to change global environment!");
        if (isMainEnvironment() || containsSymbol(symbol.getName())) {
            LispSymbol variable = mySymbols.get(symbol.getName());
            if (variable == null) {
                defineSymbol(symbol);
                return;
            }
            variable.setValue(symbol.getValue());
            defineSymbol(variable);
            return;
        }
        myOuterEnv.setVariable(symbol);
    }

    public void updateFunction (LispSymbol symbol) {
        if (myOuterEnv == null)
            throw new EnvironmentException("You are not allowed to change global environment!");
        if (isMainEnvironment() || containsSymbol(symbol.getName())) {
            LispSymbol function = mySymbols.get(symbol.getName());
            if (function == null) {
                throw new EnvironmentException("Trying to update nonexistent function!");
            }
            defineSymbol(symbol);
            return;
        }
        myOuterEnv.updateFunction(symbol);

    }

    //============================= buffer processing =====================================

    public void defineBuffer (LispBuffer buffer) {
        if (getIndexByName(buffer.getName()) != -1) {
            throw new DoubleBufferException("double "+buffer.getName());
        }
        myBuffers.add(buffer);
    }

    public void updateBuffer (LispBuffer buffer) {
        myBuffers.set(getIndexByName(buffer.getName()), buffer);
    }

    private LispBuffer getCurrentBuffer () {
        if (myBuffers.size() == 0)
            throw new NoOpenedBufferException();
        return myBuffers.get(myBuffers.size() - 1);
    }

    private int getIndexByName(String bufferName) {
        for (int i=0; i!= myBuffers.size(); ++i) {
            if (myBuffers.get(i).getName().equals(bufferName))
                return i;
        }
        return -1;
    }

    public void switchToBuffer(String bufferName) {
        if (!isMainEnvironment()) {
            myOuterEnv.switchToBuffer(bufferName);
            return;
        }
        if (myBuffers.size() == 0)
            throw new NoOpenedBufferException();
        if (myBuffers.get(myBuffers.size() - 1).getName().equals(bufferName))
            return;
        int newCurrentBufferIndex = getIndexByName(bufferName);
        if (newCurrentBufferIndex == -1)
            throw new EnvironmentException("this buffer is not opened");
        Collections.rotate(myBuffers.subList(newCurrentBufferIndex, myBuffers.size()), -1);
    }

    public void setBufferCurrentForEditing (LispBuffer buffer) {
        myBufferCurrentForEditing = buffer;
    }

    public LispBuffer getBufferCurrentForEditing() {
        if (myBufferCurrentForEditing == null) {
            return getCurrentBuffer();
        }
        return myBufferCurrentForEditing;
    }

    public LispBuffer findBuffer (String bufferName) {
        if (!isMainEnvironment())
            return myOuterEnv.findBuffer(bufferName);
        for (LispBuffer buffer: myBuffers) {
            if (buffer.getName().equals(bufferName))
                return buffer;
        }
        /*if (myOuterEnv != null)
            return myOuterEnv.findBuffer(bufferName);*/
        return null;
    }

    public ArrayList<LispBuffer> getBuffersWithNameNotBeginningWithSpace () {
        Environment main = getMainEnvironment();
        ArrayList<LispBuffer> noSpace = new ArrayList<LispBuffer>();
        for (LispBuffer buffer: main.myBuffers) {
            if (buffer.getName().charAt(0) != ' ')
                noSpace.add(buffer);
        }
        return noSpace;
    }

    public LispBuffer getFirstBufferWithNameNotBeginningWithSpace () {
        Environment main = getMainEnvironment();
        for (LispBuffer buffer: main.myBuffers) {
            if (buffer.getName().charAt(0) != ' ')
                return buffer;
        }
        throw new NoBufferException("Buffer with name not beginning with space");
    }

    public ArrayList<LispBuffer> getBuffers () {
        return myBuffers;
    }

    public LispBuffer getOtherBuffer () {
        return getOtherBuffer(getBufferCurrentForEditing().getName());
    }

    public LispBuffer getOtherBuffer (String bufferName) {
        ArrayList<LispBuffer> noSpace = getBuffersWithNameNotBeginningWithSpace();
        if (noSpace.isEmpty())
            throw new NoOpenedBufferException();
        if (noSpace.size() == 1) {
            return noSpace.get(0);
        }
        for (int i = noSpace.size() - 1; i!=-1; --i) {
            if (!noSpace.get(i).getName().equals(bufferName))
                return noSpace.get(i);
        }
        throw new RuntimeException("other-buffer " + bufferName);
    }

    public int getBuffersSize() {
        return myBuffers.size();
    }

    public LispList getBufferList() {
        Environment main = getMainEnvironment();
        LispList bufferList = new LispList();
        for (LispBuffer buffer: main.myBuffers) {
            bufferList.add(buffer);
        }
        return bufferList;
    }

    public void closeBuffer(String bufferName) {
        int toRemove = getIndexByName(bufferName);
        myBuffers.remove(toRemove);
    }

    public void closeCurrentBuffer () {
        myBuffers.remove(getCurrentBuffer());
    }

    public void closeAllBuffers () {
        myBuffers.clear();
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

    public LispString getDefaultDirectory () {
        return new LispString(getBufferCurrentForEditing().getDefaultDirectory());
    }

    public void buryBuffer (LispBuffer buffer) {
        Environment main = getMainEnvironment();
        main.myBuffers.remove(buffer);
        main.myBuffers.add(0, buffer);
    }

    public LispBuffer lastBuffer () {
        return lastBuffer("");
    }

    public LispBuffer lastBuffer (String bufferName) {
        ArrayList<LispBuffer> noSpace = getBuffersWithNameNotBeginningWithSpace();
        for (int i=0; i!=noSpace.size(); ++i)
            if (!noSpace.get(i).getName().equals(bufferName))
                return noSpace.get(i);
        //todo: create and return *scratch*
        throw new NoOpenedBufferException();
    }

    public boolean containsBuffer (String bufferName) {
        return findBuffer(bufferName) != null;
    }

    //========== mini buffer ==========================

    public LispMiniBuffer getMiniBuffer () {
        LispMiniBuffer miniBuffer = (LispMiniBuffer) findBuffer(" *Minibuf-0*");
        if (miniBuffer == null)
            throw new RuntimeException("mini buffer does not exist!");
        return miniBuffer;
    }

}
