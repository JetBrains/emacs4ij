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
import java.util.Iterator;


/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/11/11
 * Time: 5:37 PM
 * To change this template use File | Settings | File Templates.
 */
public class Environment {
    protected HashMap<String, LispSymbol> mySymbols = new HashMap<String, LispSymbol>();
    protected Environment myOuterEnv;

  //  private final Environment myGlobalEnvironment;




    /**
     * Constructor for global environment
     * @param bufferFactory
     * @param project
     */
    public Environment (@NotNull LispBufferFactory bufferFactory, Object project) {
        myProject = project;
        this.myBufferFactory = bufferFactory;
        myGlobalEnvironment = this;
        setGlobal();
    }

    //for test only!!!
    public Environment () {
        this.myBufferFactory = null;
        myProject = null;
        myGlobalEnvironment = this;
        setGlobal();
    }

    public Environment (@NotNull final Environment outerEnv) {
        myOuterEnv = outerEnv;
        myGlobalEnvironment = outerEnv.getGlobalEnvironment();
        myBufferFactory = outerEnv.getBufferFactory();
        myProject = outerEnv.getProject();
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

    public boolean isSelectionManagedBySubroutine() {
        return selectionManagedBySubroutine;
    }

    public void setSelectionManagedBySubroutine(boolean selectionManagedBySubroutine) {
        this.selectionManagedBySubroutine = selectionManagedBySubroutine;
    }

    private Environment getGlobalEnvironment() {
        return myGlobalEnvironment;
    }

   /* public Environment getMainEnvironment () {
        if (myOuterEnv.equals(myGlobalEnvironment))
            return this;
        return myOuterEnv.getMainEnvironment();
    }  */

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
                if (annotation.isCmd() && annotation.interactive().equals(ourUnsetInteractiveString))
                    throw new RuntimeException("Interactive string not set! Subroutine " + name);
                mySymbols.put(name, LispSymbol.newSubroutine(name, annotation.isCmd(), annotation.interactive()));
                //System.out.print(name + ' ');
            }
        }
    }

    private void setGlobal() {
        mySymbols.put("nil", LispSymbol.ourNil);
        mySymbols.put("t", LispSymbol.ourT);
        setSubroutines();
       // mySymbols.put("*scratch*",  new LispSymbol("*scratch*", new LispBuffer("*scratch*")));
    }

    public boolean isGlobalEnvironment() {
        return myOuterEnv == null;
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
        /*if (myOuterEnv == null)
            throw new EnvironmentException("You are not allowed to change global environment!");  */
        if (isGlobalEnvironment() || containsSymbol(symbol.getName())) {
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
        if (isGlobalEnvironment() || containsSymbol(symbol.getName())) {
            LispSymbol function = mySymbols.get(symbol.getName());
            if (function == null) {
                throw new EnvironmentException("Trying to update nonexistent function!");
            }
            defineSymbol(symbol);
            return;
        }
        myOuterEnv.updateFunction(symbol);

    }

    public ArrayList<String> getCommandList () {
        Iterator iterator = mySymbols.entrySet().iterator();
        ArrayList<String> commandList = new ArrayList<String>();
        while (iterator.hasNext()) {
            LispSymbol symbol = (LispSymbol) iterator.next();
            if (BuiltinsCheck.commandp(this, symbol, null).equals(LispSymbol.ourT)) {
                commandList.add(symbol.getName());
            }

        }
        return commandList;
    }

    //============================= buffer processing =====================================

    public void defineBuffer (LispBuffer buffer) {
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
        if (!myOuterEnv.equals(myGlobalEnvironment))
            throw new RuntimeException("You cannot define service buffers outside the main environment!");
        myServiceBuffers.add(buffer);
    }

    public void updateBuffer(LispBuffer buffer) {
        myBuffers.set(getIndexByName(myBuffers, buffer.getName()), buffer);
    }

    public void updateServiceBuffer (LispBuffer buffer) {
        myServiceBuffers.set(getIndexByName(myServiceBuffers, buffer.getName()), buffer);
    }

    private LispBuffer getCurrentBuffer () {
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
        if (!isGlobalEnvironment()) {
            myOuterEnv.switchToBuffer(bufferName);
            return;
        }
        if (myBuffers.size() == 0)
            return;
        if (myBuffers.get(myBuffers.size() - 1).getName().equals(bufferName))
            return;
        int newCurrentBufferIndex = getIndexByName(myBuffers, bufferName);
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
        if (!isGlobalEnvironment())
            return myOuterEnv.findBuffer(bufferName);
        for (LispBuffer buffer: myBuffers) {
            if (buffer.getName().equals(bufferName))
                return buffer;
        }
        /*if (myOuterEnv != null)
            return myOuterEnv.findBuffer(bufferName);*/
        return null;
    }

    public LispBuffer getServiceBuffer (String bufferName) {
        //Environment main = getMainEnvironment();
        for (LispBuffer buffer: myGlobalEnvironment.myServiceBuffers) {
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

    public LispBuffer getOtherBuffer () {
        return getOtherBuffer(getBufferCurrentForEditing().getName());
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
       // Environment main = getMainEnvironment();
        LispList bufferList = new LispList();
        for (LispBuffer buffer: myGlobalEnvironment.myBuffers) {
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
       // Environment main = getMainEnvironment();
        myGlobalEnvironment.myBuffers.remove(buffer);
        myGlobalEnvironment.myBuffers.add(0, buffer);
    }

    public LispBuffer lastBuffer () {
        return lastBuffer("");
    }

    public LispBuffer lastBuffer (String bufferName) {
       // ArrayList<LispBuffer> noSpace = getBuffersWithNameNotBeginningWithSpace();
        for (int i=0; i!=myBuffers.size(); ++i)
            if (!myBuffers.get(i).getName().equals(bufferName))
                return myBuffers.get(i);
        //todo: create and return *scratch*
        throw new NoOpenedBufferException();
    }

    public boolean containsBuffer (String bufferName) {
        return findBuffer(bufferName) != null;
    }

    public boolean isDead (String bufferName) {
        //Environment main = getMainEnvironment();
        for (LispBuffer buffer: myGlobalEnvironment.myDeadBuffers) {
            if (buffer.getName().equals(bufferName))
                return true;
        }
        return false;
    }

    //========== mini buffer ==========================

    public LispMiniBuffer getMiniBuffer () {
        LispMiniBuffer miniBuffer = (LispMiniBuffer) getServiceBuffer(ourMiniBufferName);
        if (miniBuffer == null)
            throw new RuntimeException("mini buffer does not exist!");
        return miniBuffer;
    }

}
