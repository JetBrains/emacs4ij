package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.DoubleBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.NoBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.NoOpenedBufferException;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;

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
    private final LispBufferFactory myBufferFactory;
    private final Object myProject;

    public static final String ourMiniBufferName = " *Minibuf-0*";
    public static final String ourScratchBufferName = "*scratch*";
    public static final String ourUnsetInteractiveString = "0";

    private static GlobalEnvironment myInstance = null;

    public static void initialize (LispBufferFactory bufferFactory, Object project) {
        myInstance = new GlobalEnvironment(bufferFactory, project);
    }

    public static GlobalEnvironment getInstance () {
        return myInstance;
    }

    private GlobalEnvironment (LispBufferFactory bufferFactory, Object project) {
        myProject = project;
        myBufferFactory = bufferFactory;
        myOuterEnv = null;
        setGlobal();
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

    public LispString getDefaultDirectory () {
        return new LispString(getBufferCurrentForEditing().getDefaultDirectory());
    }

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



}
