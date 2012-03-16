package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.DoubleBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.EnvironmentException;
import org.jetbrains.emacs4ij.jelisp.exception.NoBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.NoOpenedBufferException;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/19/11
 * Time: 8:15 PM
 * To change this template use File | Settings | File Templates.
 */
public class BufferManager {
    private List<LispBuffer> myBuffers = new ArrayList<>();
    private List<LispBuffer> myDeadBuffers = new ArrayList<>();
    private List<LispBuffer> myServiceBuffers = new ArrayList<>();
    private List<String> myRecordedBuffers = new ArrayList<>();
    private LispBufferFactory myBufferFactory = null;

    public BufferManager(LispBufferFactory bufferFactory) {
        myBufferFactory = bufferFactory;
    }
        
    public LispBuffer createBuffer (String bufferName) {
        return myBufferFactory.createBuffer(bufferName);
    }

    public LispBuffer getCurrentBuffer () {
        if (myBuffers.size() == 0)
            throw new NoOpenedBufferException();
        return myBuffers.get(myBuffers.size() - 1);
    }

    private int getIndexByName(List<LispBuffer> buffers, String bufferName) {
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

    public boolean containsBuffer (String bufferName) {
        return findBuffer(bufferName) != null;
    }
    
    public LispBuffer findBufferSafe (String bufferName) {
        LispBuffer buffer = findBuffer(bufferName);
        if (buffer == null)
            throw new NoBufferException(bufferName);
        return buffer;
    }

    public boolean defineBuffer(LispBuffer buffer) {
        if (containsBuffer(buffer.getName())) {
            throw new DoubleBufferException("double "+buffer.getName());
        }
        if (!isDead(buffer.getName())) {
            myBuffers.add(buffer);
            return true;
        }
        myDeadBuffers.remove(getIndexByName(myDeadBuffers, buffer.getName()));
        return false;
    }

    public void defineServiceBuffer (LispBuffer buffer) {
        myServiceBuffers.add(buffer);
    }

    public List<LispBuffer> getBuffers () {
        return myBuffers;
    }

    public LispBuffer getOtherBuffer (String bufferName) {
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
        ArrayList<LispObject> bufferList = new ArrayList<>();
        for (LispBuffer buffer: myBuffers) {
            bufferList.add(buffer);
        }
        return LispList.list(bufferList);
    }
    
    public void removeBuffer (LispBuffer buffer) {
        myBuffers.remove(buffer);
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

    public void buryBuffer (LispBuffer buffer) {
        myBuffers.remove(buffer);
        myBuffers.add(0, buffer);
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
    
    public void clearRecorded() {
        for (String name: myRecordedBuffers) {
            GlobalEnvironment.INSTANCE.removeBuffer(name);
        }
        myRecordedBuffers.clear();
    }
    
    public void startRecording() {
        myRecordedBuffers.clear();
    }
    
    public void defineBufferLocalVariable (LispSymbol symbol) {
        for (LispBuffer buffer: myBuffers) {
            buffer.defineLocalVariable(symbol, true);
        }
    }

}
