package org.jetbrains.emacs4ij;

import com.intellij.openapi.editor.Editor;
import org.jetbrains.emacs4ij.jelisp.BufferManager;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.elisp.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.DoubleBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.InternalException;
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
public class BufferManagerImpl implements BufferManager {
    private List<LispBuffer> myBuffers = new ArrayList<>();
    private List<LispBuffer> myDeadBuffers = new ArrayList<>();
    private List<LispBuffer> myServiceBuffers = new ArrayList<>();

    public BufferManagerImpl() {}

    public LispBuffer createBuffer (String bufferName) {
        String baseDir = ((LispString) GlobalEnvironment.INSTANCE.getBufferCurrentForEditing().getLocalVariableValue("default-directory")).getData();
        return new IdeaBuffer(GlobalEnvironment.INSTANCE, bufferName, baseDir, null);
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

    public LispBuffer switchToWindow(String bufferName, Editor editor) {
        LispBuffer current = switchToBuffer(bufferName);
        if (current != null)
            current.switchToEditor(editor);
        return current;
    }

    public LispBuffer switchToBuffer(String bufferName) {
        if (myBuffers.size() == 0)
            return null;
        if (myBuffers.get(myBuffers.size() - 1).getName().equals(bufferName)) {
            return myBuffers.get(myBuffers.size() - 1);
        }
        int newCurrentBufferIndex = getIndexByName(myBuffers, bufferName);
        if (newCurrentBufferIndex == -1)
            throw new NoBufferException(bufferName);
        Collections.rotate(myBuffers.subList(newCurrentBufferIndex, myBuffers.size()), -1);
        return myBuffers.get(myBuffers.size() - 1);
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

    public LispBuffer findBuffer (Editor editor) {
        for (LispBuffer buffer: myBuffers) {
            if (buffer.containsEditor(editor))
                return buffer;
        }
        return null;
    }

    @Override
    public boolean defineBuffer(LispBuffer buffer) {
        if (containsBuffer(buffer.getName())) {
            LispBuffer existing = findBuffer(buffer.getName());
            if (!buffer.hasWindows())
                throw new DoubleBufferException(buffer.getName());
            if (existing.getDocument() != buffer.getDocument()) {
                throw new InternalException(JelispBundle.message("two.buffers.one.name"));
            }
            existing.mergeEditors(buffer);
            return true;
        }
        if (!isDead(buffer.getName())) {
            myBuffers.add(buffer);
            return true;
        }
        LispBuffer buried = myDeadBuffers.get(getIndexByName(myDeadBuffers, buffer.getName()));
        buried.setEditor(buffer.getEditor());
        myBuffers.add(buried);
        myDeadBuffers.remove(buried);
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
        throw new InternalException("other-buffer " + bufferName);
    }

    public int getBuffersSize() {
        return myBuffers.size();
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

    public List<String> getBuffersNames () {
        List<String> list = new ArrayList<>();
        for (LispBuffer buffer: myBuffers) {
            list.add(buffer.getName());
        }
        return list;
    }

    public List<String> getBuffersNames (String begin) {
        List<String> bufferNamesList = new ArrayList<>();
        for (LispBuffer buffer: myBuffers) {
            String bufferName = buffer.getName();
            if (bufferName.length() >= begin.length()) {
                if (bufferName.substring(0, begin.length()).equals(begin)) {
                    bufferNamesList.add(bufferName);
                }
            }
        }
        return bufferNamesList;
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

    @Override
    public boolean isDead (String bufferName) {
        for (LispBuffer buffer: myDeadBuffers) {
            if (buffer.getName().equals(bufferName))
                return true;
        }
        return false;
    }

    public void defineBufferLocalVariable (LispSymbol symbol) {
        for (LispBuffer buffer: myBuffers) {
            buffer.defineLocalVariable(symbol, true);
        }
    }

    @Override
    public void closeCurrentBuffer() {
        removeBuffer(getCurrentBuffer());
    }
}

