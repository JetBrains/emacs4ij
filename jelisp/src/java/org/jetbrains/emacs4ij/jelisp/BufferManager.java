package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.exception.DoubleBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.InternalException;
import org.jetbrains.emacs4ij.jelisp.exception.NoOpenedBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.UnregisteredBufferException;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispBufferFactory;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispMinibuffer;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispToolWindow;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/19/11
 * Time: 8:15 PM
 * To change this template use File | Settings | File Templates.
 */
final class BufferManager extends CyclicManager<LispBuffer> {
    private final LispBufferFactory myFactory;

    public BufferManager(LispBufferFactory factory) {
        myFactory = factory;
    }

    public LispBuffer createBuffer (String bufferName) {
        String baseDir = ((LispString) GlobalEnvironment.INSTANCE.getBufferCurrentForEditing()
                .getVariableValue("default-directory")).getData();
//        VirtualFile file = VirtualFileManager.getInstance().findFileByUrl(baseDir + bufferName);
//        if (file == null)
            throw new InternalException("create buffer failed");
//        return myFactory.createBuffer(GlobalEnvironment.INSTANCE, file, null);
    }

    public LispBuffer createBuffer (String bufferName, LispToolWindow window) {
        String baseDir = ".";
        try {
            LispObject defaultDir = GlobalEnvironment.INSTANCE.getBufferCurrentForEditing().getVariableValue("default-directory");
            if (defaultDir instanceof LispString)
                baseDir = ((LispString) defaultDir).getData();
        } catch (NoOpenedBufferException e) {
            //skip
        }
        return myFactory.createBuffer(GlobalEnvironment.INSTANCE, bufferName, baseDir, window);
    }

    @Nullable
    public LispBuffer findBuffer (String bufferName) {
        for (LispBuffer buffer: myData) {
            if (buffer.getName().equals(bufferName))
                return buffer;
        }
        return null;
    }

    public List<String> getBuffersNames () {
        List<String> list = new ArrayList<>();
        for (LispBuffer buffer: myData) {
            list.add(buffer.getName());
        }
        return list;
    }

    public List<String> getBuffersNames (final String begin) {
        List<String> bufferNamesList = new ArrayList<>();
        for (LispBuffer buffer: myData) {
            String bufferName = buffer.getName();
            if (bufferName.length() >= begin.length()) {
                if (bufferName.substring(0, begin.length()).equals(begin)) {
                    bufferNamesList.add(bufferName);
                }
            }
        }
        return bufferNamesList;
    }

//    public void setVariableBufferLocal(LispSymbol symbol) {
//        for (LispBuffer buffer: myData) {
//            buffer.defineVariable(symbol);
//        }
//    }

    @Override
    protected void throwNoOpenedItem() {
        throw new NoOpenedBufferException();
    }

    @Override
    protected void throwItemIsNotInDataSet(LispBuffer item) {
        throw new UnregisteredBufferException(item.getName());
    }

    @Override
    protected void throwDuplicateItem(LispBuffer item) {
        throw new DoubleBufferException(item.getName());
    }

    public boolean containsBuffer (String bufferName) {
        return findBuffer(bufferName) != null;
    }

    public LispBuffer findBufferSafe (String bufferName) {
        LispBuffer buffer = findBuffer(bufferName);
        if (buffer == null)
            throw new UnregisteredBufferException(bufferName);
        return buffer;
    }

    public void killBuffer (LispBuffer buffer) {
        buffer.kill();
        remove(buffer);
    }

    public LispMinibuffer getMinibuffer() {
        for (LispBuffer buffer: myData) {
            if (buffer instanceof LispMinibuffer)
                return (LispMinibuffer) buffer;
        }
        return null;
    }

    //for test
    public LispBuffer getBufferByIndex (int index) {
        return myData.get(index);
    }

    @Nullable
    public LispBuffer getCurrentNonToolBuffer() {
        for (LispBuffer buffer : myData) {
            if (buffer.isToolBuffer())
                continue;
            return buffer;
        }
        return null;
    }
}

