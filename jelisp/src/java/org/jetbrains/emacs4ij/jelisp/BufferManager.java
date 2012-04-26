package org.jetbrains.emacs4ij.jelisp;

import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.VirtualFileManager;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.DoubleBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.InternalException;
import org.jetbrains.emacs4ij.jelisp.exception.NoOpenedBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.UnregisteredBufferException;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/19/11
 * Time: 8:15 PM
 * To change this template use File | Settings | File Templates.
 */
public class BufferManager extends CyclicManager<LispBuffer> {
    private final LispBufferFactory myFactory;

    public BufferManager(LispBufferFactory factory) {
        myFactory = factory;
    }

    public LispBuffer createBuffer (String bufferName) {
        String baseDir = ((LispString) GlobalEnvironment.INSTANCE.getBufferCurrentForEditing()
                .getLocalVariableValue("default-directory")).getData();

        VirtualFile file = VirtualFileManager.getInstance().findFileByUrl(baseDir + bufferName);
        if (file == null)
            throw new InternalException("create buffer failed");

        return myFactory.createBuffer(GlobalEnvironment.INSTANCE, file, null);
    }

    public LispBuffer findBuffer (String bufferName) {
        for (LispBuffer buffer: myData) {
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
            throw new UnregisteredBufferException(bufferName);
        return buffer;
    }

    public void killBuffer (LispBuffer buffer) {
        buffer.kill();
        remove(buffer);
    }

    public List<String> getBuffersNames () {
        List<String> list = new ArrayList<>();
        for (LispBuffer buffer: myData) {
            list.add(buffer.getName());
        }
        return list;
    }

    public List<String> getBuffersNames (String begin) {
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

    public void defineBufferLocalVariable (LispSymbol symbol) {
        for (LispBuffer buffer: myData) {
            buffer.defineLocalVariable(symbol, true);
        }
    }

    public LispMinibuffer getMinibuffer() {
        for (LispBuffer buffer: myData) {
            if (buffer instanceof LispMinibuffer)
                return (LispMinibuffer) buffer;
        }
        return null;
    }

    @Override
    protected void throwNoOpenedItem() {
        throw new NoOpenedBufferException();
    }

    @Override
    protected void throwItemIsNotInDataSet(LispBuffer buffer) {
        throw new UnregisteredBufferException(buffer.getName());
    }

    @Override
    protected void throwDuplicateItem(LispBuffer item) {
        throw new DoubleBufferException(item.getName());
    }

    public LispBuffer getBufferByIndex (int index) {
        return myData.get(index);
    }
}

