package org.jetbrains.emacs4ij.jelisp;

import com.intellij.openapi.editor.Editor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.*;

import java.util.*;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/10/12
 * Time: 6:17 PM
 * To change this template use File | Settings | File Templates.
 */
public class WindowManager extends CyclicManager<LispWindow> {
    private final LispWindowFactory myFactory;

    public WindowManager (LispWindowFactory factory) {
        myFactory = factory;
    }

    public void onOpenToolBuffer (LispFrame frame, LispToolWindow window) {
        if (!window.isRegistered()) {
            int id = getFrameWindows(frame).size();
            window.setId(id);
            window.setFrame(frame);
            define(window);
        }
    }

    public void onOpenBuffer(LispBuffer buffer, LispFrame frame, Editor editor) {
        List<LispWindow> bufferWindowsOnFrame = getBufferWindowsOnFrame(buffer, frame);
        if (bufferWindowsOnFrame.isEmpty()) {
            int id = getFrameWindows(frame).size();
            LispWindow window = myFactory.createWindow(id, buffer, frame, editor);
            define(window);
            buffer.onOpen(editor.getDocument());
            return;
        }
        if (bufferWindowsOnFrame.size() == 1 && !bufferWindowsOnFrame.get(0).isVisible()) {
            bufferWindowsOnFrame.get(0).open(editor);
            buffer.onOpen(editor.getDocument());
            return;
        }
        throw new DoubleBufferException(buffer.getName());
    }

    @Override
    protected void throwNoOpenedItem() {
        throw new NoOpenedWindowException();
    }

    @Override
    protected void throwItemIsNotInDataSet(LispWindow item) {
        throw new UnregisteredWindowException(item.toString());
    }

    @Override
    protected void throwDuplicateItem(LispWindow item) {
        throw new DuplicateWindow(item);
    }

    public void onKillBuffer(LispBuffer buffer) {
        if (buffer instanceof LispMinibuffer)
            for (LispFrame frame: getBufferFrames(buffer)) {
                hideBufferOnFrame(buffer, frame);
            }
        else
            for (Iterator<LispWindow> iterator = myData.iterator(); iterator.hasNext(); ) {
                LispWindow window = iterator.next();
                if (window.getBuffer() == buffer)
                    iterator.remove();
            }
    }

    @NotNull
    public LispWindow getBufferLastSelectedWindow (LispBuffer buffer) {
        for (LispWindow window: myData) {
            if (window.getBuffer().equals(buffer))
                return window;
        }
        throw new NoWindowForBuffer(buffer.getName());
    }

    /**
     *
     * @param frame on which must be found window
     * @param buffer which must be displayed by the found window
     * @return the first window displaying @param buffer on @param frame
     */
    @Nullable
    public LispWindow getBufferWindowOnFrame(LispFrame frame, LispBuffer buffer) {
        for (LispWindow window: myData) {
            if (window.getBuffer() == buffer && window.getFrame() == frame)
                return window;
        }
        return null;
    }

    private List<LispWindow> getBufferWindowsOnFrame (LispBuffer buffer, LispFrame frame) {
        List<LispWindow> windows = new ArrayList<>();
        for (int i =  myData.size() - 1; i > -1; i--) {
            LispWindow window = myData.get(i);
            if (window.getFrame() == frame && window.getBuffer() == buffer)
                windows.add(window);
        }
        //verify visibility
        for (LispWindow window: windows) {
            if (!window.isVisible()) {
                assert windows.size() == 1;
                break;
            }
        }
        return windows;
    }

    @NotNull
    public LispWindow getFrameSelectedWindow (LispFrame frame) {
        for (LispWindow window: myData) {
            if (window.getFrame() == frame)
                return window;
        }
        throw new NoWindowForFrame(frame.toString());
    }

    public void deleteFrameOtherWindows (LispFrame frame, LispWindow window) {
        assert frame == window.getFrame();
        for (Iterator<LispWindow> iterator = myData.iterator(); iterator.hasNext(); ) {
            LispWindow w = iterator.next();
            if (w.getFrame() == frame && w != window)
                iterator.remove();
        }
    }

    public LispFrame[] getBufferFrames(LispBuffer buffer) {
        Set<LispFrame> frames = new LinkedHashSet<>();
        for (LispWindow window: myData) {
            if (window.getBuffer() == buffer)
                frames.add(window.getFrame());
        }
        return frames.toArray(new LispFrame[frames.size()]);
    }

    public LispBuffer[] getFrameBuffers(LispFrame frame) {
        Set<LispBuffer> buffers = new LinkedHashSet<>();
        for (LispWindow window: myData) {
            if (window.getFrame().equals(frame))
                buffers.add(window.getBuffer());
        }
        return buffers.toArray(new LispBuffer[buffers.size()]);
    }

    public List<LispWindow> getFrameWindows (LispFrame frame) {
        List<LispWindow> windows = new ArrayList<>();
        for (LispWindow window: myData) {
            if (window.getFrame() == frame)
                windows.add(window);
        }
        return windows;
    }

    /**
     * @return the last selected minibuffer window for given frame
     */
    @Nullable
    public LispWindow getFrameMinibufferWindow (LispFrame frame) {
        for (LispWindow window: myData) {
            if (window.getBuffer() instanceof LispMinibuffer && window.getFrame() == frame)
                return window;
        }
        return null;
    }

    @NotNull
    public LispWindow getEditorWindow (Editor editor) {
        for (LispWindow window: myData) {
            if (window.getEditor() == editor)
                return window;
        }
        throw new UnregisteredEditorException();
    }

    @Nullable
    public LispBuffer getFrameOtherBuffer (LispBuffer buffer, LispFrame frame, boolean invisibleBuffersPreferred) {
        LispBuffer spare = buffer;
        for (LispWindow window: myData) {
            if (window.getFrame() == frame && window.getBuffer() != buffer) {
                if ((invisibleBuffersPreferred && !window.isVisible()) || !invisibleBuffersPreferred)
                    return window.getBuffer();
                if (spare == buffer)
                    spare = window.getBuffer();
            }
        }
        //todo: in Emacs if there is no other, return *scratch*
        return spare;
    }

    public void hideBufferOnFrame(LispBuffer buffer, LispFrame frame) {
        List<LispWindow> windows = getBufferWindowsOnFrame(buffer, frame);
        if (windows.isEmpty() || (windows.size() == 1 && !windows.get(0).isVisible())) //already closed
            return;
        windows.get(0).close();
        for (int i = 1, windowsSize = windows.size(); i < windowsSize; i++) {
            LispWindow window = windows.get(i);
            window.closeTab();
            myData.remove(window);
        }
    }
}



