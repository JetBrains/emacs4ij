package org.jetbrains.emacs4ij;

import com.intellij.ide.IdeEventQueue;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.event.DocumentEvent;
import com.intellij.openapi.editor.event.DocumentListener;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.editor.ex.FocusChangeListener;
import com.intellij.openapi.fileTypes.FileTypes;
import com.intellij.ui.EditorTextField;
import com.intellij.util.Alarm;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.UnregisteredBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.interactive.InteractiveReader;
import org.jetbrains.emacs4ij.jelisp.platform_dependent.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.platform_dependent.LispKeymap;
import org.jetbrains.emacs4ij.jelisp.platform_dependent.LispMinibuffer;
import org.jetbrains.emacs4ij.jelisp.subroutine.Minibuffer;

import java.awt.*;
import java.awt.event.KeyEvent;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/23/11
 * Time: 4:01 PM
 * To change this template use File | Settings | File Templates.
 */
public final class IdeaMiniBuffer extends IdeaBuffer implements LispMinibuffer {
    private static IdeaMiniBuffer myInstance;

    private int myActivationsDepth = 0;
    private InteractiveReader myInteractive;
    private Integer myCharCode = null;
    private Alarm myAlarm;
    private LispBuffer myParent;
    private boolean isOpened = false;
    private boolean exitOnSuccess = false;
    private static Deque<InteractiveReader> myInteractiveStack = new ArrayDeque<>();

    private IdeaMiniBuffer (int number, Editor editor, Environment environment, LispBuffer parent) {
        super(" *Minibuf-" + number + '*', environment, editor, null);
        myParent = parent;
        myAlarm = new Alarm();
    }

    public static void init (@Nullable Editor editor, @NotNull Environment environment) {
        if (myInstance != null)
            return;
        myInstance = new IdeaMiniBuffer(0, editor, environment, null);
    }

    public static IdeaMiniBuffer getInstance() {
        return myInstance;
    }

    private final DocumentListener myMiniBufferChangedListener = new DocumentListener() {
        @Override
        public void beforeDocumentChange(DocumentEvent documentEvent) {
        }

        @Override
        public void documentChanged(DocumentEvent documentEvent) {
            documentEvent.getDocument().removeDocumentListener(this);
            myAlarm.cancelAllRequests();
            if (myInteractive.isNoMatch()) {
                String newText = documentEvent.getDocument().getText();
                int k = newText.indexOf(myInteractive.getNoMatchMessage());
                if (k < 0)
                    return;
                newText = newText.substring(0, k);
                write(newText);
            }
        }
    };

    private final FocusChangeListener myFocusListener = new FocusChangeListener() {
        private LispKeymap myOldKeymap;

        @Override
        public void focusGained(Editor editor) {
            myOldKeymap = myEnvironment.getActiveKeymap();
            myEnvironment.setActiveKeymap("minibuffer-local-completion-map");
        }

        @Override
        public void focusLost(Editor editor) {
            myEnvironment.setActiveKeymap(myOldKeymap);
        }
    };

    private void cancelNoMatchMessageUpdate() {
        myAlarm.cancelAllRequests();
    }

    @Override
    public void addCharListener () {
        IdeEventQueue.getInstance().addActivityListener(new Runnable() {
            @Override
            public void run() {
                IdeEventQueue ideEventQueue = IdeEventQueue.getInstance();
                AWTEvent currentEvent = ideEventQueue.getTrueCurrentEvent();
                if (currentEvent instanceof KeyEvent) {
                    if (currentEvent.getID() == KeyEvent.KEY_PRESSED) {
                        if (((KeyEvent) currentEvent).getKeyChar() != KeyEvent.CHAR_UNDEFINED) {
                            myCharCode = ((KeyEvent) currentEvent).getKeyCode();
                            ((KeyEvent) currentEvent).consume();
                            ideEventQueue.removeActivityListener(this);
                            onReadInput();
                        }
                    }
                }
            }
        });
    }

    @Override
    public List<String> getCompletions (String parameter) {
        return myInteractive.getCompletions(parameter);
    }

    @Override
    public void setInputStartValue (String startValue) {
        myInteractive.setInitialInput(startValue);
    }

    private void clearNoMatch () {
        getDocument().addDocumentListener(myMiniBufferChangedListener);
        myAlarm.addRequest(new Runnable() {
            @Override
            public void run() {
                getDocument().removeDocumentListener(myMiniBufferChangedListener);
                String text = getDocument().getText();
                String prompt = myInteractive.getPrompt();
                if (myInteractive.isNoMatch()
                        && text.endsWith(myInteractive.getNoMatchMessage())
                        && text.startsWith(prompt)) {
                    String input = text.substring(prompt.length(), text.length() - myInteractive.getNoMatchMessage().length());
                    write(prompt + input);
                }
            }
        }, 3000);
    }

    @Override
    public void updateEditorText() {
        String text = myInteractive.getPrompt() +
                ((myInteractive.getInitialInput() == null) ? "" : myInteractive.getInitialInput());
        write(text);
    }

    /*
        ** for usage from SpecialFormInteractive only
     */
    @Override
    public void readParameter (@NotNull InteractiveReader interactive) {
        if (!isOpened) {
            open(myEnvironment.getCurrentNonToolBuffer());
            if (myActivationsDepth > 1) {
                myEnvironment.killBuffer(this);
                GlobalEnvironment.echo(Emacs4ijBundle.message("call.interactively.message"), GlobalEnvironment.MessageType.WARNING);
                return;
            }
        }

        if (myInteractive.toShowSpecialNoMatchMessage()) {
            write(myInteractive.getNoMatchMessage());
            clearNoMatch();
            return;
        }
        String text = myInteractive.getPrompt()
                + (myInteractive.getInitialInput() == null ? "" : myInteractive.getInitialInput());
        int cursorPosition = text.length()+1;
        text += myInteractive.getNoMatchMessage();
        write(text);

        gotoChar(cursorPosition);
        //todo unchangeable prompt
        setActive();

        clearNoMatch();
    }

    //is public only for test!
    public LispSymbol returnDefault(LispObject defaultValue) {
        if (defaultValue == null)
            return new LispSymbol("");
        if (defaultValue instanceof LispSymbol) {
            if (!defaultValue.equals(LispSymbol.ourNil))
                return (LispSymbol) defaultValue;
            return new LispSymbol("");
        }
        if (defaultValue instanceof LispString) {
            String name = ((LispString) defaultValue).getData();
            LispSymbol ret = myEnvironment.find(name);
            if (ret == null) {
                return new LispSymbol(name);
            }
            return ret;
        }
        if (defaultValue instanceof LispList) {
            defaultValue = ((LispList) defaultValue).car();
            if (!(defaultValue instanceof LispList))
                return returnDefault(defaultValue);
        }
        throw new WrongTypeArgumentException("stringp", defaultValue.toString());
    }

    @Override
    public String readInputString() {
        if (myCharCode != null) {
            String code = myCharCode.toString();
            myCharCode = null;
            return code;
        }
        String text = getDocument().getText();
        int k = myInteractive.isNoMatch()
                ? text.lastIndexOf(myInteractive.getNoMatchMessage())
                : text.length();
        if (k < 0)
            k = text.length();
        if (myInteractive.getPrompt() == null)
            System.out.print(1);
        return text.substring(myInteractive.getPrompt().length(), k);
    }

    @Override
    public void kill() {
//        setReadCommandStatus();
//        myPrompt = ourEvalPrompt;
        exitOnSuccess = false;
        cancelNoMatchMessageUpdate();
//        write(myPrompt);
//        myWindowManager.closeAll();
        myActivationsDepth = 0;
        if (myParent != null) {
            myParent.closeHeader();
            myParent = null;
        }
        isOpened = false;
        myDocument = null;
        System.out.println("kill minibuffer");
//        todo: I should do this way, probably. So when 1 minibuffer is closed, the previous one is recovered
//        myActivationsDepth--;
    }

    @Override
    public int getActivationsDepth() {
        return myActivationsDepth;
    }

    private void open(LispBuffer parent) {
        if (parent.isToolBuffer())
            throw new Emacs4ijFatalException(Emacs4ijBundle.message("open.minibuffer.in.tool.window"));

        final EditorTextField input = new EditorTextField("", ourProject, FileTypes.PLAIN_TEXT);
        ((IdeaBuffer)parent).getEditor().setHeaderComponent(input);
        myParent = parent;
        input.setEnabled(true);
        Editor editor = input.getEditor();
        if (editor != null) {
            ((EditorEx) editor).addFocusListener(myFocusListener);
            openStandardBuffer(editor);
            ((EditorEx) editor).setBackgroundColor(Color.GREEN);
            myActivationsDepth++;
            isOpened = true;
            setActive();
        }
    }

    //for test
    public void appendText (String text) {
        write (getDocument().getText() + text);
    }

    @Override
    public void setNoMatch(String input) {
        myInteractive.setNoMatch(input);
    }

    @Override
    public void onReadInput () {
        myInteractive.onReadParameter(readInputString());
        if (myInteractive.isFinished()) {
            runInteractive();
        } else if (!myInteractive.isNoMatch())
            myInteractive.readNextArgument();
    }

    @Override
    public void onInteractiveNoIoInput (InteractiveReader interactive) {
        if (interactive.getCommand().equals(new LispSymbol("exit-minibuffer"))) {
            exitOnSuccess = true;
            onReadInput();
            return;
        }
        if (myInteractive != interactive) {
            if (myInteractive != null && !myInteractive.isFinished())
                myInteractiveStack.push(myInteractive);
            myInteractive = interactive;
        }
        if (myInteractive.isFinished()) {
            runInteractive();
            return;
        }
        myInteractive.readNextArgument();
    }

    private void runInteractive() {
        if (isOpened && exitOnSuccess) {
            myEnvironment.killBuffer(this);
        }
        InteractiveReader current = myInteractive;
        myInteractive = !myInteractiveStack.isEmpty() ? myInteractiveStack.removeFirst() : null;
        Minibuffer.goOn(current);
    }

    @Override
    public void setActive() {
        if (myEnvironment.findBuffer(myName) == null)
            throw new UnregisteredBufferException(myName);
        try {
//            myEnvironment.setBufferCurrentForEditing(this);
            getEditor().getContentComponent().grabFocus();
        } catch (NullPointerException e) {
            throw new Emacs4ijFatalException("Null editor!");
        }
    }

    public void message (final String text) {
        getDocument().addDocumentListener(myMiniBufferChangedListener);
        myAlarm.addRequest(new Runnable() {
            @Override
            public void run() {
                getDocument().removeDocumentListener(myMiniBufferChangedListener);
                appendText(text);
            }
        }, 3000);
    }
}
