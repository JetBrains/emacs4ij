package org.jetbrains.emacs4ij;

import com.intellij.ide.IdeEventQueue;
import com.intellij.openapi.actionSystem.CustomShortcutSet;
import com.intellij.openapi.actionSystem.KeyboardShortcut;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.event.DocumentEvent;
import com.intellij.openapi.editor.event.DocumentListener;
import com.intellij.ui.EditorTextField;
import com.intellij.util.Alarm;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.NoBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/23/11
 * Time: 4:01 PM
 * To change this template use File | Settings | File Templates.
 */
public class IdeaMiniBuffer extends IdeaBuffer implements LispMiniBuffer {
    public enum MiniBufferStatus {READ_COMMAND, READ_ARG}
    public static String ourEvalPrompt = "M-x ";
    private MiniBufferStatus myStatus;
    private int myActivationsDepth = 0;
    private SpecialFormInteractive myInteractive;
    private String myPrompt = ourEvalPrompt;
    private LispSymbol myCommand;
    private Integer myCharCode = null;
    private Alarm myAlarm;
    private LispBuffer myParent;

    private boolean isDocumentListenerSet = false;

    private final DocumentListener myMiniBufferChangedListener = new DocumentListener() {
        @Override
        public void beforeDocumentChange(DocumentEvent documentEvent) {
        }
        @Override
        public void documentChanged(DocumentEvent documentEvent) {
            documentEvent.getDocument().removeDocumentListener(this);
            isDocumentListenerSet = false;
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
    
    private void cancelNoMatchMessageUpdate() {
        myAlarm.cancelAllRequests();
        if (isDocumentListenerSet) {
            getDocument().removeDocumentListener(myMiniBufferChangedListener);
            isDocumentListenerSet = false;
        }
    }

    public IdeaMiniBuffer (int number, Editor editor, Environment environment, LispBuffer parent) {
        myName = " *Minibuf-" + number + '*';
        setEditor(editor);
        myEnvironment = environment;
        myParent = parent;
        setReadCommandStatus();
        myAlarm = new Alarm();
        myEnvironment.defineServiceBuffer(this);
    }

    private void setDefaultInteractive () {
        myInteractive = new SpecialFormInteractive(myEnvironment, 'C'+ourEvalPrompt);
    }

    @Override
    public void setReadCommandStatus () {
        myStatus = MiniBufferStatus.READ_COMMAND;
        setDefaultInteractive();
    }

    private void setReadArgumentStatus () {
        myStatus = MiniBufferStatus.READ_ARG;
    }

    @Override
    public void setEditor(Editor editor) {
        super.setEditor(editor);
        //todo: on hide save state of interactive input and restore here.
        if (editor != null)
            write(myPrompt);
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

    public List<String> getCompletions (String parameter) {
        return myInteractive.getCompletions(parameter);
    }

    public void setInputStartValue (String startValue) {
        myInteractive.setParameterStartValue(startValue);
    }

    private void clearNoMatch () {
        getDocument().addDocumentListener(myMiniBufferChangedListener);
        isDocumentListenerSet = true;
        myAlarm.addRequest(new Runnable() {
            @Override
            public void run() {
                getDocument().removeDocumentListener(myMiniBufferChangedListener);
                isDocumentListenerSet = false;
                write(myPrompt + ((myInteractive.getParameterStartValue() == null) ? "" : myInteractive.getParameterStartValue()));
            }
        }, 3000);
    }

   /*
     ** for outer usage plugin
     */
    @Override
    public void startRead () {
        myInteractive.readNextArgument();
    }

    public void updateEditorText() {
        String text = myInteractive.getPrompt() + myInteractive.getPromptDefaultValue() + ((myInteractive.getParameterStartValue() == null) ? "" : myInteractive.getParameterStartValue());
        write(text);
    }

    /*
        ** for usage from SpecialFormInteractive only
     */
    @Override
    public void readParameter (@NotNull SpecialFormInteractive interactive) {
        myPrompt = myInteractive.getPrompt() + myInteractive.getPromptDefaultValue();

        if (myInteractive.toShowNoMatchMessage()) {
            write(myInteractive.getNoMatchMessage());
            clearNoMatch();
            return;
        }

        String text = myInteractive.getPrompt() + myInteractive.getPromptDefaultValue() + ((myInteractive.getParameterStartValue() == null) ? "" : myInteractive.getParameterStartValue());
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
        return text.substring(myPrompt.length(), k);
    }

    @Override
    public void kill() {
        setReadCommandStatus();
        myPrompt = ourEvalPrompt;
        cancelNoMatchMessageUpdate();
        write(myPrompt);
        myWindowManager.closeAll();
        myActivationsDepth = 0;
        if (myParent != null) {
            myParent.closeHeader();
            myParent = null;
        }
//        todo: I should do this way, probably. So when 1 minibuffer is closed, the previous one is recovered
//        myActivationsDepth--;
    }

    @Override
    public int getActivationsDepth() {
        return myActivationsDepth;
    }

    @Override
    public void open(LispBuffer parent) {
        EditorTextField input = new EditorTextField();
        parent.getEditor().setHeaderComponent(input);
        myParent = parent;
        input.setEnabled(true);
        setEditor(input.getEditor());


        InterruptMiniBuffer imb = new InterruptMiniBuffer();
        imb.registerCustomShortcutSet(new CustomShortcutSet
                (new KeyboardShortcut(KeyStroke.getKeyStroke("ESCAPE"), KeyStroke.getKeyStroke("ESCAPE"))),
                input.getComponent());

        ExecuteCommand command = new ExecuteCommand();
        command.registerCustomShortcutSet(KeyEvent.VK_ENTER, 0, input.getComponent());

        AutoComplete autoComplete = new AutoComplete();
        autoComplete.registerCustomShortcutSet(KeyEvent.VK_TAB, 0, input.getComponent());
        setActive();
    }

    @Override
    public boolean wasInteractiveFormResult() {
        return myCommand == null;
    }

    //for test
    public void appendText (String text) {
        write (getDocument().getText() + text);
    }

    public void setNoMatch(String input) {
        myInteractive.setNoMatch(input);
    }

    @Override
    public LispObject onReadInput () {
        switch (myStatus) {
            case READ_COMMAND:
                myInteractive.onReadParameter(readInputString());
                if (myInteractive.isFinished()) {
                    LispSymbol cmd = (LispSymbol) myInteractive.getArguments().get(0);
                    String interactiveString = cmd.getInteractiveString();
                    if (interactiveString == null) {
                        throw new Emacs4ijFatalException(Emacs4ijBundle.message("interactive.string.error", cmd.getName()));
                    }
                    myCommand = cmd;
                    if (interactiveString.equals("")) {
                        kill();
                        return myCommand.evaluateFunction(myEnvironment, new ArrayList<LispObject>());
                    }
                    myInteractive = new SpecialFormInteractive(myEnvironment, interactiveString);
                    setReadArgumentStatus();
                    myInteractive.readNextArgument();
                } else {
                    if (!myInteractive.isNoMatch())
                        throw new Emacs4ijFatalException(Emacs4ijBundle.message("interactive.read.error"));
                }
                break;
            case READ_ARG:
                myInteractive.onReadParameter(readInputString());
                if (myInteractive.isFinished()) {
                    return runInteractive();
                } else {
                    if (!myInteractive.isNoMatch())
                        myInteractive.readNextArgument();
                }
                break;
        }
        return null;
    }
    
    private LispObject onInteractiveNoIoInput (@Nullable LispSymbol command, SpecialFormInteractive interactive) {
        myCommand = command;
        myInteractive = interactive;
        if (myInteractive.isFinished()) {
            return runInteractive();
        } else {
            myStatus = MiniBufferStatus.READ_ARG;
            open(myEnvironment.getBufferCurrentForEditing());
            if (myActivationsDepth > 1) {
                kill();
                GlobalEnvironment.showInfoMessage(Emacs4ijBundle.message("call.interactively.message"));
                return null;
            }
            myInteractive.readNextArgument();
        }
        return null;        
    }
    
    @Override
    public LispObject onInteractiveNoIoInput (SpecialFormInteractive interactive) {
        return onInteractiveNoIoInput(null, interactive);    
    }

    private void shiftPrefixArgs() {
        myEnvironment.setVariable(new LispSymbol("last-prefix-arg",
                myEnvironment.find("current-prefix-arg").getValue()));
        myEnvironment.setVariable(new LispSymbol("current-prefix-arg",
                myEnvironment.find("prefix-arg").getValue()));
        myEnvironment.setVariable(new LispSymbol("prefix-arg",
                LispSymbol.ourNil));
    }

    private void clearPrefixArgs() {
        myEnvironment.setVariable(new LispSymbol("last-prefix-arg",
                myEnvironment.find("current-prefix-arg").getValue()));
        myEnvironment.setVariable(new LispSymbol("current-prefix-arg",
                LispSymbol.ourNil));
    }

    @Override
    public LispObject onInteractiveCall(Environment environment, LispSymbol command) {
        return onInteractiveNoIoInput(command,
                new SpecialFormInteractive(environment, ((LispCommand)command.getFunction()).getInteractiveString()));
    }

    private LispObject runInteractive() {
        myEnvironment.setArgumentsEvaluated(true);
        if (myCommand != null && myCommand.getName().equals("execute-extended-command")) {
            LispSymbol cmd = (LispSymbol) myInteractive.getArguments().get(0);
            String interactiveString = cmd.getInteractiveString();
            if (interactiveString == null) {
                throw new Emacs4ijFatalException(Emacs4ijBundle.message("interactive.string.error", cmd.getName()));
            }
            myCommand = cmd;

            shiftPrefixArgs();

            if (interactiveString.equals("")) {
                kill();
                return myCommand.evaluateFunction(myEnvironment, new ArrayList<LispObject>());
            }
            myInteractive = new SpecialFormInteractive(myEnvironment, interactiveString);
            setReadArgumentStatus();
            myInteractive.readNextArgument();
            return null;
        }
        clearPrefixArgs();
        LispObject result = myCommand == null
                ? LispList.list(myInteractive.getArguments())
                : myCommand.evaluateFunction(myEnvironment, myInteractive.getArguments());
        kill();
        return result;
    }

    @Override
    public void setActive() {
        if (myEnvironment.getServiceBuffer(myName) == null)
            throw new NoBufferException(myName);
        if (!myWindowManager.getSelectedWindow().hasEditor())
            throw new Emacs4ijFatalException("Null editor!");
        getEditor().getContentComponent().grabFocus();
        myActivationsDepth++;
    }
}
