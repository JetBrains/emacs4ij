package org.jetbrains.emacs4ij;

import com.intellij.ide.IdeEventQueue;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.event.DocumentEvent;
import com.intellij.openapi.editor.event.DocumentListener;
import com.intellij.util.Alarm;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

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

   // private LObject myDefaultValue;
    private SpecialFormInteractive myInteractive;
    private String myPrompt;
    private LispSymbol myCommand;

    private Integer myCharCode = null;
    private Alarm myAlarm;

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
                newText = newText.substring(0, k);
                write(newText);
            }
        }
    };

    private void cancelNoMatchMessageUpdate() {
        myAlarm.cancelAllRequests();
        if (isDocumentListenerSet) {
            myEditor.getDocument().removeDocumentListener(myMiniBufferChangedListener);
            isDocumentListenerSet = false;
        }
    }

    public IdeaMiniBuffer (int number, Editor editor, CustomEnvironment environment) {
        myName = " *Minibuf-" + number + '*';
        myEditor = editor;
        myEnvironment = environment;
        setReadCommandStatus();
        myPrompt = ourEvalPrompt;
        myAlarm = new Alarm();
       // write(myPrompt);
    }

    private void setDefaultInteractive () {
        myInteractive = new SpecialFormInteractive(myEnvironment, 'C'+ourEvalPrompt);
    }

    private void setReadCommandStatus () {
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
       // myAlarm.cancelAllRequests();

        myEditor.getDocument().addDocumentListener(myMiniBufferChangedListener);
        isDocumentListenerSet = true;

        myAlarm.addRequest(new Runnable() {
            @Override
            public void run() {
                myEditor.getDocument().removeDocumentListener(myMiniBufferChangedListener);
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
        setBufferActive();

        clearNoMatch();
    }

    //is public only for test!
    public LispSymbol returnDefault(LObject defaultValue) {
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
        int k = myInteractive.isNoMatch() ? myEditor.getDocument().getText().lastIndexOf(myInteractive.getNoMatchMessage()) : myEditor.getDocument().getText().length();
        if (k < 0)
            k = myEditor.getDocument().getText().length();
        return myEditor.getDocument().getText().substring(myPrompt.length(), k);
    }

    public void hide() {
        setReadCommandStatus();
        myPrompt = ourEvalPrompt;
        cancelNoMatchMessageUpdate();
        write(myPrompt);
        close();
    }

    //for test
    public void appendText (String text) {
        write (myEditor.getDocument().getText() + text);
    }

    public void setNoMatch(String input) {
        myInteractive.setNoMatch(input);
    }

    private void viewResult (LObject result) {
        //todo: it is only for testing stage
//        Messages.showInfoMessage(result.toString(), "Evaluation result");
    }

    @Override
    public LObject onReadInput () {
        switch (myStatus) {
            case READ_COMMAND:
                myInteractive.onReadParameter(readInputString());
                if (myInteractive.isFinished()) {
                    LispSymbol cmd = (LispSymbol) myInteractive.getArguments().car();
                    //cmd.castToLambda(myEnvironment);
                    String interactiveString = cmd.getInteractiveString(myEnvironment);
                    if (interactiveString == null) {
                        throw new RuntimeException("Command has null interactive string!");
                    }
                    if (interactiveString.equals("")) {
                        hide();
                        LObject result = cmd.evaluateFunction(myEnvironment, new ArrayList<LObject>());
                        viewResult(result);
                        return result;
                    }
                    myCommand = cmd;
                    myInteractive = new SpecialFormInteractive(myEnvironment, interactiveString);
                    setReadArgumentStatus();
                    myInteractive.readNextArgument();
                } else {
                    if (!myInteractive.isNoMatch())
                        throw new RuntimeException("impossible situation!");
                }
                break;
            case READ_ARG:
                myInteractive.onReadParameter(readInputString());
                if (myInteractive.isFinished()) {
                    myEnvironment.setArgumentsEvaluated(true);
                    LObject result =  myCommand.evaluateFunction(myEnvironment, myInteractive.getArguments().toLObjectList());
                    hide();
                    viewResult(result);
                    return result;
                } else {
                    if (!myInteractive.isNoMatch())
                        myInteractive.readNextArgument();
                }
                break;
        }
        return null;
    }

    @Override
    public LObject onInteractiveNoIoInput (SpecialFormInteractive interactive) {
        //note: myStatus == READ_ARG
        myInteractive = interactive;
        if (myInteractive.isFinished()) {
            myEnvironment.setArgumentsEvaluated(true);
            LObject result = myCommand.evaluateFunction(myEnvironment, myInteractive.getArguments().toLObjectList());
            hide();

            viewResult(result);
            return result;

        } else {
            if (!myInteractive.isNoMatch())
                myInteractive.readNextArgument();
        }
        return null;
    }
}
