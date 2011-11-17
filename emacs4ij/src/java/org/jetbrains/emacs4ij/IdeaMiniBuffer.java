package org.jetbrains.emacs4ij;

import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.event.DocumentEvent;
import com.intellij.openapi.editor.event.DocumentListener;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/23/11
 * Time: 4:01 PM
 * To change this template use File | Settings | File Templates.
 */
public class IdeaMiniBuffer extends IdeaEditor implements LispMiniBuffer {
    public enum MiniBufferStatus {READ_COMMAND, READ_ARG}
    public static String ourEvalPrompt = "M-x ";
    private MiniBufferStatus myStatus;

   // private LObject myDefaultValue;
    private SpecialFormInteractive myInteractive;
    private String myPrompt;
    private LispSymbol myCommand;

    public IdeaMiniBuffer (int number, Editor editor, Environment environment) {
        myName = " *Minibuf-" + number + '*';
        myEditor = editor;
        myEnvironment = environment;
        setReadCommandStatus();
        myPrompt = ourEvalPrompt;
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
    public String getDefaultDirectory() {
        throw new RuntimeException("Wrong usage!");
    }

    @Override
    public void setEditor(Editor editor) {
        super.setEditor(editor);
        //todo: on hide save state of interactive input and restore here.
        write(myPrompt);
    }

    public List<String> getCompletions (String parameter) {
        return myInteractive.getCompletions(parameter);
    }

    public void setInputStartValue (String startValue) {
        myInteractive.setParameterStartValue(startValue);
    }

    private void clearNoMatch () {
        final Thread t = new Thread(new Runnable() {
            @Override
            public void run() {
                try {
                    Thread.sleep(3000);
                } catch (InterruptedException e) {
                    //just stop waiting
                }
                write(myPrompt + ((myInteractive.getParameterStartValue() == null) ? "" : myInteractive.getParameterStartValue()));
            }
        });
        DocumentListener documentListener = new DocumentListener() {
            @Override
            public void beforeDocumentChange(DocumentEvent documentEvent) {
                documentEvent.getDocument().removeDocumentListener(this);
                t.interrupt();
            }
            @Override
            public void documentChanged(DocumentEvent documentEvent) {
            }
        };
        myEditor.getDocument().addDocumentListener(documentListener);
        t.start();
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
        int k = myInteractive.isNoMatch() ? myEditor.getDocument().getText().lastIndexOf(myInteractive.getNoMatchMessage()) : myEditor.getDocument().getText().length();
        if (k < 0)
            k = myEditor.getDocument().getText().length();
        return myEditor.getDocument().getText().substring(myPrompt.length(), k);
    }

    public void hide() {
        setReadCommandStatus();
        myPrompt = ourEvalPrompt;
        write(myPrompt);
        close();
    }

    //for test
    public void appendText (String text) {
        write (myEditor.getDocument().getText() + text);
    }

    @Override
    public LObject onReadInput () {
        switch (myStatus) {
            case READ_COMMAND:
                myInteractive.onReadParameter(readInputString());
                if (myInteractive.isFinished()) {
                    LispSymbol cmd = (LispSymbol) myInteractive.getArguments().car();
                    cmd.castToLambda(myEnvironment);
                    String interactiveString = cmd.getInteractiveString();
                    if (interactiveString == null) {
                        throw new RuntimeException("Command has null interactive string!");
                    }
                    if (interactiveString.equals("")) {
                        hide();
                        return cmd.evaluateFunction(myEnvironment, new ArrayList<LObject>());
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
                    LObject result =  myCommand.evaluateFunction(myEnvironment, myInteractive.getArguments().getData());
                    hide();
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
            LObject result =  myCommand.evaluateFunction(myEnvironment, myInteractive.getArguments().getData());
            hide();
            return result;
        } else {
            if (!myInteractive.isNoMatch())
                myInteractive.readNextArgument();
        }
        return null;
    }
}
