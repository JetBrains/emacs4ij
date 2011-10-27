package org.jetbrains.emacs4ij;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Editor;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgument;

import java.awt.*;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/23/11
 * Time: 4:01 PM
 * To change this template use File | Settings | File Templates.
 */
public class IdeaMiniBuffer extends IdeaEditor implements LispMiniBuffer {
    public enum MiniBufferStatus {FREE, READ, READ_COMMAND, READ_ARG}
    public static String ourEvalPrompt = "M-x ";
    private MiniBufferStatus myStatus;

    private LObject myDefaultValue;
    private SpecialFormInteractive myInteractive;
    private String myPrompt;


    public IdeaMiniBuffer (int number, Editor editor, Environment environment) {
        myName = " *Minibuf-" + number + '*';
        myEditor = editor;
        myPrompt = ourEvalPrompt;
        if (myEditor != null)
            myEditor.getDocument().setText(myPrompt);
        myStatus = MiniBufferStatus.READ_COMMAND;
        myEnvironment = environment;
    }

    /*public IdeaMiniBuffer (int number, EditorTextField editor, Environment environment) {
        myName = " *Minibuf-" + number;
        myEditor = (Editor) editor;
        myStatus = MiniBufferStatus.READ_COMMAND;
        myEnvironment = environment;
        myPrompt = ourEvalPrompt;
    }   */

    private void write (final String text) {
        if (myEditor == null)
            return;
        if (EventQueue.isDispatchThread()) {
            ApplicationManager.getApplication().runWriteAction(new Runnable() {
            @Override
            public void run() {
                myEditor.getDocument().setText(text);
                gotoChar(pointMax());
            }
        });
        }
        else
            EventQueue.invokeLater (new Runnable() {
                @Override
                public void run() {
                    ApplicationManager.getApplication().runWriteAction(new Runnable() {
            @Override
            public void run() {
                myEditor.getDocument().setText(text);
                gotoChar(pointMax());
            }
        });
                }
            });

    }

    public void setReadCommandStatus () {
        myStatus = MiniBufferStatus.READ_COMMAND;
    }

    @Override
    public String getDefaultDirectory() {
        throw new RuntimeException("Wrong usage!");
    }

    @Override
    public void setEditor(Editor editor) {
        super.setEditor(editor);
        write(myPrompt);
    }

    @Override
    public void readCommand(LObject defaultValue) {
        myStatus = MiniBufferStatus.READ_COMMAND;
        myDefaultValue = defaultValue;
        write(ourEvalPrompt);
        //todo unchangeable prompt
        setBufferActive();
    }

    @Override
    public void readArgument(SpecialFormInteractive interactive) {
        myStatus = MiniBufferStatus.READ_ARG;
        myInteractive = interactive;
        String text = myInteractive.getPrompt() + ((myInteractive.getParameterStartValue() == null) ? "" : myInteractive.getParameterStartValue());
        if (myInteractive.isNoMatch()) {
            text += " [No Match]";
        }
        write(text);
        setBufferActive();
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
        throw new WrongTypeArgument("stringp", defaultValue.toString());
    }

    private String readParameter () {
        return myEditor.getDocument().getText().substring(myPrompt.length());
    }

    //todo: autocompletion while type
    @Override
    public LObject onReadInput () {
        switch (myStatus) {
            case READ:
                break;
            case READ_COMMAND:
                String parameter = readParameter();
                if (parameter.equals("")) {
                    return returnDefault(myDefaultValue);
                }
                LispSymbol cmd = myEnvironment.find(parameter);
                if (cmd != null)
                    if (BuiltinsCheck.commandp(myEnvironment, cmd, null).equals(LispSymbol.ourT)) {
                        myStatus = MiniBufferStatus.READ_ARG;
                        return BuiltinsCore.callInteractively(myEnvironment, cmd, null, null);
                    }
                //todo: no match
                readCommand(myDefaultValue);
                break;
            case READ_ARG:
                myInteractive.onReadParameter(readParameter());
                if (myInteractive.isFinished()) {


                }
            case FREE:
                break;
        }
        return null;
    }
}
