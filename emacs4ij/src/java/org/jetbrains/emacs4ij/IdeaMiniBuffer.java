package org.jetbrains.emacs4ij;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Editor;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgument;

import java.awt.*;
import java.util.ArrayList;

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
    private LispSymbol myCommand;

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

    private void hide () {
        IdeaEditor buffer = (IdeaEditor) myEnvironment.getFirstBufferWithNameNotBeginningWithSpace();
        IdeaEditor.headerClosed(this);
        buffer.setBufferActive();
    }

    @Override
    public void readCommand(LObject defaultValue, String startValue, boolean noMatch) {
        myStatus = MiniBufferStatus.READ_COMMAND;
        myDefaultValue = defaultValue;
        String text = ourEvalPrompt + (startValue == null ? "" : startValue) + (noMatch ? " [No match]" : "");
        write(text);
        int cursorPosition = ourEvalPrompt.length() + (startValue == null ? 0 : startValue.length());
        gotoChar(cursorPosition);
        //todo unchangeable prompt
        setHeaderBufferActive();
    }

    @Override
    public void readArgument(SpecialFormInteractive interactive) {
        myStatus = MiniBufferStatus.READ_ARG;
        myInteractive = interactive;
        String text = myInteractive.getPrompt() + ((myInteractive.getParameterStartValue() == null) ? "" : myInteractive.getParameterStartValue());
        int cursorPosition = text.length();
        if (myInteractive.isNoMatch()) {
            text += " [No Match]";
        }
        write(text);
        gotoChar(cursorPosition);
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
                        //myStatus = MiniBufferStatus.READ_ARG;
                        //return BuiltinsCore.callInteractively(myEnvironment, cmd, null, null); instead of next code

                        cmd.castToLambda(myEnvironment);
                        String interactiveString = cmd.getInteractiveString();
                        if (interactiveString == null || interactiveString.equals("")) {
                            hide();
                            return cmd.evaluateFunction(myEnvironment, new ArrayList<LObject>());
                        }
                        myCommand = cmd;
                        SpecialFormInteractive interactive = new SpecialFormInteractive(myEnvironment, interactiveString);
                        myStatus = MiniBufferStatus.READ_ARG;
                        interactive.readNextArgument();
                    }
                //todo: show "no match" message
                readCommand(myDefaultValue, parameter, true);
                break;
            case READ_ARG:
                myInteractive.onReadParameter(readParameter());
                if (myInteractive.isFinished()) {
                    myStatus = MiniBufferStatus.READ_COMMAND;
                    hide();
                    return myCommand.evaluateFunction(myEnvironment, myInteractive.getArguments().getData());
                } else
                    myInteractive.readNextArgument();
            case FREE:
                break;
        }
        return null;
    }
}
