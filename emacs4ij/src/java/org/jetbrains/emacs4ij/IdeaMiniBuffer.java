package org.jetbrains.emacs4ij;

import com.intellij.openapi.editor.Editor;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

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
        write(myPrompt);
        myStatus = MiniBufferStatus.FREE;
        myEnvironment = environment;
    }

    public void setReadCommandStatus () {
        myStatus = MiniBufferStatus.READ_COMMAND;
    }

    public MiniBufferStatus getStatus() {
        return myStatus;
    }

    public void setFreeStatus() {
        myStatus = MiniBufferStatus.FREE;
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
    public void readCommand(LObject defaultValue, String startValue, boolean noMatch) {
        myStatus = MiniBufferStatus.READ_COMMAND;
        myDefaultValue = defaultValue;
        String text = ourEvalPrompt + (startValue == null ? "" : startValue) + (noMatch ? " [No match]" : "");
        write(text);
        int cursorPosition = ourEvalPrompt.length() + (startValue == null ? 0 : startValue.length());
        gotoChar(cursorPosition+1);
        //todo unchangeable prompt
        setHeaderBufferActive();
    }

    @Override
    public void readArgument(SpecialFormInteractive interactive) {
        myStatus = MiniBufferStatus.READ_ARG;
        myInteractive = interactive;
        String text = myInteractive.getPrompt() + myInteractive.getPromptDefaultValue() + ((myInteractive.getParameterStartValue() == null) ? "" : myInteractive.getParameterStartValue());
        myPrompt = text;
        int cursorPosition = text.length()+1 ;
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
        throw new WrongTypeArgumentException("stringp", defaultValue.toString());
    }

    public String readParameter () {
        return myEditor.getDocument().getText().substring(myPrompt.length());
    }

    public void hide() {
        myStatus = MiniBufferStatus.FREE;
        myPrompt = ourEvalPrompt;
        write(myPrompt);
        myDefaultValue = null;
        close();
    }

    //for test
    public void appendText (String text) {
        write (myEditor.getDocument().getText() + text);
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
                if (cmd != null) {
                    if (BuiltinsCheck.commandp(myEnvironment, cmd, null).equals(LispSymbol.ourT)) {
                        //myStatus = MiniBufferStatus.READ_ARG;
                        //return BuiltinsCore.callInteractively(myEnvironment, cmd, null, null); instead of next code

                        cmd.castToLambda(myEnvironment);
                        String interactiveString = cmd.getInteractiveString();
                        if (interactiveString == null) {
                            throw new RuntimeException("Command has null interactive string!");
                        }
                        if (interactiveString.equals("")) {
                            hide();
                            //todo: interactive string cannot be null
                            return cmd.evaluateFunction(myEnvironment, new ArrayList<LObject>());
                        }
                        myCommand = cmd;
                        myInteractive = new SpecialFormInteractive(myEnvironment, interactiveString);
                        myStatus = MiniBufferStatus.READ_ARG;
                        myInteractive.readNextArgument();
                    } else {
                        //todo: show "no match" message
                        readCommand(myDefaultValue, parameter, true);
                    }
                } else {
                    //todo: show "no match" message
                    readCommand(myDefaultValue, parameter, true);
                }
                break;
            case READ_ARG:
                myInteractive.onReadParameter(readParameter());
                if (myInteractive.isFinished()) {
                    hide();
                    myEnvironment.setArgumentsEvaluated(true);
                    return myCommand.evaluateFunction(myEnvironment, myInteractive.getArguments().getData());
                } else {
                    if (!myInteractive.isNoMatch())
                        myInteractive.readNextArgument();
                }
            case FREE:
                break;
        }
        return null;
    }
}
