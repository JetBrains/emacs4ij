package org.jetbrains.emacs4ij;

import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import org.apache.commons.lang.NotImplementedException;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/4/11
 * Time: 11:10 AM
 * To change this template use File | Settings | File Templates.
 */
public class IdeaEditor extends LispObject implements LispBuffer {
    private Editor myEditor;

    public IdeaEditor (Editor editor) {
        myEditor = editor;
    }

    @Override
    public LispString toLispString() {
        throw new NotImplementedException();  //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public LispObject evaluate(Environment environment) {
        throw new NotImplementedException();  //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public String getName() {
        return FileDocumentManager.getInstance().getFile(myEditor.getDocument()).getName();
    }

    @Override
    public int getSize() {
        return myEditor.getCaretModel().getOffset();
    }

    @Override
    public int point() {
        return myEditor.getCaretModel().getOffset();
    }
}
