package org.jetbrains.emacs4ij.jelisp.elisp;

import com.intellij.openapi.editor.Editor;
import org.jetbrains.emacs4ij.jelisp.Environment;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/3/11
 * Time: 5:19 PM
 * To change this template use File | Settings | File Templates.
 */
public interface LispBuffer extends LObject {
    public Editor getEditor();
    public void setEditor (Editor editor);
    public String getName();
    public int getSize();
    public int point();
    public int pointMin();
    public int pointMax();
    public int bufferEnd(double parameter);
    public String gotoChar(int position);
    public String forwardChar (int shift);
    public void setBufferActive ();
    public void grabFocus();
    public boolean isAlive();
    public void kill();

    public Environment getEnvironment();
    public String getHeaderName();
    public void closeHeader();


    public String getDefaultDirectory();

}
