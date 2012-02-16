package org.jetbrains.emacs4ij.jelisp.elisp;

import com.intellij.openapi.editor.Editor;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/3/11
 * Time: 5:19 PM
 * To change this template use File | Settings | File Templates.
 */
public interface LispBuffer extends LObject {
    public void showMessage (String message);

    public LObject getLocalVariableValue (String name);
    public LispSymbol getLocalVariable (String name);
    public void defineLocalVariable (LispSymbol variable);
    public void defineLocalVariable (LispSymbol variable, boolean noValue);

    public String getName();
    public int getSize();
    public int point();
    public int pointMin();
    public int pointMax();
    public int bufferEnd(double parameter);
    public String gotoChar(int position);
    public String forwardChar (int shift);
  //  public boolean isAlive();
    public void kill();
    ////public String getDefaultDirectory();
    
    public LObject evaluateLastForm ();

    public void setBufferActive ();
    public Editor getEditor();
    public void setEditor (Editor editor);
    public void closeHeader();

    public void addMarker (LispMarker marker);
    public void removeMarker (LispMarker marker);
    public boolean hasMarkersAt (int position);

   // public Integer getMark();
    //public void setMark (int position);
    //public void pushMark (@Nullable Integer position, boolean activate);
    //public void popMark ();
}
