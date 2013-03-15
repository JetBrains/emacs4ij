package org.jetbrains.emacs4ij.jelisp.platformDependent;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public abstract class LispFrame implements LispObject {
  private Map<LispSymbol, LispObject> myParameters = new HashMap<>();
  private Map<LispSymbol, LispObject> myFaceAList = new HashMap<>();

  public abstract void setVisible (boolean visible);
  public abstract void setIconified (boolean iconified);

  public LispFrame() {
    setParameter("visibility", LispSymbol.T);
    setParameter("buffer-predicate", LispSymbol.NIL);
  }

  @Override
  public LispObject evaluate(Environment environment) {
    return this;
  }

  public Map<LispSymbol, LispObject> getFacesAList() {
    return myFaceAList;
  }

  public boolean isIconified() {
    return getParameter("visibility").equals(new LispSymbol("icon"));
  }

  public boolean isVisible() {
    return getParameter("visibility").equals(LispSymbol.T);
  }

  public LispObject getParameter(LispSymbol parameter) {
    LispObject value = myParameters.get(parameter);
    if (value == null)
      throw new VoidVariableException(parameter.getName());
    return value;
  }

  public void setParameter(LispSymbol name, LispObject value) {
    myParameters.put(name, value);
  }

  public LispList getParameters() {
    List<LispObject> list = new ArrayList<>();
    for (Map.Entry<LispSymbol, LispObject> parameter: myParameters.entrySet()) {
      LispList item = LispList.list(parameter.getKey());
      item.append(parameter.getValue());
      list.add(item);
    }
    return LispList.list(list);
  }

  protected final void setParameter(String name, LispObject value) {
    myParameters.put(new LispSymbol(name), value);
  }

  private LispObject getParameter(String parameter) {
    return getParameter(new LispSymbol(parameter));
  }
}
