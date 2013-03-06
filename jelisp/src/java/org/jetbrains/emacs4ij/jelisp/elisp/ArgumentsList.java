package org.jetbrains.emacs4ij.jelisp.elisp;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

final class ArgumentsList {
  private class Argument {
    private boolean myOptional = false;
    private Object myValue = null;
    private Type myType = null;

    public Argument (boolean optional, Type type) {
      myOptional = optional;
      myType = type;
    }

    public void setValue (Object value) {
      myValue = value;
    }

    public boolean isOptional () {
      return myOptional;
    }

    public Object getValue () {
      return myValue;
    }

    public Type getType () {
      return myType;
    }
  }

  private List<Argument> myList = new ArrayList<Argument>();
  private int myRequiredSize = 0;

  public ArgumentsList () { }

  public void setRequiredSize (int requiredSize) {
    myRequiredSize = requiredSize;
  }

  public int getRequiredSize() {
    return myRequiredSize;
  }

  public void setValue (int index, Object value) {
    myList.get(index).setValue(value);
  }

  public boolean isOptional (int index) {
    return myList.get(index).isOptional();
  }

  public void add (boolean optional, Type type) {
    myList.add(new Argument(optional, type));
  }

  public int getSize () {
    return myList.size();
  }

  public Object[] getValues() {
    Object values[] = new Object[myList.size()];
    if (!myList.isEmpty())
      for (int i=0; i!=myList.size(); ++i) {
        values[i] = myList.get(i).getValue();
      }
    return values;
  }

  public Type getType (int index) {
    return myList.get(index).getType();
  }
}
