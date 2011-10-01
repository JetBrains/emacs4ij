package org.jetbrains.emacs4ij.jelisp.elisp;

import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/1/11
 * Time: 2:36 PM
 * To change this template use File | Settings | File Templates.
 */
public class ArgumentsList {
    private class Argument {
        private boolean myOptional = false;
        private Object myValue = null;

        public Argument (Object value) {
            myValue = value;
        }

        public Argument (boolean optional) {
            myOptional = optional;
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
    }

    private ArrayList<Argument> myList = new ArrayList<Argument>();
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

    public void add (boolean optional) {
        myList.add(new Argument(optional));
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

    public void setValues (Object[] values) {
        myList = new ArrayList<Argument>();
        myRequiredSize = values.length;
        for (int i=0; i!=values.length; ++i) {
            myList.add(new Argument(values[i]));
        }
    }
}
