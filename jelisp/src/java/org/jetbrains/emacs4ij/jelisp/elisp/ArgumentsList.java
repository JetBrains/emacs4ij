package org.jetbrains.emacs4ij.jelisp.elisp;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Arrays;

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
        private Type myType = null;

        public Argument (Object value) {
            myValue = value;
        }

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
                if (values[i] == null) {
                    System.out.println("    null");
                    continue;
                }
                if (values[i].getClass().isArray()) {
                    System.out.println("    " + Arrays.toString((LObject[])values[i]));//.toString();
                    continue;
                }
                System.out.println("    " + values[i].toString());
            }
        System.out.println("--------");
        return values;
    }

    public Type getType (int index) {
        return myList.get(index).getType();
    }

    /*public void setValues (Object[] values) {
        myList = new ArrayList<Argument>();
        myRequiredSize = values.length;
        for (int i=0; i!=values.length; ++i) {
            myList.add(new Argument(values[i]));
        }
    } */
}
