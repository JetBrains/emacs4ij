package org.jetbrains.emacs4ij.jelisp;

import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA.
 * User: ekaterina.polishchuk
 * Date: 7/8/11
 * Time: 3:46 PM
 *
 * This class represents a lisp program
 */
public class LispProgram {

    private ArrayList<String> myConstant;
    private ArrayList<String> mySpecialForms; //this list must be common for every program
    private ArrayList<String> myVariables;
    private StringBuilder myStackTrace;

    public void appendConstant () {

    }


}
