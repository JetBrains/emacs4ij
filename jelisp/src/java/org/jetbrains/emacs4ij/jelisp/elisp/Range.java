package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.exception.InternalException;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 5/11/12
 * Time: 12:23 PM
 * To change this template use File | Settings | File Templates.
 */
public class Range implements Comparable<Range> {
    private int myStart;
    private int myEnd;

    public Range (int leftBound, int rightBound, int min, int max) {
        myStart = normalize(leftBound, min, max);
        myEnd = normalize(rightBound, min, max);
        swapIfNeeded();
    }

    public Range (int leftBound, int rightBound) {
        myStart = leftBound;
        myEnd = rightBound;
        swapIfNeeded();
    }

    public Range (Range range) {
        myStart = range.myStart;
        myEnd = range.myEnd;
    }

    private void swapIfNeeded() {
        if (myStart > myEnd) {
            int tmp = myStart;
            myStart = myEnd;
            myEnd = tmp;
        }
    }

    private static int normalize(int what, int min, int max) {
        return what > max ? max : what < min ? min : what;
    }

    public void set (Range range) {
        myStart = range.myStart;
        myEnd = range.myEnd;
    }

    public void setStart (int start) {
        if (start >= myEnd)
            throw new InternalException(JelispBundle.message("invalid.range", start, myEnd));
        myStart = start;
    }

    public void setEnd (int end) {
        if (myStart >= end)
            throw new InternalException(JelispBundle.message("invalid.range", myStart, end));
        myEnd = end;
    }

    public int getStart() {
        return myStart;
    }

    public int getEnd() {
        return myEnd;
    }

    public boolean isEmpty () {
        return myStart >= myEnd;
    }

    public boolean equals (int start, int end) {
        return myStart == start && myEnd == end;
    }

    @Override
    public String toString() {
        return myStart + " " + myEnd;
    }

    @Override
    public int compareTo(Range o) {
        return myStart < o.myStart ? -1 : myStart == o.myStart ? 0 : 1;
    }

    public boolean contains (int index) {
        return index >= myStart && index < myEnd;
    }
}
