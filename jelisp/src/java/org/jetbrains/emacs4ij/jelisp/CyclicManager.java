package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/24/12
 * Time: 4:08 PM
 *
 * This is a generic manager for buffers, windows and frames.
 * It handles data objects so that the current one is the last one.
 */

abstract class CyclicManager<T> implements Manager<T> {
    protected final List<T> myData = new ArrayList<>();

    protected abstract void throwNoOpenedItem();
    protected abstract void throwItemIsNotInDataSet(T item);
    protected abstract void throwDuplicateItem(T item);

    @NotNull
    @Override
    public final T getCurrent () {
        if (myData.isEmpty())
            throwNoOpenedItem();
        return myData.get(0);
    }

    @NotNull
    @Override
    public final T switchTo (T item) {
        if (myData.isEmpty())
            throwNoOpenedItem();
        if (myData.get(0).equals(item))
            return myData.get(0);
        int newCurrentItemIndex = myData.indexOf(item);
        if (newCurrentItemIndex == -1)
            throwItemIsNotInDataSet(item);
        Collections.rotate(myData.subList(0, newCurrentItemIndex + 1), 1);
        return myData.get(0);
    }

    @Override
    public final void bury (T item) {
        if (myData.remove(item))
            myData.add(item);
    }

    @Override
    public final List<T> getData() {
        return myData;
    }

    @Override
    public final int getSize() {
        return myData.size();
    }

    @Override
    public final void remove (T item) {
        if (!myData.remove(item))
            throwItemIsNotInDataSet(item);
    }

    @Override
    public final void clear() {
        myData.clear();
    }

    @Override
    public final boolean isAlive (T item) {
        return myData.contains(item);
    }

    @Override
    public final boolean isEmpty() {
        return myData.isEmpty();
    }

    @Override
    public final boolean define(T item) {
        if (myData.contains(item))
            throwDuplicateItem(item);
        myData.add(0, item);
        return true;
    }
}
