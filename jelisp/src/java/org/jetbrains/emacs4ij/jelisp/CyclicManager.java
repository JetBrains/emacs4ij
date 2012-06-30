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

public abstract class CyclicManager<T> implements Manager<T> {
    protected List<T> myData = new ArrayList<>();

    protected abstract void throwNoOpenedItem();
    protected abstract void throwItemIsNotInDataSet(T item);
    protected abstract void throwDuplicateItem(T item);

    @NotNull
    @Override
    public T getCurrent () {
        if (myData.isEmpty())
            throwNoOpenedItem();
        return myData.get(0);
    }

    @NotNull
    @Override
    public T switchTo (T item) {
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

//    @Override
    //note: invalid!!!
//    public T getOther (T item) {
//        if (myData.isEmpty())
//            throwNoOpenedItem();
//        if (myData.size() == 1)
//            return myData.get(0);
//        for (int i = myData.size() - 1; i != -1; --i) {
//            if (!myData.get(i).equals(item))
//                return myData.get(i);
//        }
//        throw new InternalException("other " + item.getClass().getSimpleName() + ": " + item);
//    }

    @Override
    public void bury (T item) {
        if (myData.remove(item))
            myData.add(item);
    }

    @Override
    public List<T> getData() {
        return myData;
    }

    @Override
    public int getSize() {
        return myData.size();
    }

    @Override
    public void remove (T item) {
        if (!myData.remove(item))
            throwItemIsNotInDataSet(item);
    }

    @Override
    public void clear() {
        myData.clear();
    }

    @Override
    public boolean isAlive (T item) {
        return myData.contains(item);
    }

    @Override
    public boolean isEmpty() {
        return myData.isEmpty();
    }

    @Override
    public boolean define(T item) {
        if (myData.contains(item))
            throwDuplicateItem(item);
        myData.add(0, item);
        return true;
    }
}
