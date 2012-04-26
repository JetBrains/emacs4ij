package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.annotations.NotNull;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/24/12
 * Time: 5:28 PM
 *
 */

interface Manager<T> {
    boolean define (T item);
    @NotNull T getCurrent () ;
    T switchTo (T item);
//    T getOther (T item);
    void bury (T item);
    List<T> getData();
    int getSize();
    void remove (T item);
    void clear();
    boolean isAlive (T item);
    boolean isEmpty();
}
