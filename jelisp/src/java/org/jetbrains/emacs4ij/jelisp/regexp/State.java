package org.jetbrains.emacs4ij.jelisp.regexp;

import com.sun.istack.internal.Nullable;

import java.util.HashMap;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/24/12
 * Time: 5:27 PM
 * To change this template use File | Settings | File Templates.
 */
public class State {
//    private int myId;
    private boolean isFinish = false;
    private HashMap<Character, State> myTransitionTable = new HashMap<>();

//    public State (int id) {
//        myId = id;
//    }
//
//    public State (int id, boolean finish) {
//        myId = id;
//        isFinish = finish;
//    }

    public State (boolean finish) {
        isFinish = finish;
    }

    public State () {}
    
    public void addTransition (@Nullable Character symbol, State state) {
        myTransitionTable.put(symbol, state);    
    }
    
    public State move (char symbol) {
        return myTransitionTable.get(symbol);
    }

    public boolean isFinish() {
        return isFinish;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof State)) return false;

        State state = (State) o;

        if (isFinish != state.isFinish) return false;
        if (myTransitionTable != null ? !myTransitionTable.equals(state.myTransitionTable) : state.myTransitionTable != null)
            return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = (isFinish ? 1 : 0);
        result = 31 * result + (myTransitionTable != null ? myTransitionTable.hashCode() : 0);
        return result;
    }
}
