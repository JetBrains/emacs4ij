package org.jetbrains.emacs4ij.jelisp.regexp;

import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/24/12
 * Time: 5:27 PM
 * To change this template use File | Settings | File Templates.
 */
public class StateMachine {
    private ArrayList<State> myStates = new ArrayList<>();
    private State myCurrentState = null;
    private State myStart = new State();
    private State myFinish = new State(true);
    private int start = 0;
    
    private boolean isInitialState(State state) {
        return state.equals(myStates.get(0));
    }
    
    private boolean isCommonSpecial(char i) {
        return false;
    }
    
    private boolean hasNext (String s, int i) {
        return i + 1 < s.length();
    }   
    
    public StateMachine (String pattern) {
        myCurrentState = myStart;
        myStates.add(myStart);
        myStates.add(myFinish);
        if (pattern.length() == 0) {
            myStart.addTransition(null, myFinish);
            return;
        }

        ArrayList<State> toProcess = new ArrayList<>();
        toProcess.add(myStart);
        
        for (int i = 0; i < pattern.length(); ++i) {            
            char c = pattern.charAt(i);
            
//            for (int index = 0; i < )
//            State state = toProcess.get(index);
//            switch (c) {
//                case '.':
//                    break;
//            }
            
        }
        
        
        

    }
    
    public int match (String source) {
        for (int i = 0, dataLength = source.length(); i < dataLength; i++) {
            char c = source.charAt(i);
            if (isInitialState(myCurrentState)) {
                start = i;
            }
            myCurrentState = myCurrentState.move(c);
            if (myCurrentState == null) {
                myCurrentState = myStart;
                //don'read next
                continue;
            }
            if (myCurrentState.isFinish()) {
                return start;
            }
        }
        return -1;
    }
}
