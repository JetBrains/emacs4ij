package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.ArgumentOutOfRange;
import org.jetbrains.emacs4ij.jelisp.exception.NoMatchData;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/27/12
 * Time: 11:03 AM
 * To change this template use File | Settings | File Templates.
 */
public abstract class Search {
    private Search() {}

    private static List<Integer> myLastMatch = new ArrayList<>();
    private static LispBuffer myBuffer = null;

    public static void clearHistory() {
        myLastMatch.clear();
        myBuffer = null;
    }

    public static void registerSearchResult (@NotNull Matcher matcher) {
        clearHistory();
        try {
            myLastMatch.add(matcher.start());
            myLastMatch.add(matcher.end());
            for (int i = 1; i <= matcher.groupCount(); i++) {
                myLastMatch.add(matcher.start(i));
                myLastMatch.add(matcher.end(i));
            }
        } catch (IllegalStateException e) {
            myLastMatch.clear();
        }
    }

    private static LispObject getMatch (LispInteger subExp, boolean start) {
        if (myLastMatch.isEmpty())
            throw new NoMatchData();
        int index = subExp.getData();
        if (index < 0)
            throw new ArgumentOutOfRange(subExp, 0);
        try {
            Integer value = myLastMatch.get(index * 2 + (start ? 0 : 1));
            if (value == null)
                return LispSymbol.ourNil;
            return myBuffer == null
                    ? new LispInteger(value)
                    : new LispMarker(value, myBuffer);
        } catch (IndexOutOfBoundsException e) {
            return LispSymbol.ourNil;
        }
    }

    @Subroutine("match-beginning")
    public static LispObject matchBeginning (LispInteger subExp) {
        return getMatch(subExp, true);
    }

    @Subroutine("match-end")
    public static LispObject matchEnd (LispInteger subExp) {
        return getMatch(subExp, false);
    }

    /**
     * if (reset & list contains any marker) then reset all that markers to nowhere
     */
    private static void resetMarkersInListIfNeeded (LispObject reset, LispList list) {
        if (Predicate.isNil(reset))
            return;
        for (LispObject item: list.toLispObjectList()) {
            if (item instanceof LispMarker) {
                ((LispMarker) item).set(null, null);
            }
        }
    }

    @Subroutine("match-data")
    public static LispList matchData(@Optional LispObject integers, LispObject reuse, LispObject reset) {
        List<LispObject> result = new ArrayList<>();
        if (!Predicate.isNil(integers)) {
            for (Integer item: myLastMatch) {
                result.add(item == null ? LispSymbol.ourNil : new LispInteger(item));
            }
            if (myBuffer != null)
                result.add(myBuffer);
        } else if (myBuffer != null) {
            for (Integer item: myLastMatch) {
                result.add(item == null ? LispSymbol.ourNil : new LispMarker(item, myBuffer));
            }
        } else {
            for (Integer item: myLastMatch) {
                result.add(item == null ? LispSymbol.ourNil : new LispInteger(item));
            }
        }

        LispList reuseList = reuse instanceof LispList ? (LispList) reuse : null;
        if (reuseList == null)
            return LispList.list(result);

        resetMarkersInListIfNeeded(reset, reuseList);

        ((LispList) reuse).resetWith(LispList.list(result));
        return (LispList) reuse;
    }

    private static int getPosition (MarkerOrInteger item) {
        Integer position = item.getPosition();
        return position == null
                ? myBuffer == null ? 0 : 1
                : position;
    }

    @Subroutine("set-match-data")
    public static LispSymbol setMatchData (LispObject list, @Optional LispObject reset) {
        if (list instanceof LispList) {
            List<LispObject> objects = ((LispList) list).toLispObjectList();
            if (!objects.isEmpty() && Predicate.isIntegerOrMarker(objects.get(0))) {
                LispBuffer buffer = null;
                for (int j = 0, objectsSize = objects.size(); j < objectsSize; j++) {
                    LispObject item = objects.get(j);
                    if (!Predicate.isIntegerOrMarker(item))
                        throw new WrongTypeArgumentException("interger-or-marker-p", item);
                    if (item instanceof LispMarker && ((LispMarker) item).getBuffer() != null
                            && j % 2 == 0 && j + 1 < objectsSize)
                        buffer = ((LispMarker) item).getBuffer();
                }
                myLastMatch.clear();
                myBuffer = buffer;
                for (int i = 0; i + 1 < objects.size(); i += 2) {
                    myLastMatch.add(getPosition((MarkerOrInteger) objects.get(i)));
                    myLastMatch.add(getPosition((MarkerOrInteger) objects.get(i + 1)));
                }
                resetMarkersInListIfNeeded(reset, (LispList) list);
            }
        } else if (list.equals(LispSymbol.ourNil)) {
            myLastMatch.clear();
        } else {
            throw new WrongTypeArgumentException("listp", list);
        }
        return LispSymbol.ourNil;
    }
}
