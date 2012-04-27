package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.ArgumentOutOfRange;
import org.jetbrains.emacs4ij.jelisp.exception.NoMatchData;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;
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

    private static List<LispObject> myLastMatch = null;

    public static void clearHistory() {
        myLastMatch = null;
    }

    public static void registerSearchResult (@NotNull Matcher matcher) {
        try {
            myLastMatch = new ArrayList<>();
            myLastMatch.add(new LispInteger(matcher.start()));
            myLastMatch.add(new LispInteger(matcher.end()));
            for (int i = 1; i <= matcher.groupCount(); i++) {
                myLastMatch.add(new LispInteger(matcher.start(i)));
                myLastMatch.add(new LispInteger(matcher.end(i)));
            }
        } catch (IllegalStateException e) {
            myLastMatch = null;
        }
    }

    private static LispObject getMatch (LispInteger subExp, boolean start) {
        if (myLastMatch == null)
            throw new NoMatchData();
        int index = subExp.getData();
        if (index < 0)
            throw new ArgumentOutOfRange(subExp, 0);
        try {
            return myLastMatch.get(index*2 + (start ? 0 : 1));
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
        if (myLastMatch == null)
            throw new NoMatchData();
        if (!Predicate.isNil(integers)) {
            for (ListIterator<LispObject> iterator = myLastMatch.listIterator(); iterator.hasNext(); ) {
                LispObject object = iterator.next();
                if (object instanceof LispMarker)
                    iterator.set(new LispInteger(((LispMarker) object).getPosition()));
            }
        }
        LispList reuseList = reuse instanceof LispList ? (LispList) reuse : null;
        if (reuseList == null)
            return LispList.list(myLastMatch);
        resetMarkersInListIfNeeded(reset, reuseList);
        ((LispList) reuse).resetWith(LispList.list(myLastMatch));
        return (LispList) reuse;
    }

    @Subroutine("set-match-data")
    public static LispList setMatchData (LispObject list, @Optional LispObject reset) {
        if (list instanceof LispList) {
            resetMarkersInListIfNeeded(reset, (LispList) list);
            List<LispObject> objects = ((LispList) list).toLispObjectList();
            if (!objects.isEmpty() && Predicate.isIntegerOrMarker(objects.get(0))) {
                for (LispObject item: objects) {
                    if (!Predicate.isIntegerOrMarker(item))
                        throw new WrongTypeArgumentException("interger-or-marker-p", item);
                }
                myLastMatch = objects;
            }
        } else if (list.equals(LispSymbol.ourNil)) {
            myLastMatch = new ArrayList<>();
        } else {
            throw new WrongTypeArgumentException("listp", list);
        }
        return LispList.list(myLastMatch);
    }
}
