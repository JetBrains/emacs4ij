package org.jetbrains.emacs4ij.jelisp.elisp;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 5/11/12
 * Time: 10:39 AM
 * To change this template use File | Settings | File Templates.
 */
public abstract class TextPropertiesHolder {
    private List<TextPropertiesInterval> myIntervals;

    public TextPropertiesHolder () {
        myIntervals = new ArrayList<>();
    }

    public boolean noTextProperties() {
        return myIntervals.isEmpty();
    }

    protected abstract int size();

    protected void setTextProperties (List<TextPropertiesInterval> intervals) {
        myIntervals = intervals;
    }

    /**
     * @param start = left bound of interval
     * @param end = right bound of interval
     * @param propertyList to work with
     * @return true if any properties were added or changed, false otherwise
     */
    public boolean actOnTextProperties(int start, int end, LispList propertyList, TextPropertiesInterval.Action action) {
        Range range = new Range(start, end, 0, size());
        if (range.isEmpty())
            return false;
        if (noTextProperties()) {
            switch (action) {
                case ADD:
                    myIntervals.add(new TextPropertiesInterval(range.getStart(), range.getEnd(), 0, size(), propertyList));
                    return true;
                case SET:
                    if (propertyList.equals(LispList.list(LispSymbol.ourNil, LispSymbol.ourNil)))
                        return false;
                    myIntervals.add(new TextPropertiesInterval(range.getStart(), range.getEnd(), 0, size(), propertyList));
                    return true;
                case REMOVE:case REMOVE_LIST:
                    return false;
            }
        }
        List<TextPropertiesInterval> additionalIntervals = new ArrayList<>();
        boolean changed = false;
        for (Iterator<TextPropertiesInterval> iterator = myIntervals.iterator(); iterator.hasNext(); ) {
            TextPropertiesInterval interval = iterator.next();
            if (!interval.getRange().contains(range.getStart())) {
                continue;
            }
            int oldEnd = interval.getRange().getEnd();
            changed = interval.extractIntervalAndPerformAction(range, propertyList, action, additionalIntervals);
            if (interval.hasNoProperties())
                iterator.remove();
            if (oldEnd < range.getEnd()) {
                range.setStart(oldEnd);
                continue;
            }
            break;
        }
        myIntervals.addAll(additionalIntervals);
        Collections.sort(myIntervals);
        return changed;
    }

    protected List<TextPropertiesInterval> getIntervals(){
        return myIntervals;
    }

    protected String intervalsString() {
        StringBuilder sb = new StringBuilder();
        for (TextPropertiesInterval interval: myIntervals) {
            sb.append(interval.toString()).append(" ");
        }
        return sb.toString().trim();
    }

    protected List<TextPropertiesInterval> getTextPropertiesInRange (int start, int end) {
        List<TextPropertiesInterval> list = new ArrayList<>();
        for (TextPropertiesInterval interval : myIntervals) {
            if (interval.getRange().getEnd() <= start) {
                continue;
            }
            if (interval.getRange().getStart() >= end) {
                break;
            }
            list.add(interval.extract(start, end));
        }
        Collections.sort(list);
        return list;
    }
}
