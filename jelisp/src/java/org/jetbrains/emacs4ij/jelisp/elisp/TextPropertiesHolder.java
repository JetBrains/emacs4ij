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
//        Collections.sort(myIntervals);
    }

    /**
     * Adds properties
     * @param start = left bound of interval
     * @param end = right bound of interval
     * @param propertyList to add
     * @return true if any properties were added or changed, false otherwise
     */
    public boolean addTextProperties(int start, int end, LispList propertyList) {
        Range range = new Range(start, end, 0, size());
        if (range.isEmpty())
            return false;
        if (noTextProperties()) {
            myIntervals.add(new TextPropertiesInterval(range.getStart(), range.getEnd(), 0, size(), propertyList));
            return true;
        }
        List<TextPropertiesInterval> additionalIntervals = new ArrayList<>();
        boolean changed = false;
        for (TextPropertiesInterval interval : myIntervals) {
            if (!interval.getRange().contains(range.getStart())) {
                continue;
            }
            int oldEnd = interval.getRange().getEnd();
            changed = interval.extractIntervalAndAddProperties(range.getStart(), range.getEnd(), propertyList, additionalIntervals);
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

    /**
     * Replaces all properties in [start; end) with given propertyList
     * @param start = left bound of interval
     * @param end = right bound of interval
     * @param propertyList to set. If it is (nil nil), remove all text properties in given range
     * @return true if any properties were changed, false otherwise
     */
    public boolean setTextProperties(int start, int end, LispList propertyList) {
        Range range = new Range(start, end, 0, size());
        if (range.isEmpty())
            return false;
        if (noTextProperties()) {
            if (propertyList.equals(LispList.list(LispSymbol.ourNil, LispSymbol.ourNil))) {
                return false;
            }
            myIntervals.add(new TextPropertiesInterval(range.getStart(), range.getEnd(), 0, size(), propertyList));
            return true;
        }
        List<TextPropertiesInterval> additionalIntervals = new ArrayList<>();
        boolean changed = false;
        for (Iterator<TextPropertiesInterval> iterator = myIntervals.iterator(); iterator.hasNext(); ) {
            TextPropertiesInterval interval = iterator.next();
            if (!interval.getRange().contains(range.getStart())) {
                continue;
            }
            int oldEnd = interval.getRange().getEnd();
            changed = interval.extractIntervalAndSetProperties(range.getStart(), range.getEnd(), propertyList, additionalIntervals);
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

    /**
     * Remove properties with names from propertyList in [start; end)
     * @param start = left bound of interval
     * @param end = right bound of interval
     * @param propertyList = list of properties to be removed. Values don't matter.
     * @return true if any properties were removed, false otherwise
     */
    public boolean removeTextProperties(int start, int end, LispList propertyList) {
        Range range = new Range(start, end, 0, size());
        if (range.isEmpty())
            return false;
        if (noTextProperties()) {
            return false;
        }
        List<TextPropertiesInterval> additionalIntervals = new ArrayList<>();
        boolean changed = false;
        for (Iterator<TextPropertiesInterval> iterator = myIntervals.iterator(); iterator.hasNext(); ) {
            TextPropertiesInterval interval = iterator.next();
            if (!interval.getRange().contains(range.getStart())) {
                continue;
            }
            int oldEnd = interval.getRange().getEnd();
            changed = interval.extractIntervalAndRemoveProperties(range.getStart(), range.getEnd(), propertyList, additionalIntervals);
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

    /**
     * Remove properties with names from propertyNames in [start; end)
     * @param start = left bound of interval
     * @param end = right bound of interval
     * @param propertyNames = list of properties to be removed.
     * @return true if any properties were removed, false otherwise
     */
    public boolean removeListOfTextProperties(int start, int end, LispList propertyNames) {
        Range range = new Range(start, end, 0, size());
        if (range.isEmpty())
            return false;
        if (noTextProperties()) {
            return false;
        }
        List<TextPropertiesInterval> additionalIntervals = new ArrayList<>();
        boolean changed = false;
        for (Iterator<TextPropertiesInterval> iterator = myIntervals.iterator(); iterator.hasNext(); ) {
            TextPropertiesInterval interval = iterator.next();
            if (!interval.getRange().contains(range.getStart())) {
                continue;
            }
            int oldEnd = interval.getRange().getEnd();
            changed = interval.extractIntervalAndRemovePropertiesWithNames(range.getStart(), range.getEnd(), propertyNames,
                    additionalIntervals);
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
}
