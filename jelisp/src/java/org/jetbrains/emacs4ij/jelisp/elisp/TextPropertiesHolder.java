package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.exception.ArgumentOutOfRange;
import org.jetbrains.emacs4ij.jelisp.exception.InternalException;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.subroutine.Core;

import java.util.*;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 5/11/12
 * Time: 10:39 AM
 * To change this template use File | Settings | File Templates.
 */
public abstract class TextPropertiesHolder {
    public static enum Action {ADD, SET, REMOVE, REMOVE_LIST}
    protected List<TextPropertiesInterval> myIntervals = new ArrayList<>();

    public boolean noTextProperties() {
        return myIntervals.isEmpty();
    }

    protected abstract int size();

    protected final void setTextProperties (List<TextPropertiesInterval> intervals) {
        myIntervals = intervals;
    }

    protected final List<TextPropertiesInterval> getIntervals(){
        return myIntervals;
    }

    /**
     * @param givenStart = left bound of interval
     * @param givenEnd = right bound of interval
     * @param propertyList to work with
     * @return true if any properties were added or changed, false otherwise
     */
    public final boolean actOnTextProperties(int givenStart, int givenEnd, LispList propertyList, Action action) {
        TextPropertiesInterval.Range range;
        try {
            int start = this instanceof LispBuffer ? givenStart - 1 : givenStart;
            int end   = this instanceof LispBuffer ? givenEnd - 1 : givenEnd;
            range = new TextPropertiesInterval.Range(start, end, 0, size());
        } catch (ArgumentOutOfRange e) {
            throw new ArgumentOutOfRange(givenStart, givenEnd);
        }
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

    protected final String intervalsString() {
        StringBuilder sb = new StringBuilder();
        for (TextPropertiesInterval interval: myIntervals) {
            sb.append(interval.toString()).append(" ");
        }
        return sb.toString().trim();
    }

    protected final List<TextPropertiesInterval> getTextPropertiesInRange (int start, int end) {
        if (start < 0 || end > size())
            throw new ArgumentOutOfRange(start, end);
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

    @Nullable
    private TextPropertiesInterval getTextPropertiesIntervalFor (int position) {
        if (position == size())
            return null;
        List<TextPropertiesInterval> properties = getTextPropertiesInRange(position, position);
        assert properties.size() <= 1 : "Multiple text properties intervals for position";
        if (properties.isEmpty())
            return null;
        return properties.get(0);
    }

    public final LispObject getTextPropertiesAt (int position) {
        TextPropertiesInterval interval = getTextPropertiesIntervalFor(position);
        return interval == null
                ? LispSymbol.ourNil
                : interval.getPropertyList();
    }

    public final LispObject getTextPropertyAt (int position, LispObject property) {
        TextPropertiesInterval interval = getTextPropertiesIntervalFor(position);
        return interval == null
                ? LispSymbol.ourNil
                : Core.thisOrNil(interval.getProperties().get(property));
    }

    public static final class TextPropertiesInterval implements Comparable<TextPropertiesInterval> {
        private Range myRange;
        private Map<LispObject, LispObject> myProperties;

        public TextPropertiesInterval (int from, int to, int min, int max, LispList properties) {
            myRange = new Range(from, to, min, max);
            myProperties = toPropertyMap(properties);
        }

        private TextPropertiesInterval (int from, int to, Map<LispObject, LispObject> properties) {
            myRange = new Range(from, to);
            myProperties = new LinkedHashMap<>(properties);
        }

        public LispList getPropertyList() {
            List<LispObject> list = new ArrayList<>();
            for (Map.Entry<LispObject, LispObject> entry: myProperties.entrySet()) {
                list.add(entry.getKey());
                list.add(entry.getValue());
            }
            return LispList.list(list);
        }

        public Map<LispObject, LispObject> getProperties () {
            return myProperties;
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder(myRange.toString() + " (");
            for (Map.Entry<LispObject, LispObject> property: myProperties.entrySet())
                sb.append(property.getKey().toString()).append(" ").append(property.getValue().toString()).append(" ");
            return sb.toString().trim() + ")";
        }

        private static Map<LispObject, LispObject> toPropertyMap (LispList propertyList) {
            Map<LispObject, LispObject> properties = new LinkedHashMap<>();
            List<LispObject> list = propertyList.toLispObjectList();
            for (int i = 0; i + 1 < list.size(); i += 2) {
                properties.put(list.get(i), list.get(i + 1));
            }
            return properties;
        }

        @Override
        public int compareTo(TextPropertiesInterval o) {
            return myRange.compareTo(o.myRange);
        }

        public Range getRange() {
            return myRange;
        }

        /*
       returns true if any property changed, false otherwise
        */
        private boolean addOrChangeProperties (LispList propertyList) {
            Map<LispObject, LispObject> properties = toPropertyMap(propertyList);
            if (myProperties.isEmpty()) {
                myProperties = properties;
                return true;
            }
            boolean changed = false;
            for (Map.Entry<LispObject, LispObject> property: properties.entrySet()) {
                if (myProperties.containsKey(property.getKey()) && myProperties.get(property.getKey()).equals(property.getValue()))
                    continue;
                myProperties.put(property.getKey(), property.getValue());
                changed = true;
            }
            return changed;
        }

        /*
         returns false if properties were empty and properties are to be removed, true otherwise
         */
        private boolean resetOrRemoveProperties (LispList propertyList) {
            if (propertyList.equals(LispList.list(LispSymbol.ourNil, LispSymbol.ourNil))) { //remove
                boolean changed = !myProperties.isEmpty();
                myProperties.clear();
                return changed;
            }
            myProperties = toPropertyMap(propertyList);
            return true;
        }

        /*
        returns true if any property changed, false otherwise
        */
        private boolean removeProperties (LispList propertyList) {
            if (myProperties.isEmpty()) {
                return false;
            }
            List<LispObject> properties = propertyList.toLispObjectList();
            boolean changed = false;
            for (int i = 0; i < properties.size(); i += 2) {
                LispObject propertyName = properties.get(i);
                if (!myProperties.containsKey(propertyName))
                    continue;
                myProperties.remove(propertyName);
                changed = true;
            }
            return changed;
        }

        /*
        returns true if any property changed, false otherwise
        */
        private boolean removePropertiesWithNames (LispList propertyNames) {
            if (myProperties.isEmpty()) {
                return false;
            }
            List<LispObject> properties = propertyNames.toLispObjectList();
            boolean changed = false;
            for (LispObject propertyName : properties) {
                if (!myProperties.containsKey(propertyName))
                    continue;
                myProperties.remove(propertyName);
                changed = true;
            }
            return changed;
        }

        /**
         * we assume that this interval contains range start
         * @param propertyList to work with
         * @param result = resulting intervals, born from this
         * @return true if this interval was changed and/or new interval(s) created
         */
        public boolean extractIntervalAndPerformAction (Range range, LispList propertyList, Action action,
                                                        List<TextPropertiesInterval> result) {
            Range oldRange = new Range(myRange);
            List<TextPropertiesInterval> additional = extractInterval(range);
            boolean changed = false;
            switch (action) {
                case ADD:
                    changed = addOrChangeProperties(propertyList);
                    break;
                case SET:
                    changed = resetOrRemoveProperties(propertyList);
                    break;
                case REMOVE:
                    changed = removeProperties(propertyList);
                    break;
                case REMOVE_LIST:
                    changed = removePropertiesWithNames(propertyList);
            }
            if (changed)
                result.addAll(additional);
            else myRange.set(oldRange);
            return changed;
        }

        public TextPropertiesInterval extract (int start, int end) {
            int length = Math.min(myRange.getEnd(), end) - Math.max(myRange.getStart(), start);
            return new TextPropertiesInterval(0, length, myProperties);
        }

        /**
         * Extract interval of given range, assuming that this interval contains start
         * @return true if this interval was changed and/or new interval(s) created
         */
        private List<TextPropertiesInterval> extractInterval (Range range) {
            List<TextPropertiesInterval> result = new ArrayList<>();
            if (myRange.getStart() < range.getStart()) {
                result.add(new TextPropertiesInterval(myRange.getStart(), range.getStart(), myProperties));
                myRange.setStart(range.getStart());
            }
            if (range.getEnd() < myRange.getEnd()) {
                result.add(new TextPropertiesInterval(range.getEnd(), myRange.getEnd(), myProperties));
                myRange.setEnd(range.getEnd());
            }
            return result;
        }

        public boolean hasNoProperties() {
            return myProperties.isEmpty();
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof TextPropertiesInterval)) return false;

            TextPropertiesInterval interval = (TextPropertiesInterval) o;

            if (myProperties != null ? !myProperties.equals(interval.myProperties) : interval.myProperties != null)
                return false;
            if (myRange != null ? !myRange.equals(interval.myRange) : interval.myRange != null) return false;

            return true;
        }

        @Override
        public int hashCode() {
            int result = myRange != null ? myRange.hashCode() : 0;
            result = 31 * result + (myProperties != null ? myProperties.hashCode() : 0);
            return result;
        }

        /**
         * Created with IntelliJ IDEA.
         * User: kate
         * Date: 5/11/12
         * Time: 12:23 PM
         * To change this template use File | Settings | File Templates.
         */
        public static class Range implements Comparable<Range> {
            private int myStart;
            private int myEnd;

            public Range (int leftBound, int rightBound, int min, int max) {
                myStart = verify(leftBound, min, max);
                myEnd   = verify(rightBound, min, max);
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

            private static int verify (int what, int min, int max) {
                if (what < min || what > max)
                    throw new ArgumentOutOfRange(what);
                return what;
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

            @Override
            public boolean equals(Object o) {
                if (this == o) return true;
                if (!(o instanceof Range)) return false;

                Range range = (Range) o;

                if (myEnd != range.myEnd) return false;
                if (myStart != range.myStart) return false;

                return true;
            }

            @Override
            public int hashCode() {
                int result = myStart;
                result = 31 * result + myEnd;
                return result;
            }
        }
    }
}
