package org.jetbrains.emacs4ij.jelisp.elisp;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class TextPropertiesInterval implements Comparable<TextPropertiesInterval> {
    private Range myRange;
    private Map<LispObject, LispObject> myProperties;
    public static enum Action {ADD, SET, REMOVE, REMOVE_LIST}

    public TextPropertiesInterval (int from, int to, int min, int max, LispList properties) {
        myRange = new Range(from, to, min, max);
        myProperties = toPropertyMap(properties);
    }

    private TextPropertiesInterval (int from, int to, Map<LispObject, LispObject> properties) {
        myRange = new Range(from, to);
        myProperties = new LinkedHashMap<>(properties);
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
}