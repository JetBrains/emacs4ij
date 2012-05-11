package org.jetbrains.emacs4ij.jelisp.elisp;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class TextPropertiesInterval implements Comparable<TextPropertiesInterval> {
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
     * we assume that this interval contains start
     * @param start = left bound of interval to change/create
     * @param end = right bound of interval to change/create
     * @param propertyList to add
     * @param result = resulting intervals, born from this
     * @return true if this interval was changed and/or new interval(s) created
     */
    public boolean extractIntervalAndAddProperties (int start, int end, LispList propertyList,
                                                    List<TextPropertiesInterval> result) {
        boolean changed = extractInterval(start, end, result);
        return addOrChangeProperties(propertyList) || changed;
    }

    /**
     * we assume that this interval contains start
     * @param start = left bound of interval to change/create
     * @param end = right bound of interval to change/create
     * @param propertyList to set
     * @param result = resulting intervals, born from this
     * @return true if this interval was changed and/or new interval(s) created
     */
    public boolean extractIntervalAndSetProperties (int start, int end, LispList propertyList,
                                                    List<TextPropertiesInterval> result) {
        boolean changed = extractInterval(start, end, result);
        return resetOrRemoveProperties(propertyList) || changed;
    }

    /**
     * we assume that this interval contains start
     * @param start = left bound of interval to change/create
     * @param end = right bound of interval to change/create
     * @param propertyList = list of properties to be removed
     * @param result = resulting intervals, born from this
     * @return true if this interval was changed and/or new interval(s) created
     */
    public boolean extractIntervalAndRemoveProperties (int start, int end, LispList propertyList,
                                                    List<TextPropertiesInterval> result) {
        boolean changed = extractInterval(start, end, result);
        return removeProperties(propertyList) || changed;
    }

    /**
     * we assume that this interval contains start
     * @param start = left bound of interval to change/create
     * @param end = right bound of interval to change/create
     * @param propertyNames = list of property names to be removed
     * @param result = resulting intervals, born from this
     * @return true if this interval was changed and/or new interval(s) created
     */
    public boolean extractIntervalAndRemovePropertiesWithNames (int start, int end, LispList propertyNames,
                                                       List<TextPropertiesInterval> result) {
        boolean changed = extractInterval(start, end, result);
        return removePropertiesWithNames(propertyNames) || changed;
    }

    public TextPropertiesInterval extract (int start, int end) {
        return new TextPropertiesInterval(Math.max(myRange.getStart(), start), Math.min(myRange.getEnd(), end), myProperties);
    }

    /**
     * Extract [start; end) from given range, assuming that this interval contains start
     * @param start = left bound of interval to change/create
     * @param end = right bound of interval to change/create
     * @param result = resulting intervals, born from this
     * @return true if this interval was changed and/or new interval(s) created
     */
    private boolean extractInterval (int start, int end, List<TextPropertiesInterval> result) {
        boolean changed = false;
        if (myRange.getStart() < start) {
            result.add(new TextPropertiesInterval(myRange.getStart(), start, myProperties));
            myRange.setStart(start);
            changed = true;
        }
        if (end < myRange.getEnd()) {
            result.add(new TextPropertiesInterval(end, myRange.getEnd(), myProperties));
            myRange.setEnd(end);
            changed = true;
        }
        return changed;
    }

    public boolean hasNoProperties() {
        return myProperties.isEmpty();
    }
}