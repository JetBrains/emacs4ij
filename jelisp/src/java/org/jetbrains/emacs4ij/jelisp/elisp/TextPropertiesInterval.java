package org.jetbrains.emacs4ij.jelisp.elisp;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class TextPropertiesInterval implements Comparable<TextPropertiesInterval> {
    private Range myRange;
    private Map<LispObject, LispObject> myProperties;

    public TextPropertiesInterval (MarkerOrInteger from, MarkerOrInteger to, TextPropertiesHolder holder) {
        myRange = new Range(from.getPosition(), to.getPosition(), 0, holder.size());
        myProperties = new LinkedHashMap<>();
    }

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
    public boolean addOrChangeProperties (LispList propertyList) {
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

    /**
     * we assume that this interval contains start
     * @param start = left bound of interval to change/create
     * @param end = right bound of interval to change/create
     * @param propertyList to set
     * @param result = resulting intervals, born from this
     * @return true if this interval was changed and/or new interval(s) created
     */
    public boolean extractIntervalAndAddProperties (int start, int end, LispList propertyList,
                                                                   List<TextPropertiesInterval> result) {
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
        return addOrChangeProperties(propertyList) || changed;
    }

    public TextPropertiesInterval extract (int start, int end) {
        return new TextPropertiesInterval(Math.max(myRange.getStart(), start), Math.min(myRange.getEnd(), end), myProperties);
    }
}