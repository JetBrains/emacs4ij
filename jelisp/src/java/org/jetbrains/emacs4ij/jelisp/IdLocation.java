package org.jetbrains.emacs4ij.jelisp;

import com.intellij.util.xmlb.annotations.MapAnnotation;

import java.util.SortedMap;
import java.util.TreeMap;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 7/1/12
 * Time: 5:16 PM
 * To change this template use File | Settings | File Templates.
 */

/**
 * This class is a helper class for index, @see org.jetbrains.emacs4ij.jelisp.DefinitionIndex
 *
 * IdLocation is a wrapper for map which is the positions of Emacs symbols' definitions in Emacs source code.
 * It has public API for serialization and deserialization for index persistence.
 */

public final class IdLocation {
    private SortedMap<String, Long> myLocations = new TreeMap<>();

    @Deprecated
    public IdLocation() {
    }

    public IdLocation (SortedMap<String, Long> map) {
        myLocations = map;
    }

    @Deprecated
    public void setLocations (SortedMap<String, Long> locations) {
        myLocations = locations;
    }

    @MapAnnotation(surroundWithTag = false, entryTagName = "locations", keyAttributeName = "file-name", valueAttributeName = "position")
    public SortedMap<String, Long> getLocations() {
        return myLocations;
    }
}
