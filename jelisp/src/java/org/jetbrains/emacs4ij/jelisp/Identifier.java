package org.jetbrains.emacs4ij.jelisp;

import com.intellij.util.xmlb.annotations.Attribute;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 7/1/12
 * Time: 2:21 PM
 * To change this template use File | Settings | File Templates.
 */

/**
 * This class is a helper class for index, @see org.jetbrains.emacs4ij.jelisp.DefinitionIndex
 * This class is public for serialization and deserialization for index persistence.
 * Identifier presents an identifier for Emacs item: variable or function.
 */
public final class Identifier {
    private String myName;
    private DefinitionLoader.DefType myType;
    private DefinitionLoader.SymbolType mySymbolType;

    /**
     * Constructor for deserialization.
     */
    @Deprecated
    public Identifier() {
    }

    public Identifier (final String name, final DefinitionLoader.DefType type) {
        myName = name;
        myType = type;
        mySymbolType = type == DefinitionLoader.DefType.VAR ? DefinitionLoader.SymbolType.VAR : DefinitionLoader.SymbolType.FUN;
    }

    Identifier (String name, DefinitionLoader.SymbolType type) {
        myName = name;
        myType = type == DefinitionLoader.SymbolType.VAR ? DefinitionLoader.DefType.VAR : DefinitionLoader.DefType.FUN;
        mySymbolType = type;
    }

    @Attribute("name")
    public String getName() {
        return myName;
    }

    /**
     * Don't use this setter! It is only for deserialization of persistent data.
     * @param name of symbol to be set
     */
    @Deprecated
    public void setName(final String name) {
        myName = name;
    }

    @Attribute("type")
    public DefinitionLoader.DefType getType() {
        return myType;
    }

    /**
     * Don't use this setter! It is only for deserialization of persistent data.
     * @param type (variable or function) to be set
     */
    @Deprecated
    public void setType(DefinitionLoader.DefType type) {
        myType = type;
        mySymbolType = type == DefinitionLoader.DefType.VAR ? DefinitionLoader.SymbolType.VAR : DefinitionLoader.SymbolType.FUN;
    }

//    @Attribute("symbol-type")
//    public DefinitionLoader.SymbolType getSymbolType() {
//        return mySymbolType;
//    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Identifier)) return false;

        Identifier that = (Identifier) o;

        if (myName != null ? !myName.equals(that.myName) : that.myName != null) return false;
        if (myType != that.myType) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = myName != null ? myName.hashCode() : 0;
        result = 31 * result + (myType != null ? myType.hashCode() : 0);
        return result;
    }

    @Override
    public String toString() {
        return "[" + myType + " " + myName + "]";
    }
}
