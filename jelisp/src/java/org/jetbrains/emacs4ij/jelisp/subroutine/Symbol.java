package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.elisp.Lambda;
import org.jetbrains.emacs4ij.jelisp.elisp.LispCommand;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispMacro;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.elisp.LispVector;
import org.jetbrains.emacs4ij.jelisp.elisp.Optional;
import org.jetbrains.emacs4ij.jelisp.elisp.Primitive;
import org.jetbrains.emacs4ij.jelisp.exception.CyclicDefinitionLoadException;
import org.jetbrains.emacs4ij.jelisp.exception.InvalidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.List;

public abstract class Symbol {
  private Symbol() {}

  @Subroutine("symbol-function")
  public static LispObject symbolFunction(Environment environment, LispSymbol arg) {
    LispSymbol f = environment.find(arg.getName());
    if (f == null || !f.isFunction())
      throw new VoidFunctionException(arg.getName());
    return f.getFunction();
  }

  @Subroutine("symbol-value")
  public static LispObject symbolValue (Environment environment, LispSymbol arg) {
    LispSymbol symbol = environment.find(arg.getName());
    if (symbol == null || !symbol.hasValue() || symbol.getValue().equals(LispSymbol.VOID))
      throw new VoidVariableException(arg.getName());
    return symbol.getValue();
  }

  @Subroutine("symbol-name")
  public static LispObject symbolName (LispSymbol arg) {
    return new LispString(arg.getName());
  }

  @Subroutine("get")
  public static LispObject get(Environment environment, LispSymbol symbol, LispSymbol propertyName) {
    try {
      LispSymbol real = symbol.uploadVariableDefinition();
      return Core.thisOrNil(real.getProperty(propertyName));
    } catch (CyclicDefinitionLoadException | VoidVariableException | NullPointerException e) {
      return LispSymbol.NIL;
    }
  }

  @Subroutine("put")
  public static LispObject put(Environment environment, LispSymbol symbol, LispSymbol propertyName, LispObject value) {
    try {
      LispSymbol real = symbol.uploadVariableDefinition();
      real.setProperty(propertyName, value);
    } catch (CyclicDefinitionLoadException | VoidVariableException | NullPointerException e) {
      symbol.setProperty(propertyName, value);
      GlobalEnvironment.INSTANCE.defineSymbol(symbol);
    }
    return value;
  }

  public static LispObject getDocumentationProperty (Environment environment, LispObject doc) {
    if (doc instanceof LispString)
      return doc;
    if (doc instanceof LispInteger)
      return getDocByInt(((LispInteger) doc).getData());
    return doc.evaluate(environment);
  }

  private static LispObject getDocByInt (int offset) {
    if (offset < 0)
      offset = -offset;
    try {
      RandomAccessFile docFile = new RandomAccessFile(
          ((LispString)GlobalEnvironment.INSTANCE.find("doc-directory").getValue()).getData() +
              ((LispString)GlobalEnvironment.INSTANCE.find("internal-doc-file-name").getValue()).getData(), "r");
      docFile.seek(offset);
      String doc = "";
      String line = docFile.readLine();
      while (line != null && !line.contains("")) {
        doc += (doc.length() > 0 ? '\n' : "") + line;
        line = docFile.readLine();
      }
      docFile.close();
      if (line != null)
        doc += (doc.length() > 0 ? '\n' : "") + line.substring(0, line.indexOf(''));
      return new LispString(doc);
    } catch (IOException e) {
      return LispSymbol.NIL;
    }
  }

  @Subroutine("documentation-property")
  public static LispObject documentationProperty (Environment environment, LispSymbol symbol, LispSymbol propertyName,
                                                  @Nullable @Optional LispObject verbatim)  {
    //todo: if (verbatim != null) && !(verbatim.equals(LispSymbol.NIL) ---
    // Third argument RAW omitted or nil means pass the result through `substitute-command-keys' if it is a string.

    LispObject value = get(environment, symbol, propertyName);
    return getDocumentationProperty(environment, value);
  }

  @Subroutine("documentation")
  public static LispObject documentation (Environment environment, LispObject function) {
    if (function instanceof LispSymbol) {
      String name = ((LispSymbol) function).getName();
      LispSymbol f = environment.find(name);
      if (f == null)
        throw new VoidFunctionException(name);

      LispObject funPropValue = documentationProperty(environment, f, new LispSymbol("function-documentation"), null);
      if (!funPropValue.equals(LispSymbol.NIL))
        return funPropValue;

      if (!f.isFunction())
        throw new VoidFunctionException(name);
      return f.getDocumentation();

    } else if (function instanceof Lambda) {
      return ((Lambda) function).getDocumentation();
    } else if (function instanceof LispMacro) {
      return ((LispMacro)function).getDocumentation();
    } else if (function instanceof Primitive) {
      return ((Primitive) function).getDocumentation();
    }
    throw new InvalidFunctionException(function.toString());
  }


  @Subroutine(value = "default-value")
  public static LispObject defaultValue (Environment environment, LispSymbol symbol) {
    LispSymbol real = environment.find(symbol.getName());
    if (real == null)
      throw new VoidVariableException(symbol.getName());
    if (!real.isBufferLocal()) {
      if (!real.hasValue())
        throw new VoidVariableException(symbol.getName());
      return real.getValue();
    }
    LispObject value = GlobalEnvironment.INSTANCE.getBufferLocalSymbolValue(symbol);
    if (value == null)
      throw new VoidVariableException(symbol.getName());
    return value;
  }

  @Subroutine("set-default")
  public static LispObject setDefault (Environment environment, LispSymbol symbol, LispObject value) {
    LispSymbol real = GlobalEnvironment.INSTANCE.find(symbol.getName());
    if (real != null) {
      //todo: check for buffer-locality
      // real.setBufferLocal(true); //?
      real.setValue(value);
    } else {
      symbol.setValue(value);
      GlobalEnvironment.INSTANCE.defineSymbol(symbol);
    }
    return value;
  }

  @Subroutine("make-symbol")
  public static LispObject makeSymbol (LispString name) {
    return new LispSymbol(name.getData());
  }

  private static LispSymbol getSymbol (final String name, @Nullable LispVector objectArray) {
    if (objectArray == null) {
      return GlobalEnvironment.INSTANCE.find(name);
    }
    List<LispSymbol> filtered = new ArrayList<>();
    for (LispObject o: objectArray.toLispObjectList()) {
      if (o.equals(new LispInteger(0))) continue;
      if (o instanceof LispSymbol) {
        if (((LispSymbol) o).getName().equals(name)) {
          filtered.add((LispSymbol) o);
        }
      } else {
        Core.error(JelispBundle.message("invalid.object.array"));
      }
    }
    if (filtered.size() != 1 && filtered.size() != 0) throw new IllegalStateException("real size = " + filtered.size());
    if (filtered.isEmpty()) return null;
    return filtered.get(0);
  }

  @Subroutine("intern")
  public static LispSymbol intern(LispString name, @Optional LispObject objectArray) {
    return intern(name.getData(), objectArray);
  }

  public static LispSymbol intern(String name, LispObject objectArray) {
    //TODO? you cannot intern a given symbol in more than one object array

    if (Predicate.isNil(objectArray))
      objectArray = null;
    if (objectArray != null && (!(objectArray instanceof LispVector) || ((LispVector) objectArray).isEmpty()))
      throw new WrongTypeArgumentException("vectorp", objectArray);

    LispSymbol symbol = getSymbol(name, (LispVector) objectArray);
    if (symbol != null)
      return symbol;

    symbol = new LispSymbol(name);

    if (objectArray == null)
      GlobalEnvironment.INSTANCE.defineSymbol(symbol);
    else {
      GlobalEnvironment.INSTANCE.defineSymbol(symbol);
      ((LispVector)objectArray).defineSymbol(symbol);
    }

    return symbol;
  }

  @Subroutine("interactive-form")
  public static LispObject interactiveForm (LispObject command) {
    if (command instanceof LispList)
      command = new Lambda((LispList) command);
    if (!Predicate.commandp(command, null).toBoolean())
      return LispSymbol.NIL;
    return ((LispCommand)command).getInteractiveForm();
  }

  @Subroutine("boundp")
  public static LispSymbol boundP (LispSymbol symbol) {
    return LispSymbol.bool(symbol.hasValue());
  }

  @Subroutine("symbol-plist")
  public static LispList getPropertyList (LispSymbol symbol) {
    return symbol.getPropertyList();
  }
}
