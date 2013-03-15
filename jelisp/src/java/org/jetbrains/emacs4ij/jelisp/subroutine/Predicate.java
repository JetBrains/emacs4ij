package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.FunctionCell;
import org.jetbrains.emacs4ij.jelisp.elisp.LispFloat;
import org.jetbrains.emacs4ij.jelisp.elisp.LispHashTable;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispMarker;
import org.jetbrains.emacs4ij.jelisp.elisp.LispNumber;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.elisp.LispVector;
import org.jetbrains.emacs4ij.jelisp.elisp.MarkerOrInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.Optional;
import org.jetbrains.emacs4ij.jelisp.elisp.Primitive;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispFrame;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispMinibuffer;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispWindow;

public abstract class Predicate {
  private Predicate() {}

  public static boolean isCharacter(LispObject object) {
    return object instanceof LispInteger && ((LispInteger) object).isCharacter();
  }

  public static boolean isString (LispObject object) {
    return object instanceof LispString;
  }

  public static boolean isCharOrString (LispObject object) {
    return isCharacter(object) || isString(object);
  }

  @Subroutine("stringp")
  public static LispSymbol stringp (LispObject arg) {
    return LispSymbol.bool(isString(arg));
  }

  @Subroutine("symbolp")
  public static LispSymbol symbolp (LispObject arg) {
    return LispSymbol.bool(arg instanceof LispSymbol);
  }

  public static boolean isInteger (LispObject arg) {
    return arg instanceof LispInteger;
  }

  @Subroutine("integerp")
  public static LispSymbol integerp (LispObject arg) {
    return LispSymbol.bool(isInteger(arg));
  }

  @Subroutine("subrp")
  public static LispObject subrp (LispObject functionCell) {
    if (functionCell == null || !(functionCell instanceof FunctionCell))
      return LispSymbol.NIL;
    if (functionCell instanceof Primitive)
      return LispSymbol.T;
    return LispSymbol.NIL;
  }

  @Subroutine("bufferp")
  public static LispSymbol bufferp (LispObject arg) {
    return LispSymbol.bool(arg instanceof LispBuffer);
  }

  @Subroutine("commandp")
  public static LispSymbol commandp (LispObject function, @Nullable @Optional LispObject forCallInteractively) {
    if (Core.toCommand(function) != null)
      return LispSymbol.T;

    //todo: autoload objects
    // http://www.gnu.org/s/emacs/manual/html_node/elisp/Interactive-Call.html

    if (isNil(forCallInteractively)) {
      // do not accept keyboard macros: string and vector
      return LispSymbol.NIL;
    }
    if (function instanceof LispString) {
      //todo: check
      return LispSymbol.NIL;
    }
    if (function instanceof LispVector) {
      //todo: check
      return LispSymbol.NIL;
    }
    return LispSymbol.NIL;
  }

  @Subroutine("buffer-live-p")
  public static LispSymbol bufferLivePredicate (Environment environment, LispObject object) {
    if (object instanceof LispBuffer) {
      return LispSymbol.bool(environment.isBufferAlive((LispBuffer) object));
    }
    return LispSymbol.NIL;
  }

  @Subroutine("fboundp")
  public static LispSymbol fboundp (Environment environment, LispSymbol symbol) {
    LispSymbol f = environment.find(symbol.getName());
    if (f == null || !f.isFunction())
      return LispSymbol.NIL;
    return LispSymbol.T;
  }

  @Subroutine("byte-code-function-p")
  public static LispSymbol byteCodeFunctionP (LispObject object) {
    return LispSymbol.NIL;
  }

  @Subroutine("framep")
  public static LispSymbol framep (LispObject object) {
    if (object instanceof LispFrame) {
      String os = System.getProperty("os.name").toLowerCase();
      if (os.contains("win"))
        return new LispSymbol("w32");
      if (os.contains("mac"))
        return new LispSymbol("mac");
      if (os.contains("nix") || os.contains("nux"))
        return new LispSymbol("x");
      //todo: ?? `ns' for an Emacs frame on a GNUstep or Macintosh Cocoa display,
      return LispSymbol.T;
    }
    return LispSymbol.NIL;
  }

  @Subroutine("frame-live-p")
  public static LispSymbol frameLiveP (Environment environment, LispObject object) {
    LispSymbol frameP = framep(object);
    if (frameP.equals(LispSymbol.NIL))
      return LispSymbol.NIL;
    return LispSymbol.bool(environment.isFrameAlive((LispFrame) object));
  }

  @Subroutine("frame-visible-p")
  public static LispSymbol frameVisibleP (Environment environment, LispObject object) {
    LispSymbol frameLiveP = frameLiveP(environment, object);
    if (frameLiveP.equals(LispSymbol.NIL))
      throw new WrongTypeArgumentException("frame-live-p", object);
    if (((LispFrame) object).isIconified())
      return new LispSymbol("icon");
    if (((LispFrame) object).isVisible())
      return LispSymbol.T;
    return LispSymbol.NIL;
  }

  @Subroutine("windowp")
  public static LispSymbol windowP (LispObject object) {
    return LispSymbol.bool (object instanceof LispWindow);
  }

  @Subroutine("number-or-marker-p")
  public static LispSymbol numberOrMarkerP (LispObject object) {
    return LispSymbol.bool (object instanceof LispNumber || object instanceof LispMarker);
  }

  public static boolean isIntegerOrMarker (LispObject object) {
    return object instanceof MarkerOrInteger;
  }

  @Subroutine("integer-or-marker-p")
  public static LispSymbol integerOrMarkerP (LispObject object) {
    return LispSymbol.bool (isIntegerOrMarker(object));
  }

  @Subroutine("markerp")
  public static LispSymbol markerP (LispObject object) {
    return LispSymbol.bool(object instanceof LispMarker);
  }

  @Subroutine("keywordp")
  public static LispSymbol keywordP (LispObject object) {
    return LispSymbol.bool(object instanceof LispSymbol && ((LispSymbol) object).isKeyword());
  }

  @Subroutine("default-boundp")
  public static LispSymbol defaultBoundP (Environment environment, LispSymbol symbol) {
    try {
      Symbol.defaultValue(environment, symbol);
    } catch (VoidVariableException e) {
      return LispSymbol.NIL;
    }
    return LispSymbol.T;
  }

  @Subroutine("vectorp")
  public static LispSymbol vectorP (LispObject object) {
    return LispSymbol.bool(object instanceof LispVector);
  }

  @Subroutine("characterp")
  public static LispSymbol characterP (LispObject object, @Optional LispObject ignore) {
    return LispSymbol.bool(isCharacter(object));
  }

  @Subroutine("char-or-string-p")
  public static LispSymbol charOrStringP (LispObject object) {
    return LispSymbol.bool(isCharOrString(object));
  }

  @Subroutine("numberp")
  public static LispSymbol numberP (LispObject object) {
    return LispSymbol.bool(isNumber(object));
  }

  @Subroutine("zerop")
  public static LispSymbol zeroP (LispNumber object) {
    return LispSymbol.bool(object.getDoubleData() == 0);
  }

  public static boolean isNil (LispObject object) {
    return object == null || object.equals(LispSymbol.NIL);
  }

  public static boolean isWholeNumber(LispObject object) {
    return object instanceof LispInteger && ((LispInteger) object).getData() >= 0;
  }

  public static boolean isNumber(LispObject object) {
    return object instanceof LispNumber;
  }

  public static boolean isNumberGreaterThanOne(LispObject object) {
    return object instanceof LispNumber && ((LispNumber) object).getDoubleData() > 1;
  }

  public static boolean isNumberFromZeroToOne (LispObject object) {
    return object instanceof LispNumber && ((LispNumber) object).getDoubleData() >= 0
        && ((LispNumber) object).getDoubleData() <= 1;
  }

  @Subroutine("wholenump")
  public static LispSymbol wholeNumP (LispObject object) {
    return LispSymbol.bool(isWholeNumber(object));
  }

  @Subroutine("window-live-p")
  public static LispSymbol windowLiveP (Environment environment, LispWindow window) {
    return LispSymbol.bool(environment.isWindowAlive(window));
  }

  @Subroutine("minibufferp")
  public static LispSymbol minibufferP (Environment environment, @Optional LispObject bufferOrName) {
    LispBuffer buffer = Buffer.getBufferByBufferNameOrNil(environment, bufferOrName);
    return LispSymbol.bool(buffer instanceof LispMinibuffer);
  }

  @Subroutine("input-pending-p")
  public static LispSymbol inputPendingP (Environment environment) {
    return LispSymbol.bool(environment.getMinibufferWindow() != null
        && environment.getSelectedWindow() == environment.getMinibufferWindow());
  }

  public static boolean isUserOption(LispObject object) {
    if (!(object instanceof LispSymbol))
      return false;
    LispObject doc = ((LispSymbol) object).getDocumentation();
    if (doc instanceof LispString && ((LispString) doc).getData().startsWith("*"))
      return true;
    if (!Predicate.isNil (((LispSymbol) object).getProperty("standard-value"))
        || !Predicate.isNil (((LispSymbol) object).getProperty("custom-autoload")))
      return true;
    //todo: true if object is an alias for other user option. But if there is a loop in symbols chain, return false
    return false;
  }

  @Subroutine("user-variable-p")
  public static LispSymbol userOptionP (LispObject var) {
    return LispSymbol.bool(isUserOption(var));
  }

  @Subroutine("hash-table-p")
  public static LispSymbol hashTableP (LispObject object) {
    return LispSymbol.bool(object instanceof LispHashTable);
  }

  @Subroutine("called-interactively-p")
  public static LispSymbol calledInteractivelyP (LispObject kind) {
    //todo: if kind.equals(new LispSymbol("interactive") then return t only if the interactive call was made by user directly
    // (not in non-interactive mode or from kbd macro)
    return LispSymbol.bool(GlobalEnvironment.isInteractiveCall());
  }

  @Subroutine("nlistp")
  public static LispSymbol notListP (LispObject object) {
    return LispSymbol.bool(!(object.equals(LispSymbol.NIL) || object instanceof LispList));
  }

  @Subroutine("floatp")
  public static LispSymbol floatP (LispObject object) {
    return LispSymbol.bool(object instanceof LispFloat);
  }

  public static boolean isFeature (LispSymbol feature, @Optional LispObject subFeature) {
    LispObject features = GlobalEnvironment.INSTANCE.find("features").getValue();
    if (!(features instanceof LispList))
      throw new WrongTypeArgumentException("listp", features);
    LispList car = ((LispList) features).memq(feature, "eq");
    if (car.isEmpty())
      return false;
    if (isNil(subFeature))
      return true;
    LispObject symbol = car.car();
    if (!(symbol instanceof LispSymbol))
      throw new WrongTypeArgumentException("symbolp", symbol);
    LispSymbol f = GlobalEnvironment.INSTANCE.find(((LispSymbol)symbol).getName());
    LispObject subFeatures = f.getProperty("subfeatures");
    if (!(subFeatures instanceof LispList))
      throw new WrongTypeArgumentException("listp", subFeatures);
    return !((LispList) subFeatures).memq(subFeature, "equal").isEmpty();
  }

  @Subroutine("featurep")
  public static LispSymbol featureP (LispSymbol feature, @Optional LispObject subFeature) {
    return LispSymbol.bool(isFeature(feature, subFeature));
  }

  public static int getInteger(LispObject i) {
    if (!isInteger(i)) throw new WrongTypeArgumentException("integerp", i);
    return ((LispInteger)i).getData();
  }
}

