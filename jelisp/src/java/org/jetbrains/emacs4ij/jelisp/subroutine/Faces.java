package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.elisp.LispVector;
import org.jetbrains.emacs4ij.jelisp.elisp.Optional;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispFrame;
import org.jetbrains.emacs4ij.jelisp.util.XlfdField;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public abstract class Faces {
  private Faces() {}

  private final static int SORT_ORDER_SIZE = 4;

  private static LispSymbol kwSlant = new LispSymbol(":slant");
  private static LispSymbol kwHeight = new LispSymbol(":height");
  private static LispSymbol kwWidth = new LispSymbol(":width");
  private static LispSymbol kwWeight = new LispSymbol(":weight");

  private static List<XlfdField> ourFontSortOrder = new ArrayList<>(SORT_ORDER_SIZE);
  private static List<LispObject> ourAlternativeFontFamilyAlist = new ArrayList<>();
  private static List<LispObject> ourAlternativeFontRegistryAlist = new ArrayList<>();
  private static Map<LispSymbol, LispVector> ourDefaultFrameFacesAttributes = new HashMap<>();

  static {
    GlobalEnvironment.INSTANCE.defineSymbols(kwSlant, kwHeight, kwWidth, kwWeight);
    ourFontSortOrder.add(XlfdField.SET_WIDTH);
    ourFontSortOrder.add(XlfdField.POINT_SIZE);
    ourFontSortOrder.add(XlfdField.WEIGHT);
    ourFontSortOrder.add(XlfdField.SLANT);
  }

  @Subroutine("internal-set-font-selection-order")
  public static LispSymbol internalSetFontSelectionOrder(Environment environment, LispList order) {

    List<LispObject> orderObjects = order.toLispObjectList();
    if (orderObjects.size() != SORT_ORDER_SIZE) {
      Core.error(JelispBundle.message("invalid.font.sort.order", order));
    }

    List<XlfdField> values = new ArrayList<>(SORT_ORDER_SIZE);

    for (LispObject item : orderObjects) {
      if (item.equals(kwHeight)) {
        values.add(XlfdField.POINT_SIZE);
      } else if (item.equals(kwSlant)) {
        values.add(XlfdField.SLANT);
      } else if (item.equals(kwWeight)) {
        values.add(XlfdField.WEIGHT);
      } else if (item.equals(kwWidth)) {
        values.add(XlfdField.SET_WIDTH);
      } else {
        values.add(null);
      }
    }

    if (values.contains(null)) {
      Core.error(JelispBundle.message("invalid.font.sort.order", order));
    }
    
    if (!ourFontSortOrder.equals(values)) {
      for (int i = 0; i < SORT_ORDER_SIZE; i++) {
        ourFontSortOrder.set(i, values.get(i));
      }
      resetFaces();
    }

    //todo update global sort order with ourFontSortOrder

    return LispSymbol.ourNil;
  }

  @Subroutine("internal-set-alternative-font-family-alist")
  public static LispList internalSetAlternativeFontFamilyAlist(Environment environment, LispList aList) {
    ourAlternativeFontFamilyAlist = checkAndCopyAlternativeAlist(environment, aList);
    resetFaces();
    return LispList.list(ourAlternativeFontFamilyAlist);
  }

  @Subroutine("internal-lisp-face-p")
  public static LispSymbol internalLispFaceP(Environment environment, LispObject face, @Optional LispObject frame) {
    LispSymbol f;

    if (face instanceof LispString) {
      f = Symbol.intern(environment, (LispString) face, null);
    } else if (!(face instanceof LispSymbol)) {
      return LispSymbol.ourNil;
    } else {
      f = environment.find(((LispSymbol) face).getName());
    }

    if (Predicate.isNil(frame)) {
      return LispSymbol.bool(ourDefaultFrameFacesAttributes.containsKey(f));
    }
    if (Predicate.frameLiveP(environment, frame).toBoolean()) {
      return LispSymbol.bool(((LispFrame) frame).hasFace(f));
    }

    throw new WrongTypeArgumentException("frame-live-p", frame);
  }

  @Subroutine("internal-set-alternative-font-registry-alist")
  public static LispList internalSetAlternativeFontRegistryAlist(Environment environment, LispList aList) {
    ourAlternativeFontRegistryAlist = checkAndCopyAlternativeAlist(environment, aList);
    resetFaces();
    return LispList.list(ourAlternativeFontRegistryAlist);
  }

  protected static List<XlfdField> getFontSortOrder() {
    return Collections.unmodifiableList(ourFontSortOrder);
  }

  private static List<LispObject> checkAndCopyAlternativeAlist(Environment environment, LispList aList) {
    List<LispObject> copy = new ArrayList<>();

    for (LispObject item: aList.toLispObjectList()) {
      if (!(item instanceof LispList)) {
        throw new WrongTypeArgumentException("listp", item);
      }

      List<LispObject> itemCopy = new ArrayList<>();

      for (LispObject alternative: ((LispList)item).toLispObjectList()) {
        if (!(alternative instanceof LispString)) {
          throw new WrongTypeArgumentException("stringp", alternative);
        }
        itemCopy.add(Symbol.intern(environment, (LispString) alternative, null));
      }

      copy.add(LispList.list(itemCopy));
    }

    return copy;
  }

  private static void resetFaces() {
    //todo reset all used faces
  }
}
