package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.elisp.LispVector;
import org.jetbrains.emacs4ij.jelisp.elisp.Optional;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
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

  private static enum FaceAttribute {
    FACE, FAMILY, FOUNDRY, WIDTH, HEIGHT, WEIGHT, SLANT, UNDERLINE, INVERSE, FOREGROUND, BACKGROUND, STIPPLE,
    OVERLINE, STRIKE_THROUGH, BOX, INHERIT, FONT_SET, VECTOR
  }

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
    GlobalEnvironment.INSTANCE.defineSymbol("unspecified");
    GlobalEnvironment.INSTANCE.defineSymbol("face");
    GlobalEnvironment.INSTANCE.defineSymbol(":ignore-defface");
    GlobalEnvironment.INSTANCE.defineSymbol(":family");
    GlobalEnvironment.INSTANCE.defineSymbol("face-alias");
    GlobalEnvironment.INSTANCE.defineSymbol("face-no-inherit");

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

    return LispSymbol.NIL;
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
      return LispSymbol.NIL;
    } else {
      f = environment.find(((LispSymbol) face).getName());
    }

    if (Predicate.isNil(frame)) {
      return LispSymbol.bool(ourDefaultFrameFacesAttributes.containsKey(f));
    }
    if (Predicate.frameLiveP(environment, frame).toBoolean()) {
      return LispSymbol.bool(((LispFrame) frame).getFacesAList().containsKey(f));
    }

    throw new WrongTypeArgumentException("frame-live-p", frame);
  }

  @Subroutine("internal-set-alternative-font-registry-alist")
  public static LispList internalSetAlternativeFontRegistryAlist(Environment environment, LispList aList) {
    ourAlternativeFontRegistryAlist = checkAndCopyAlternativeAlist(environment, aList);
    resetFaces();
    return LispList.list(ourAlternativeFontRegistryAlist);
  }

  @Subroutine("display-supports-face-attributes-p")
  public static LispSymbol displaySupportsFaceAttrsP(LispVector attributes, @Optional LispObject display) {
    //todo:)
    return LispSymbol.T;
  }

  @Subroutine("internal-make-lisp-face")
  public static LispVector internalMakeLispFace(Environment environment, LispSymbol face, @Optional LispObject frame) {
    LispVector globalFace = getFaceDefOnFrame(null, face, false);
    LispVector resolvedFace = null;
    LispFrame f = null;
    if (!Predicate.isNil(frame)) {
      if (Predicate.frameLiveP(environment, face).equals(LispSymbol.NIL))
        throw new WrongTypeArgumentException("frame-live-p", frame);
      f = (LispFrame) frame;
      resolvedFace = getFaceDefOnFrame(f, face, false);
    }

    if (globalFace == null) {
      ourDefaultFrameFacesAttributes.put(face, createEmptyFaceAttributes());
    } else if (f == null) {
      emptifyFaceAttributes(globalFace);
    }

    if (f != null) {
      if (resolvedFace == null) {
        f.getFacesAList().put(face, createEmptyFaceAttributes());
      } else {
        emptifyFaceAttributes(resolvedFace);
      }
    } else {
      resolvedFace = globalFace;
    }

    if (face.getProperty("face-no-inherit") == LispSymbol.NIL) {
      //todo (1)
    }

    if (!verifyFaceAttributes(resolvedFace))
      throw new LispException(JelispBundle.message("invalid.face", face));

    return resolvedFace;
  }

  private static LispVector createEmptyFaceAttributes() {
    LispVector attrs = LispVector.make(getAttributesSize(), get("unspecified"));
    attrs.setItem(0, get("face"));
    return attrs;
  }

  private static void emptifyFaceAttributes(LispVector attrs) {
    for (int i = 1; i < getAttributesSize(); i++) {
      attrs.setItem(i, get("unspecified"));
    }
  }

  @Subroutine("internal-set-lisp-face-attribute")
  public static LispSymbol internalSetLispFaceAttribute(Environment environment, LispSymbol face, LispSymbol attribute, LispObject value, @Optional LispObject frame) {
    face = (LispSymbol) getFace(face, true);
    LispVector attrs;

    if (Predicate.isNil(frame)) { //set for selected frame
      return internalSetLispFaceAttribute(environment, face, attribute, value, environment.getSelectedFrame());
    } else if (frame.equals(LispSymbol.T)) { //default for new frames

      attrs = getFaceDefOnFrame(null, face, true);

      if (isUnspecified(value)) {
        value = get(":ignore-defface");
      }

    } else if (frame.equals(new LispInteger(0))) { //change face on all frames + default for new
      internalSetLispFaceAttribute(environment, face, attribute, value, LispSymbol.T);
      for (LispFrame f: environment.getAllFrames()) {
        internalSetLispFaceAttribute(environment, face, attribute, value, f);
      }
      return (LispSymbol) getFace(face, true);
    } else if (Predicate.frameLiveP(environment, frame).equals(LispSymbol.T)) { //set for given frame
      attrs = getFaceDefOnFrame((LispFrame) frame, face, false);
      if (attrs == null) {
        attrs = internalMakeLispFace(environment, face, frame);
      }
    } else {
      throw new WrongTypeArgumentException("frame-live-p", frame);
    }
    LispObject old = null;
    if (get(":family").equals(attribute)) {
      if (notUnspecifiedAndNotIgnoreDefFace(value)) {
        verifyNonEmptyString(value, "Invalid face family");
      }
      old = setFaceAttribute(attrs, FaceAttribute.FAMILY, value);
    } else if (get(":foundry").equals(attribute)) {
      if (notUnspecifiedAndNotIgnoreDefFace(value)) {
        verifyNonEmptyString(value, "Invalid face foundry");
      }
      old = setFaceAttribute(attrs, FaceAttribute.FOUNDRY, value);
    }

    if (get(":height").equals(attribute)) {
      if (notUnspecifiedAndNotIgnoreDefFace(value)) {
        if (symbol("default").equals(face)) {
          if (!Predicate.isInteger(value) || ((LispInteger)value).getData() <= 0) {
            throw new LispException("Default face height not absolute and positive " + value);
          }
        } else {
          //todo check if value is valid
        }
      }
      old = setFaceAttribute(attrs, FaceAttribute.HEIGHT, value);
      //todo implement other attributes

    } else {
      Core.error(JelispBundle.message("invalid.face.attr.name", attribute.getName()));
    }

    if (!value.equals(old)) {
      //todo (1) apply to frame:)
    }

    return face;
  }

  private static LispObject setFaceAttribute(LispVector attrs, FaceAttribute index, LispObject value) {
    LispObject old = attrs.get(index.ordinal());
    attrs.setItem(index.ordinal(), value);
    return old;
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

  private static LispVector getFaceDefOnFrame(@Nullable LispFrame frame, LispObject face, boolean throwError) {
    face = getFace(face, throwError);
    LispObject attributes = frame == null ? ourDefaultFrameFacesAttributes.get(face) : frame.getFacesAList().get(face);
    if (attributes == null || !verifyFaceAttributes(attributes)) {
      if (throwError)
        throw new LispException(JelispBundle.message("invalid.face", face));
      else return null;
    }
    return (LispVector) attributes;
  }

  private static LispObject getFace(LispObject face, boolean throwError) {
    LispSymbol f;
    if (face instanceof LispString) {
      f = new LispSymbol(((LispString) face).getData());
      GlobalEnvironment.INSTANCE.defineSymbol(f);
    } if (!isNonNilSymbol(face)) {
      return face;
    } else {
      f = ((LispSymbol) face).uploadVariableDefinition();
    }

    List<LispSymbol> aliases = new ArrayList<>();
    while (true) {
      LispObject alias = f.getProperty(get("face-alias"));
      if (alias == null || !isNonNilSymbol(alias)) return f;
      if (aliases.contains((LispSymbol)alias)) {
        if (throwError) throw new LispException("circular-list " + f);
        return symbol("default");
      }
      aliases.add((LispSymbol)alias);
      f = (LispSymbol)alias;
    }
  }

  private static void verifyNonEmptyString(LispObject s, String error) {
    if (!Predicate.isString(s)) throw new WrongTypeArgumentException("stringp", s);
    if (((LispString)s).getData().isEmpty()) throw new LispException(error + " " + s);
  }

  private static boolean verifyFaceAttributes(LispObject attr) {
    return attr instanceof LispVector && ((LispVector) attr).size() == getAttributesSize()
        && ((LispVector) attr).get(0).equals(symbol("face"));
  }

  private static int getAttributesSize() {
    return FaceAttribute.values().length;
  }

  private static boolean isNonNilSymbol(LispObject o) {
    return o instanceof LispSymbol && !LispSymbol.NIL.equals(o);
  }

  private static boolean notUnspecifiedAndNotIgnoreDefFace(LispObject attribute) {
    return !get("unspecified").equals(attribute) && !get(":ignore-defface").equals(attribute);
  }

  private static boolean isUnspecified(LispObject attribute) {
    return get("unspecified").equals(attribute);
  }

  private static LispSymbol symbol(String name) {
    return new LispSymbol(name);
  }

  private static LispSymbol get(String name) {
    LispSymbol s = GlobalEnvironment.INSTANCE.find(name);
    if (name == null) throw new IllegalStateException();
    return s;
  }
}
