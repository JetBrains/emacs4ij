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
import org.jetbrains.emacs4ij.jelisp.elisp.MarkerOrInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.Optional;
import org.jetbrains.emacs4ij.jelisp.exception.MarkerPointsNowhereException;
import org.jetbrains.emacs4ij.jelisp.exception.NoBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.parser.ForwardParser;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispFrame;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispToolWindow;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.jetbrains.emacs4ij.jelisp.subroutine.Predicate.isCharOrString;
import static org.jetbrains.emacs4ij.jelisp.subroutine.Predicate.isNil;

public abstract class Buffer {
  private Buffer() {}

  public static LispBuffer getBufferByBufferNameOrNil (Environment environment, @Optional LispObject bufferOrName) {
    if (isNil(bufferOrName))
      return environment.getBufferCurrentForEditing();
    if (bufferOrName instanceof LispString) {
      return environment.findBufferSafe(((LispString) bufferOrName).getData());
    }
    if (bufferOrName instanceof LispBuffer)
      return (LispBuffer) bufferOrName;
    throw new WrongTypeArgumentException("buffer-or-name", bufferOrName);
  }

  @Subroutine(value = "current-buffer")
  public static LispBuffer getCurrentBuffer(Environment environment) {
    return environment.getBufferCurrentForEditing();
  }

  @Subroutine("buffer-size")
  public static LispObject bufferSize(Environment environment, @Optional LispObject buffer) {
    if (isNil(buffer))
      buffer = environment.getBufferCurrentForEditing();
    if (!(buffer instanceof LispBuffer))
      throw new WrongTypeArgumentException("bufferp", buffer);
    return new LispInteger(((LispBuffer)buffer).size());
  }

  @Subroutine("buffer-name")
  public static LispObject bufferName (Environment environment, @Optional LispObject buffer) {
    if (isNil(buffer))
      buffer = environment.getBufferCurrentForEditing();
    if (!(buffer instanceof LispBuffer))
      throw new WrongTypeArgumentException("bufferp", buffer);
    return new LispString(((LispBuffer)buffer).getName());
  }

  @Subroutine("get-buffer")
  public static LispObject getBuffer (Environment environment, LispObject bufferOrName) {
    if (bufferOrName instanceof LispString) {
      LispBuffer buffer = environment.findBuffer(((LispString) bufferOrName).getData());
      if (buffer == null)
        return LispSymbol.NIL;
      return buffer;
    }
    if (bufferOrName instanceof LispBuffer) {
      return bufferOrName;
    }
    throw new WrongTypeArgumentException("stringp", bufferOrName);
  }

  public static LispBuffer getToolBufferCreate (Environment environment, String name, LispToolWindow window) {
    LispBuffer buffer = environment.findBuffer(name);
    if (buffer != null)
      return buffer;
    return environment.createToolBuffer(name, window);
  }

  public static LispBuffer getBufferCreate (Environment environment, String name) {
    LispBuffer buffer = environment.findBuffer(name);
    if (buffer != null)
      return buffer;
    return environment.createBuffer(name);
  }

  @Subroutine("get-buffer-create")
  public static LispObject getBufferCreate (Environment environment, LispObject bufferOrName) {
    if (bufferOrName instanceof LispBuffer)
      return bufferOrName;
    if (bufferOrName instanceof LispString)
      return getBufferCreate(environment, ((LispString) bufferOrName).getData());
    throw new WrongTypeArgumentException("stringp", bufferOrName);
  }

  @Subroutine("other-buffer")
  public static LispObject otherBuffer (Environment environment,
                                        @Optional @Nullable LispObject buffer, @Optional @Nullable LispObject visibleOk,
                                        @Optional @Nullable LispObject frame) {
    LispBuffer b = buffer == null || !(buffer instanceof LispBuffer)
        ? environment.getBufferCurrentForEditing()
        : (LispBuffer)buffer;
    boolean invisiblePreferred = Predicate.isNil(visibleOk);
    LispFrame f = Frame.getLiveFrame(environment, frame);

    return Core.thisOrNil(environment.getOtherBuffer(b, f, invisiblePreferred));
  }

  @Subroutine("set-buffer")
  public static LispObject setBuffer (Environment environment, LispObject bufferOrName) {
    LispObject lispObject = getBuffer(environment, bufferOrName);
    if (lispObject.equals(LispSymbol.NIL)) {
      throw new NoBufferException(bufferOrName.toString());
    }
    if (!environment.isMainOrGlobal()) {
      environment.setBufferCurrentForEditing((LispBuffer)lispObject);
    }
    return lispObject;
  }

  @Subroutine("point")
  public static LispInteger point (Environment environment) {
    return new LispInteger(environment.getBufferCurrentForEditing().point());
  }

  @Subroutine("point-min")
  public static LispObject pointMin (Environment environment) {
    return new LispInteger(environment.getBufferCurrentForEditing().pointMin());
  }

  @Subroutine("point-max")
  public static LispObject pointMax (Environment environment) {
    return new LispInteger(environment.getBufferCurrentForEditing().pointMax());
  }

  @Subroutine(value = "goto-char", isCmd = true, interactive = "nGoto char: ")
  public static LispObject gotoChar (Environment environment, MarkerOrInteger pos) {
    if (pos.getPosition() == null)
      throw new MarkerPointsNowhereException();
    environment.getBufferCurrentForEditing().gotoChar(pos.getPosition());
    return pos;
  }

  @Subroutine(value = "forward-char", isCmd = true, key = "\\C-f")
  public static LispObject forwardChar (Environment environment, @Optional LispObject shift) {
    if (isNil(shift))
      shift = new LispInteger(1);
    if (!(shift instanceof LispInteger))
      throw new WrongTypeArgumentException("integerp", shift.toString());
    String message = environment.getBufferCurrentForEditing().forwardChar(((LispInteger)shift).getData());
    if (message.equals(""))
      return LispSymbol.NIL;
    return new LispSymbol(message);
  }

  @Subroutine(value = "backward-char", isCmd = true, key = "\\C-b")
  public static LispObject backwardChar (Environment environment, @Optional LispObject shift) {
    if (isNil(shift))
      shift = new LispInteger(1);
    if (!(shift instanceof LispInteger))
      throw new WrongTypeArgumentException("integerp", shift.toString());
    String message = environment.getBufferCurrentForEditing().forwardChar(-((LispInteger)shift).getData());
    if (message.equals(""))
      return LispSymbol.NIL;
    return new LispSymbol(message);
  }

  @Subroutine("buffer-list")
  public static LispObject bufferList (Environment environment, @Optional LispObject frame) {
    return Predicate.isNil(frame) || !(frame instanceof LispFrame)
        ? environment.getBufferList()
        : environment.getBufferList((LispFrame) frame);
  }

  @Subroutine(value = "bury-buffer", isCmd = true)
  public static LispObject buryBuffer (Environment environment, @Optional LispObject bufferOrName) {
    LispBuffer buffer;
    if (isNil(bufferOrName)) {
      buffer = environment.getBufferCurrentForEditing();
    } else if (bufferOrName instanceof LispString) {
      buffer = environment.findBufferSafe(((LispString) bufferOrName).getData());
    } else if (bufferOrName instanceof LispBuffer) {
      buffer = (LispBuffer) bufferOrName;
    } else {
      throw new WrongTypeArgumentException("stringp", bufferOrName);
    }

    if (environment.getBufferCurrentForEditing().equals(buffer)) {
      Switch.switchToBuffer(environment, LispSymbol.NIL, null);
    }

    environment.buryBuffer(buffer);

    return LispSymbol.NIL;
  }
  @Subroutine("generate-new-buffer-name")
  public static LispString generateNewBufferName (Environment environment, LispString startingName, @Optional LispString ignore) {
    String result = startingName.getData();
    for (int n = 2; ; ++n) {
      if (!environment.containsBuffer(result))
        return new LispString(result);
      else if (ignore != null && result.equals(ignore.getData()))
        return ignore;
      result = startingName.getData() + '<' + n + '>';
    }
  }

  /**
   * just close all windows displaying given (or current if no given) buffer
   */
  @Subroutine(value = "replace-buffer-in-windows", isCmd = true, interactive = "bReplace buffer in windows")
  public static LispObject replaceBufferInWindows (Environment environment, @Optional LispObject bufferOrName) {
    if (bufferOrName == null)
      bufferOrName = environment.getBufferCurrentForEditing();

    LispBuffer buffer = getBufferByBufferNameOrNil(environment, bufferOrName);
    environment.hideBuffer(buffer);
    Switch.switchToBuffer(environment,
        environment.getOtherBuffer(buffer, environment.getSelectedFrame(), false),
        LispSymbol.NIL);

    return LispSymbol.NIL;
  }

  @Subroutine(value = "kill-buffer", isCmd = true, interactive = "bKill buffer", key = "\\C-xk")
  public static LispObject killBuffer (Environment environment, @Optional LispObject bufferOrName) {
    replaceBufferInWindows(environment, bufferOrName);
    LispSymbol killBufferQueryFunctions = environment.find("kill-buffer-query-functions");
    if (killBufferQueryFunctions != null) {
      LispObject functions = killBufferQueryFunctions.getValue();
      if (functions != null && functions != LispSymbol.VOID) {
        LispObject evaluationResult = functions.evaluate(environment);
        if (evaluationResult.equals(LispSymbol.NIL))
          return new LispString(JelispBundle.message("buffer.not.killed", bufferOrName.toString()));
      }
    }
    //todo: run hooks
    //todo: Any processes that have this buffer as the `process-buffer' are killed

    LispBuffer buffer = getBufferByBufferNameOrNil(environment, bufferOrName);
    //todo: check if modified. If user decides not to kill the buffer, return nil

    environment.setSelectionManagedBySubroutine(true);
    environment.killBuffer(buffer);
    return LispSymbol.T;
  }

  @Subroutine("minibuffer-depth")
  public static LispInteger minibufferDepth (Environment environment) {
    return new LispInteger(environment.getMiniBufferActivationsDepth());
  }

  private static LispObject evaluateString (Environment environment, String code) {
    return new ForwardParser().parseLine(code).evaluate(environment);
  }

  @Subroutine("insert")
  public static LispSymbol insert (Environment environment, @Optional LispObject... args) {
    StringBuilder toInsert = new StringBuilder();
    for (LispObject arg: args) {
      if (!isCharOrString(arg))
        throw new WrongTypeArgumentException("char-or-string-p", arg);
      if (arg instanceof LispInteger) {
        toInsert.append(((LispInteger) arg).toCharacterString());
        continue;
      }
      LispObject kbd = evaluateString(environment, "(kbd " + arg.toString() + ")");
      String converted = kbd instanceof LispString ? ((LispString) kbd).getData() : kbd.toString();
      toInsert.append(converted);
    }
    environment.getBufferCurrentForEditing().insert(toInsert.toString());
    return LispSymbol.NIL;
  }

  @Subroutine("set-buffer-major-mode")
  public static LispObject setBufferMajorMode (Environment environment, LispBuffer buffer) {
    //todo: if buffer == scratch => set value from "initial-major-mode". But I have no scratch :)
    LispSymbol mode = environment.find("major-mode");
    return Core.functionCall(environment, mode.getValue());
  }

  @Subroutine("following-char")
  public static LispInteger followingChar (Environment environment) {
    return new LispInteger(environment.getBufferCurrentForEditing().followingCharacter());
  }

  @Subroutine("preceding-char")
  public static LispInteger precedingChar (Environment environment) {
    return new LispInteger(environment.getBufferCurrentForEditing().precedingCharacter());
  }

  private static LispString bufferSubstring (Environment environment, MarkerOrInteger start, MarkerOrInteger end,
                                             boolean withProperties) {
    Integer begin = start.getPosition();
    Integer finish = end.getPosition();
    if (begin == null || finish == null)
      throw new MarkerPointsNowhereException();
    return environment.getBufferCurrentForEditing().substring(begin, finish, withProperties);
  }

  @Subroutine("buffer-substring")
  public static LispString bufferSubstring (Environment environment, MarkerOrInteger start, MarkerOrInteger end) {
    return bufferSubstring(environment, start, end, true);
  }

  @Subroutine("buffer-substring-no-properties")
  public static LispString bufferSubstringNoProperties (Environment environment, MarkerOrInteger start, MarkerOrInteger end) {
    return bufferSubstring(environment, start, end, false);
  }

  private static LispSymbol getExistingVar (Environment environment, LispSymbol variable) {
    LispSymbol existing = environment.find(variable.getName());
    return existing == null ? variable : existing;
  }

  @Subroutine(value = "make-variable-buffer-local", isCmd = true, interactive = "vMake Variable Buffer Local: ")
  public static LispObject makeVariableBufferLocal (Environment environment, LispSymbol variable) {
    LispSymbol existing = getExistingVar(environment, variable);
    GlobalEnvironment.INSTANCE.setVariableBufferLocal(existing);
    return existing.getSource();
  }

  @Subroutine(value = "make-local-variable", isCmd = true, interactive = "vMake Local Variable: ")
  public static LispObject makeVariableLocal (Environment environment, LispSymbol variable) {
    LispSymbol existing = getExistingVar(environment, variable);
    environment.getBufferCurrentForEditing().defineVariable(existing);
    return existing.getSource();
  }

  private static LispBuffer currentBufferOrGiven (Environment environment, @Nullable LispObject buffer) {
    if (Predicate.isNil(buffer))
      return environment.getBufferCurrentForEditing();
    if (!(buffer instanceof LispBuffer))
      throw new WrongTypeArgumentException("bufferp", buffer);
    return (LispBuffer) buffer;
  }

  @Subroutine("local-variable-p")
  public static LispSymbol isVariableLocal (Environment environment, LispSymbol variable, @Optional LispObject bufferObject) {
    LispBuffer buffer = currentBufferOrGiven(environment, bufferObject);
    return LispSymbol.bool(buffer.hasVariable(variable.getName()));
  }

  @Subroutine("local-variable-if-set-p")
  public static LispSymbol isVariableLocalIfSet (Environment environment, LispSymbol variable, @Optional LispObject bufferObject) {
    LispBuffer buffer = currentBufferOrGiven(environment, bufferObject);
    LispSymbol symbol = environment.find(variable.getName());
    return LispSymbol.bool(symbol != null && symbol.isBufferLocal() && !buffer.hasVariable(variable.getName()));
  }

  @Subroutine("buffer-local-value")
  public static LispObject bufferLocalValue (Environment environment, LispSymbol variable, LispBuffer buffer) {
    String name = variable.getName();
    if (buffer.hasVariable(name)) {
      return buffer.getVariableValue(name);
    }
    LispSymbol symbol = environment.find(name);
    if (symbol == null)
      throw new VoidVariableException(name);
    return symbol.getValue();
  }

  @Subroutine("buffer-local-variables")
  public static LispList bufferLocalVariables (Environment environment, @Optional LispObject bufferObject) {
    LispBuffer buffer = currentBufferOrGiven(environment, bufferObject);
    Map<LispSymbol, LispObject> map = buffer.getAllLocalVarValues();
    List<LispObject> alist = new ArrayList<>();
    for (Map.Entry<LispSymbol, LispObject> entry: map.entrySet()) {
      if (entry.getValue() == null) {
        alist.add(entry.getKey());
        continue;
      }
      alist.add(LispList.cons(entry.getKey(), entry.getValue()));
    }
    return LispList.list(alist);
  }

  @Subroutine(value = "kill-local-variable", isCmd = true, interactive = "vKill Local Variable: ")
  public static LispSymbol killLocalVariable (Environment environment, LispSymbol variable) {
    environment.getBufferCurrentForEditing().killVariable(variable.getName());
    return variable;
  }

  @Subroutine("kill-all-local-variables")
  public static LispSymbol killAllLocalVars (Environment environment) {
    Core.runHooks(environment, new LispSymbol("change-major-mode-hook"));
    environment.getBufferCurrentForEditing().reset();
    return LispSymbol.NIL;
  }

  @Subroutine("set-buffer-modified-p")
  public static LispObject setBufferModified (Environment environment, LispObject flag) {
    environment.getBufferCurrentForEditing().setModified(flag);
    return flag;
  }

  @Subroutine("buffer-modified-p")
  public static LispSymbol isBufferModified(Environment environment, @Optional LispObject bufferObject) {
    if (Predicate.isNil(bufferObject))
      bufferObject = environment.getBufferCurrentForEditing();
    if (!(bufferObject instanceof LispBuffer))
      throw new WrongTypeArgumentException("bufferp", bufferObject);
    return LispSymbol.bool(((LispBuffer) bufferObject).isModified());
  }

  @Subroutine("restore-buffer-modified-p")
  public static LispObject restoreBufferModified (Environment environment, LispObject flag) {
    environment.getBufferCurrentForEditing().restoreModified(flag);
    return flag;
  }

  @Subroutine(value = "widen", isCmd = true, key = "\\C-xnw")
  public static LispSymbol widen (Environment environment) {
    //todo: remove narrowing from current buffer
    return LispSymbol.NIL;
  }

  @Subroutine("buffer-file-name")
  public static LispObject bufferFileName (Environment environment, @Optional LispObject bufferObject) {
    if (Predicate.isNil(bufferObject))
      bufferObject = environment.getBufferCurrentForEditing();
    if (!(bufferObject instanceof LispBuffer))
      throw new WrongTypeArgumentException("bufferp", bufferObject);
    return ((LispBuffer)bufferObject).getVariableValue("buffer-file-name");
  }
}
